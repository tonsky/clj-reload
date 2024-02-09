(ns clj-reload.core
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File PushbackReader]))

(def ^:dynamic *log-fn*
  println)

(defn log [& args]
  (when *log-fn*
    (apply *log-fn* args)))

; {:dirs        [<string> ...]   - where to look for files
;  :no-unload   #{<symbol> ...}  - list of nses to skip unload
;  :no-reload   #{<symbol> ...}  - list of nses to skip reload
;  :reload-hook <symbol>         - if function with this name exists in ns,
;                                  it will be called after reloading.
;                                  default: after-ns-reload
;  :unload-hook <symbol>         - if function with this name exists in ns,
;                                  it will be called before unloading.
;                                  default: before-ns-unload
;  :since       <long>           - last time list of files was scanned
;  :namespaces  {<symbol> -> NS} - all found namespaces
;  :to-unload   #{<symbol> ...}  - list of nses pending unload
;  :to-load     #{<symbol> ...}  - list of nses pending load
; }
;
;  NS :: {:file     <file>
;         :modified <modified>
;         :depends  <depends>
;         :keep     {<symbol> -> <value>}}}

(def *state
  (atom {}))

(def reader-opts
  {:read-cond :allow
   :features  #{:clj}
   :eof       ::eof})

(defn update! [m k f & args]
  (assoc! m k (apply f (m k) args)))

(def conjs
  (fnil conj #{}))

(def intos
  (fnil into #{}))

(defn some-map [& args]
  (persistent!
    (reduce
      (fn [m [k v]]
        (cond-> m (some? v) (assoc! k v)))
      (transient {}) (partition 2 args))))

(defn now []
  (System/currentTimeMillis))

(defn last-modified [^File f]
  (some-> f .lastModified))

(defn file? [^File f]
  (some-> f .isFile))

(defn file-name [^File f]
  (some-> f .getName))

(defn reader ^PushbackReader [f]
  (PushbackReader. (io/reader (io/file f))))

(defn- expand-quotes [form]
  (walk/postwalk
    #(if (and (sequential? %) (not (vector? %)) (= 'quote (first %)))
       (second %)
       %)
    form))

(defn parse-require-form [form]
  (loop [body   (next form)
         result (transient #{})]
    (let [[decl & body'] body]
      (cond
        (empty? body)
        (persistent! result)
        
        (symbol? decl) ;; a.b.c
        (recur body' (conj! result decl))
        
        (not (sequential? decl))
        (do
          (log "Unexpected" (first form) "form:" (pr-str decl))
          (recur body' result))
        
        (not (symbol? (first decl)))
        (do
          (log "Unexpected" (first form) "form:" (pr-str decl))
          (recur body' result))
        
        (or
          (nil? (second decl))      ;; [a.b.d]
          (keyword? (second decl))) ;; [a.b.e :as e]
        (recur body' (conj! result (first decl)))
        
        :else ;; [a.b f [g :as g]]
        (let [prefix  (first decl)
              symbols (->> (next decl)
                        (map #(if (symbol? %) % (first %)))
                        (map #(symbol (str (name prefix) "." (name %)))))]
          (recur body' (reduce conj! result symbols)))))))

(defn parse-ns-form [form]
  (let [name (second form)]
    (loop [body    (nnext form)
           depends (transient #{})]
      (let [[form & body'] body
            tag  (when (list? form)
                   (first form))]
        (cond
          (empty? body)
          [name (persistent! depends)]
          
          (#{:require :use} tag)
          (recur body' (reduce conj! depends (parse-require-form form)))
          
          :else
          (recur body' depends))))))

(defn read-file
  "Returns [<symbol> Namespace] or Exception"
  ([file]
   (with-open [rdr (reader file)]
     (try
       (read-file rdr file)
       (catch Exception e
         (log "Failed to read" (.getPath ^File file) (.getMessage e))
         e))))
  ([rdr file]
   (loop [name    nil
          depends (transient #{})
          keep    (transient {})]
     (let [form (binding [*read-eval* false]
                  (read reader-opts rdr))
           tag  (when (list? form)
                  (first form))]
       (cond
         (= ::eof form)
         (when name
           [name (some-map
                   :depends  (persistent! depends)
                   :keep     (persistent! keep)
                   :file     file
                   :modified (last-modified file))])
        
         (and name (#{'ns 'in-ns} tag))
         (throw (ex-info (str "Second namespace definition after " name ": " form " in " file)
                  {:name name :form form}))
        
         (= 'ns tag)
         (let [[name depends] (parse-ns-form form)]
           (recur name (transient depends) keep))
          
         (= 'in-ns tag)
         (let [[_ name] (expand-quotes form)]
           (recur name depends keep))
        
         (and (nil? name) (#{'require 'use} tag))
         (throw (ex-info (str "Unexpected " tag " before ns definition in " file) {:form form}))
        
         (#{'require 'use} tag)
         (let [depends' (parse-require-form (expand-quotes form))]
           (recur name (reduce conj! depends depends') keep))
        
         (= 'defonce tag)
         (recur name depends (assoc! keep (second form) nil))
        
         :else
         (recur name depends keep))))))

(defn dependees 
  "ns -> #{downstream-ns ...}"
  [namespaces]
  (let [*m (volatile! (transient {}))]
    (doseq [[from {tos :depends}] namespaces]
      (vswap! *m update! from #(or % #{}))
      (doseq [to tos]
        (vswap! *m update! to conjs from)))
    (persistent! @*m)))

(defn transitive-closure
  "Starts from starts, expands using dependees {ns -> #{downsteram-ns ...}},
   returns #{ns ...}"
  [deps starts]
  (loop [queue starts
         acc   (transient #{})]
    (let [[start & queue'] queue]
      (cond
        (empty? queue)
        (persistent! acc)
      
        (contains? acc start)
        (recur queue' acc)
        
        :else
        (recur (into queue (deps start)) (conj! acc start))))))

(declare topo-sort)

(defn report-cycle [deps all-deps]
  (let [circular (filterv
                   (fn [node]
                     (try
                       (topo-sort (dissoc deps node) (fn [_ _] (throw (ex-info "Part of cycle" {}))))
                       true
                       (catch Exception e
                         false)))
                   (keys deps))]
    (throw (ex-info (str "Cycle detected: " (str/join ", " (sort circular))) {:nodes circular :deps all-deps}))))

(defn topo-sort
  ([deps]
   (topo-sort deps report-cycle))
  ([all-deps on-cycle]
   (loop [res  (transient [])
          deps all-deps]
     (if (empty? deps)
       (persistent! res)
       (let [root (fn [node]
                    (when (every? #(not (% node)) (vals deps))
                      node))
             node (->> (keys deps) (filter root) (sort) (first))]
         (if node
           (recur (conj! res node) (dissoc deps node))
           (on-cycle deps all-deps)))))))

(defn topo-sort-fn
  "Accepts dependees map {ns -> #{downsteram-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (topo-sort deps)]
    (fn [coll]
      (filter (set coll) sorted))))

(defn changed
  "Returns {:updated {<symbol> -> Namespace}
            :deleted #{<symbol> ...}
            :broken  {<symbol> -> Throwable}}"
  [before dirs since]
  (let [files-before     (into {}
                           (for [[ns {file :file}] before]
                             [file ns]))
        
        files-new        (->> dirs
                           (mapcat #(file-seq (io/file %)))
                           (filter file?)
                           (filter #(re-matches #".*\.cljc?" (file-name %)))
                           (set))
        
        files-modified   (into {}
                           (for [file  files-new
                                 :when (> (last-modified file) since)]
                             [file (read-file file)]))
        
        updated          (into {}
                           (for [[file res] files-modified
                                 :when (vector? res)]
                             res))
        
        deleted?         #(not (contains? files-new %))
        namespaces-new   (merge
                           (into {}
                             (for [entry before
                                   :let  [[ns {file :file}] entry]
                                   :when (not (contains? files-modified file))
                                   :when (not (deleted? file))]
                               entry))
                           updated)
        deleted          (set (remove namespaces-new (keys before)))
        
        known-broken     (into {}
                           (for [[file ex] files-modified
                                 :when (instance? Throwable ex)
                                 :let  [ns (files-before file)]
                                 :when ns]
                             [ns ex]))]
    {:updated updated
     :deleted deleted
     :broken  known-broken}))

(defn init-impl [opts]
  (let [dirs (vec (:dirs opts))
        now  (now)
        nses (:updated (changed nil dirs 0))]
    {:dirs        dirs
     :no-unload   (set (:no-unload opts))
     :no-reload   (set (:no-reload opts))
     :reload-hook (:reload-hook opts 'after-ns-reload)
     :unload-hook (:unload-hook opts 'before-ns-unload)
     :namespaces  nses
     :since       now}))

(defn init [opts]
  (binding [*log-fn* nil]
    (reset! *state (init-impl opts))))

(defn scan-impl [state opts]
  (let [{:keys [dirs since to-load to-unload no-unload no-reload namespaces]} state
        {:keys [only] :or {only :changed}} opts
        now              (now)
        loaded           (case only
                           :changed @@#'clojure.core/*loaded-libs*
                           :loaded  @@#'clojure.core/*loaded-libs*
                           :all     (constantly true))
        {:keys [updated
                deleted
                broken]} (case only
                           :changed (changed namespaces dirs since)
                           :loaded  (changed namespaces dirs 0)
                           :all     (changed namespaces dirs 0))
        _                (doseq [[ns ex] broken
                                 :when (loaded ns)
                                 :when (not (no-reload ns))]
                           (throw ex))
        since'           (->> updated
                           vals
                           (keep :modified)
                           (reduce max now))
        
        unload?          #(and
                            (loaded %)
                            (not (no-unload %))
                            (not (no-reload %)))
        deps             (dependees namespaces)
        topo-sort        (topo-sort-fn deps) 
        to-unload'       (->> (concat (keys updated) deleted)
                           (filter unload?)
                           (transitive-closure deps)
                           (filter unload?)
                           (concat to-unload)
                           (topo-sort)
                           (reverse))
        
        namespaces'      (as-> namespaces %
                           (reduce dissoc % deleted)
                           (merge % updated))
        load?            #(and
                            (loaded %)
                            (not (no-reload %))
                            (namespaces' %))
        deps'            (dependees namespaces')
        topo-sort'       (topo-sort-fn deps')
        to-load'         (->> (keys updated)
                           (filter load?)
                           (transitive-closure deps')
                           (filter load?)
                           (concat to-load)
                           (topo-sort'))]
    (assoc state
      :since      since'
      :to-unload  to-unload'
      :to-load    to-load'
      :namespaces namespaces')))

(defn ns-unload [ns unload-hook]
  (log "Unloading" ns)
  (try
    (when unload-hook
      (when-some [ns-obj (find-ns ns)]
        (when-some [unload-fn (ns-resolve ns-obj unload-hook)]
          (unload-fn))))
    (catch Throwable t
      ;; eat up unload error
      ;; if we can’t unload there’s no way to fix that
      ;; because any changes would require reload first
      (log "  exception during unload hook" t)))
  (remove-ns ns)
  (dosync
    (alter @#'clojure.core/*loaded-libs* disj ns)))

(defn ns-load [ns reload-hook]
  (log "Loading" ns)
  (try
    (require ns :reload) ;; use load to load?
    
    (when reload-hook
      (when-some [reload-fn (ns-resolve (find-ns ns) reload-hook)]
        (reload-fn)))
    
    nil
    (catch Throwable t
      (log "  failed to load" ns t)
      t)))

(defn reload
  "Options:
   
   :throw  :: true | false  - throw or return exception, default true
   :log-fn :: (fn [& args]) - fn to display unload/reload status
   :only   :: :changed      - default. Only reloads changed already loaded files
            | :loaded       - Reload all loaded files
            | :all          - Reload everything it can find in dirs"
  ([]
   (reload nil))
  ([opts]
   (binding [*log-fn* (:log-fn opts println)]
     (swap! *state scan-impl opts)
     ; (clojure.pprint/pprint (:namespaces @*state))
     ; (println "To unload:" (:to-unload @*state))
     ; (println "To load:" (:to-load @*state))
     (loop [unloaded []
            loaded   []
            state    @*state]
       (cond
         (not-empty (:to-unload state))
         (let [[ns & to-unload'] (:to-unload state)
               _                 (ns-unload ns (:unload-hook state))
               state'            (swap! *state assoc :to-unload to-unload')]
           (recur (conj unloaded ns) loaded state'))
        
         (not-empty (:to-load state))
         (let [[ns & to-load'] (:to-load state)]
           (if-some [ex (ns-load ns (:reload-hook state))]
             (if (:throw opts true)
               (throw
                 (ex-info
                   (str "Failed to load namespace: " ns)
                   {:unloaded  unloaded
                    :loaded    loaded
                    :failed    ns}
                   ex))
               {:unloaded  unloaded
                :loaded    loaded
                :failed    ns
                :exception ex})
             (let [state' (swap! *state assoc :to-load to-load')]
               (recur unloaded (conj loaded ns) state'))))
         
         :else
         {:unloaded unloaded
          :loaded   loaded})))))
