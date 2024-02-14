(ns clj-reload.core
  (:require
    [clojure.java.io :as io]
    [clojure.spec.alpha :as spec]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File PushbackReader StringReader]))

(def ^:dynamic *log-fn*
  println)

(defn log [& args]
  (when *log-fn*
    (apply *log-fn* args)))

; {// config
;  :dirs        [<string> ...]   - where to look for files
;  :no-unload   #{<symbol> ...}  - list of nses to skip unload
;  :no-reload   #{<symbol> ...}  - list of nses to skip reload
;  :reload-hook <symbol>         - if function with this name exists in ns,
;                                  it will be called after reloading.
;                                  default: after-ns-reload
;  :unload-hook <symbol>         - if function with this name exists in ns,
;                                  it will be called before unloading.
;                                  default: before-ns-unload
;  // state
;  :since       <long>           - last time list of files was scanned
;  :files       {<file> -> File} - all found files
;  :namespaces  {<symbol> -> NS} - all found namespaces
;  :to-unload   #{<symbol> ...}  - list of nses pending unload
;  :to-load     #{<symbol> ...}  - list of nses pending load
; }
;
; File :: {:namespaces #{<symbol> ...}
;          :modified   <modified>}
;
; NS   :: {:main-file <file>
;          :files     #{<file> ...}
;          :requires  #{<symbol> ...}
;          :keep      {<symbol> -> Keep}}}
;
; Keep :: {:tag   <symbol>
;          :value <any?>}

(def *state
  (atom {}))

(def reader-opts
  {:read-cond :allow
   :features  #{:clj}
   :eof       ::eof})

(defn throwable? [o]
  (instance? Throwable o))

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

(defn assoc-some [m & kvs]
  (reduce
    (fn [m [k v]]
      (cond-> m
        (some? v) (assoc k v)))
    m
    (partition 2 kvs)))

(defmacro for-map [& body]
  `(into {}
     (for ~@body)))

(defmacro for-set [& body]
  `(into #{}
     (for ~@body)))

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
    (loop [body     (nnext form)
           requires (transient #{})]
      (let [[form & body'] body
            tag (when (list? form)
                  (first form))]
        (cond
          (empty? body)
          [name (not-empty (persistent! requires))]
          
          (#{:require :use} tag)
          (recur body' (reduce conj! requires (parse-require-form form)))
          
          :else
          (recur body' requires))))))

(defn read-file
  "Returns {<symbol> NS} or Exception"
  ([file]
   (with-open [rdr (reader file)]
     (try
       (read-file rdr file)
       (catch Exception e
         (log "Failed to read" (.getPath ^File file) (.getMessage e))
         (ex-info (str "Failed to read" (.getPath ^File file)) {:file file} e)))))
  ([rdr file]
   (loop [ns   nil
          nses {}]
     (let [form (binding [*read-eval* false]
                  (read reader-opts rdr))
           tag  (when (list? form)
                  (first form))]
       (cond
         (= ::eof form)
         nses
         
         (= 'ns tag)
         (let [[ns requires] (parse-ns-form form)]
           (recur ns (update nses ns assoc-some :requires requires :main-file file)))
          
         (= 'in-ns tag)
         (let [[_ ns] (expand-quotes form)]
           (recur ns (assoc nses ns nil)))
        
         (and (nil? ns) (#{'require 'use} tag))
         (throw (ex-info (str "Unexpected " tag " before ns definition in " file) {:form form}))
        
         (#{'require 'use} tag)
         (let [requires' (parse-require-form (expand-quotes form))]
           (recur ns (update-in nses [ns :requires] intos requires')))
        
         (or
           (= 'defonce tag)
           (::keep (meta form)))
         (let [[_ name] form]
           (recur ns (assoc-in nses [ns :keep name] {:tag  tag
                                                     :form form})))
        
         :else
         (recur ns nses))))))

(defn dependees 
  "Inverts the requies graph. Returns {ns -> #{downstream-ns ...}}"
  [namespaces]
  (let [*m (volatile! (transient {}))]
    (doseq [[from {tos :requires}] namespaces]
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

(defn files->namespaces [files already-read]
  (let [*res (volatile! {})]
    (doseq [file files
            [name namespace] (or
                               (already-read file)
                               (read-file file))
            :when (not (throwable? namespace))]
      (vswap! *res update name #(merge-with into % namespace {:files #{file}})))
    @*res))

(defn scan-impl [files-before dirs since]
  (let [files-now        (->> dirs
                           (mapcat #(file-seq (io/file %)))
                           (filter file?)
                           (filter #(re-matches #".*\.cljc?" (file-name %))))

        [files-modified
         files-broken]   (reduce
                           (fn [[modified broken] file]
                             (if (<= (last-modified file) since)
                               [modified broken]
                               (let [res (read-file file)]
                                 (if (throwable? res)
                                   [modified (assoc broken file res)]
                                   [(assoc modified file res) broken]))))
                           [{} {}] files-now)

        files-deleted    (reduce disj (set (keys files-before)) files-now)
        
        nses-broken      (for-map [[file ex] files-broken
                                   ns (get-in files-before [file :namespaces])]
                           [ns ex])
        
        nses-unload      (reduce
                           #(into %1 (get-in files-before [%2 :namespaces]))
                           #{}
                           (concat (keys files-modified) files-deleted))
        
        nses-load        (for-set [[file namespaces] files-modified
                                   ns (keys namespaces)]
                           ns)
        
        files'           (as-> files-before %
                           (reduce dissoc % files-deleted)
                           (merge %
                             (for-map [[file namespaces] files-modified]
                               [file {:namespaces (set (keys namespaces))
                                      :modified   (last-modified file)}])))
        
        already-read     (merge
                           files-modified
                           (for-map [[file _] files-broken]
                             [file {}]))
        
        nses'            (files->namespaces (keys files') already-read)] ;; TODO don't parse second time
                           
    {:broken      nses-broken
     :files'      files'
     :namespaces' nses'
     :to-unload'  nses-unload
     :to-load'    nses-load}))

(defn init-impl [opts]
  (let [dirs (vec (:dirs opts))
        now  (now)
        {:keys [files' namespaces']} (scan-impl nil dirs 0)]
    {:dirs        dirs
     :no-unload   (set (:no-unload opts))
     :no-reload   (set (:no-reload opts))
     :reload-hook (:reload-hook opts 'after-ns-reload)
     :unload-hook (:unload-hook opts 'before-ns-unload)
     :since       now
     :files       files'
     :namespaces  namespaces'}))

(defn init [opts]
  (binding [*log-fn* nil]
    (reset! *state (init-impl opts))))

(defn scan [state opts]
  (let [{:keys [dirs since to-load to-unload no-unload no-reload files namespaces]} state
        {:keys [only] :or {only :changed}} opts
        now              (now)
        loaded           (case only
                           :changed @@#'clojure.core/*loaded-libs*
                           :loaded  @@#'clojure.core/*loaded-libs*
                           :all     (constantly true))
        {:keys [broken
                files'
                namespaces'
                to-unload'
                to-load']} (case only
                             :changed (scan-impl files dirs since)
                             :loaded  (scan-impl files dirs 0)
                             :all     (scan-impl files dirs 0))
        
        _                (doseq [[ns {:keys [file exception]}] broken
                                 :when (loaded ns)
                                 :when (not (no-reload ns))]
                           (throw exception))
        
        since'           (transduce (map :modified) max (max since now) (vals files'))
        
        unload?          #(and
                            (loaded %)
                            (not (no-unload %))
                            (not (no-reload %)))
        deps             (dependees namespaces)
        topo-sort        (topo-sort-fn deps) 
        to-unload''      (->> to-unload'
                           (filter unload?)
                           (transitive-closure deps)
                           (filter unload?)
                           (concat to-unload)
                           (topo-sort)
                           (reverse))
        
        load?            #(and
                            (loaded %)
                            (not (no-reload %))
                            (namespaces' %))
        deps'            (dependees namespaces')
        topo-sort'       (topo-sort-fn deps')
        to-load''        (->> to-load'
                           (filter load?)
                           (transitive-closure deps')
                           (filter load?)
                           (concat to-load)
                           (topo-sort'))]
    (assoc state
      :since      since'
      :files      files'
      :namespaces namespaces'
      :to-unload  to-unload''
      :to-load    to-load'')))

(defmulti keep-methods (fn [tag] tag))

(defmethod keep-methods :default [_]
  {:resolve 
   (fn [ns sym keep]
     (when-some [var (resolve (symbol (name ns) (name sym)))]
       {:value @var}))
   
   :embed
   (fn [ns sym keep]
     (list 'def sym (:value keep)))
   
   :patch
   (fn [ns sym keep]
     (str "(def " sym " clj-reload.keep/" sym ")"))})

(defmethod keep-methods 'deftype [_]
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       {:ctor @ctor}))
   
   :embed
   (fn [ns sym keep]
     (list 'def (symbol (str "->" sym)) (:ctor keep)))
   
   :patch
   (fn [ns sym keep]
     (str
       "(clojure.core/import " ns "." sym ")"
       "(def ->" sym " clj-reload.keep/->" sym ")"))})

(defmethod keep-methods 'defrecord [_]
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       (when-some [map-ctor (resolve (symbol (name ns) (str "map->" sym)))]
         {:ctor @ctor
          :map-ctor @map-ctor})))
   
   :embed
   (fn [ns sym keep]
     (list 'do
       (list 'def (symbol (str "->" sym)) (:ctor keep))
       (list 'def (symbol (str "map->" sym)) (:map-ctor keep))))
   
   :patch
   (fn [ns sym keep]
     (str
       "(clojure.core/import " ns "." sym ")"
       "(def ->" sym " clj-reload.keep/->" sym ")"
       "(def map->" sym " clj-reload.keep/map->" sym ")"))})

(defmethod keep-methods 'defprotocol [_]
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       (when-some [map-ctor (resolve (symbol (name ns) (str "map->" sym)))]
         {:ctor @ctor
          :map-ctor @map-ctor})))
   
   :embed
   (fn [ns sym keep]
     (list 'do
       (list 'def (symbol (str "->" sym)) (:ctor keep))
       (list 'def (symbol (str "map->" sym)) (:map-ctor keep))))
   
   :patch
   (fn [ns sym keep]
     (str
       "(clojure.core/import " ns "." sym ")"
       "(def ->" sym " clj-reload.keep/->" sym ")"
       "(def map->" sym " clj-reload.keep/map->" sym ")"))})

(defn keep-resolve [ns sym keep]
  ((:resolve (keep-methods (:tag keep))) ns sym keep))

(defn keep-embed [ns sym keep]
  ((:embed (keep-methods (:tag keep))) ns sym keep))

(defn keep-patch [ns sym keep]
  ((:patch (keep-methods (:tag keep))) ns sym keep))

(defn resolve-keeps [ns syms]
  (for-map [[sym keep] syms
            :let  [resolved (keep-resolve ns sym keep)]
            :when resolved]
    [sym (merge keep resolved)]))

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

(defn patch-file [content patches]
  (let [rdr     (clojure.lang.LineNumberingPushbackReader.
                  (StringReader. content))
        patched (StringBuilder.)]
    (loop []
      (.captureString rdr)
      (let [form (binding [*read-eval* false]
                   (read reader-opts rdr))
            text (.getString rdr)]
        (cond
          (= ::eof form)
          (str patched)
          
          (and (list? form) (contains? patches (second form)))
          (let [[_ sym] form
                [_ ws]  (re-matches #"(?s)(\s*).*" text)
                _       (.append patched ws)
                text    (subs text (count ws))
                lines   (str/split-lines text)
                text'   (patches sym)
                text''  (if (= 1 (count lines))
                          (if (<= (count text) (count text'))
                            text'
                            (str text' (str/join (repeat (- (count text) (count text')) \space))))
                          (str text'
                            (str/join (repeat (dec (count lines)) \newline))
                            (str/join (repeat (count (last lines)) \space))))]
            (.append patched text'')
            (recur))
          
          :else
          (do
            (.append patched text)
            (recur)))))))

(defn ns-load-patched [ns ^File file keeps]
  (try
    ;; stash keeps in clj-reload.keep
    (binding [*ns* *ns*]
      (in-ns 'clj-reload.keep)
      (clojure.core/use 'clojure.core)
      (eval (cons 'do
              (map (fn [[sym keep]]
                     (keep-embed ns sym keep)) keeps))))
    
    ;; replace keeps in file with refs to clj-reload.keep
    (let [patch    (for-map [[sym keep] keeps]
                     [sym (keep-patch ns sym keep)])
          contents (patch-file (slurp file) patch)]
      ;; load modified file
      (Compiler/load (StringReader. contents) (.getPath file) (.getName file)))
    
    ;; check 
    (@#'clojure.core/throw-if (not (find-ns ns))
      "namespace '%s' not found after loading '%s'"
      ns (.getPath file))
      
    (finally
      ;; drop everything in stash
      (remove-ns 'clj-reload.keep)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj 'clj-reload.keep)))))

(defn ns-load [ns ^File file keeps reload-hook]
  (log "Loading" ns "from" (.getPath file))
  (try
    (if (empty? keeps)
      (require ns :reload)
      (ns-load-patched ns file keeps))
    
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
     (swap! *state scan opts)
     (loop [unloaded []
            loaded   []
            state    @*state]
       (cond
         (not-empty (:to-unload state))
         (let [[ns & to-unload'] (:to-unload state)
               keeps           (resolve-keeps ns (-> state :namespaces ns :keep))
               _                 (ns-unload ns (:unload-hook state))
               state'            (swap! *state #(-> % 
                                                  (assoc :to-unload to-unload')
                                                  (update :namespaces update ns assoc :keep keeps)))]
           (recur (conj unloaded ns) loaded state'))
        
         (not-empty (:to-load state))
         (let [[ns & to-load'] (:to-load state)
               file (-> state :namespaces ns :main-file)]
           (if-some [ex (ns-load ns file (-> state :namespaces ns :keep) (:reload-hook state))]
             (do
               (swap! *state update :to-unload #(cons ns %))
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
                  :exception ex}))
             (let [state' (swap! *state assoc :to-load to-load')]
               (recur unloaded (conj loaded ns) state'))))
         
         :else
         {:unloaded unloaded
          :loaded   loaded})))))
