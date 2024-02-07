(ns clj-reload.core
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File PushbackReader]))

(def ^:dynamic *log-fn*
  println)

(def ^:dynamic *stable?*
  false)

; {:dirs        [<string> ...]    - where to look for files
;  :no-unload   #{<symbol> ...}   - list of namespaces to skip unload
;  :no-reload   #{<symbol> ...}   - list of namespaces to skip reload
;  :unload-hook <symbol>          - if function with this name exists in ns,
;                                   it will be called before unloading.
;                                   default: on-ns-unload
;  :since       <long>            - last time list of files was scanned
;  :files       {<file> -> File}  - files to namespace
;  :unload      #{<symbol> ...}   - list of namespaces pending unload
;  :load        #{<symbol> ...}}  - list of namespaces pending load
;
;  File :: {:modified   <long>
;           :namespaces {<symbol> -> Namespace}}
;
;  Namespace :: {:depends #{<symbol> ...}}

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
         result #{}]
    (let [[decl & body'] body]
      (cond
        (empty? body)
        result
        
        (symbol? decl) ;; a.b.c
        (recur body' (conj result decl))
        
        (not (sequential? decl))
        (do
          (println "Unexpected" (first form) "form:" (pr-str decl))
          (recur body' result))
        
        (not (symbol? (first decl)))
        (do
          (println "Unexpected" (first form) "form:" (pr-str decl))
          (recur body' result))
        
        (or
          (nil? (second decl))      ;; [a.b.d]
          (keyword? (second decl))) ;; [a.b.e :as e]
        (recur body' (conj result (first decl)))
        
        :else ;; [a.b f [g :as g]]
        (let [prefix   (first decl)
              suffixes (map #(if (symbol? %) % (first %)) (next decl))]
          (recur body' (into result (map #(symbol (str (name prefix) "." (name %))) suffixes))))))))

(defn parse-ns-form [form]
  (let [name (second form)
        body (loop [body   (nnext form)
                    result {}]
               (let [[form & body'] body
                     tag  (when (list? form)
                            (first form))]
                 (cond
                   (empty? body)
                   result
          
                   (#{:require :use} tag)
                   (recur body' (update result :depends intos (parse-require-form form)))
          
                   :else
                   (recur body' result))))]
    [name body]))

(defn read-file
  "Returns {<symbol> -> Namespace}"
  [rdr]
  (loop [current-ns nil
         result     {}]
    (let [form (binding [*read-eval* false]
                 (read reader-opts rdr))
          tag  (when (list? form)
                 (first form))]
      (cond
        (= ::eof form)
        result
          
        (= 'ns tag)
        (let [[name body] (parse-ns-form form)]
          (recur name (assoc result name body)))
          
        (= 'in-ns tag)
        (let [[_ name] (expand-quotes form)]
          (recur name result))
          
        (#{'require 'use} tag)
        (let [_    (assert current-ns (str "Unexpected " tag " form outside of ns"))
              deps (parse-require-form (expand-quotes form))]
          (recur current-ns (update result current-ns update :depends intos deps)))
            
        :else
        (recur current-ns result)))))

(defn dependees 
  "ns -> #{downstream-ns ...}"
  [files]
  (let [*m (volatile! (transient {}))]
    (doseq [[_ {nses :namespaces}] files
            [from {tos :depends}] nses]
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

(defn report-cycle [deps]
  (let [circular (filterv
                   (fn [node]
                     (try
                       (topo-sort (dissoc deps node) (fn [_] (throw (ex-info "Part of cycle" {}))))
                       true
                       (catch Exception e
                         false)))
                   (keys deps))]
    (throw (ex-info (str "Cycle detected: " (str/join ", " (sort circular))) {:nodes circular}))))

(defn topo-sort
  ([deps]
   (topo-sort deps report-cycle))
  ([deps on-cycle]
   (loop [res  (transient [])
          deps deps]
     (if (empty? deps)
       (persistent! res)
       (let [root (fn [node]
                    (when (every? #(not (% node)) (vals deps))
                      node))
             node (if *stable?*
                    (->> (keys deps) (filter root) (sort) (first))
                    (->> (keys deps) (some root)))]
         (if node
           (recur (conj! res node) (dissoc deps node))
           (on-cycle deps)))))))

(defn topo-sort-fn
  "Accepts dependees map {ns -> #{downsteram-ns ...}},
   returns a fn that topologically sorts dependencies"
  [deps]
  (let [sorted (topo-sort deps)]
    (fn [coll]
      (filter (set coll) sorted))))

(defn changed-files
  "Returns {<file> -> File}"
  [before dirs since]
  (let [all     (->> dirs
                  (mapcat #(file-seq (io/file %)))
                  (filter file?)
                  (filter #(re-matches #".*\.cljc?" (file-name %))))
        updated (filter #(> (last-modified %) (or since 0)) all)
        deleted (remove (set all) (keys before))]
    (into {}
      (concat
        (for [file updated]
          [file {:modified (last-modified file)
                 :namespaces
                 (with-open [rdr (reader file)]
                   (try
                     (read-file rdr)
                     (catch Exception e
                       #_(printf "Failed to load %s: %s\n" (.getPath ^File file) (.getMessage e)))))}])
        (for [file deleted]
          [file {:modified (now)}])))))

(defn init-impl [opts]
  (let [dirs  (vec (:dirs opts))
        now   (now)
        files (changed-files nil dirs 0)]
    {:dirs        dirs
     :no-unload   (set (:no-unload opts))
     :no-reload   (set (:no-reload opts))
     :unload-hook (:unload-hook opts 'on-ns-unload)
     :files       files
     :since       now}))

(defn init [opts]
  (reset! *state (init-impl opts)))

(defn scan-impl [state]
  (let [{:keys [dirs since to-load to-unload no-unload no-reload files]} state
        now           (now)
        changed-files (changed-files files dirs since)
        since'        (->> changed-files
                        vals
                        (map :modified)
                        (reduce max now))
        loaded        @@#'clojure.core/*loaded-libs*
        
        changed-nses  (for [[file _] changed-files
                            :let [{nses :namespaces} (get files file)]
                            [ns _] nses]
                        ns)
        unload?       #(and
                         (loaded %)
                         (not (no-unload %))
                         (not (no-reload %)))
        dependees     (dependees files)
        topo-sort     (topo-sort-fn dependees) 
        to-unload'    (->> changed-nses
                        (filter unload?)
                        (transitive-closure dependees)
                        (filter unload?)
                        (concat to-unload)
                        (topo-sort)
                        (reverse))
        
        load?         #(and
                         (loaded %)
                         (not (no-reload %)))
        files'        (merge files changed-files)
        changed-nses  (for [[_ {nses :namespaces}] changed-files
                            [ns _] nses]
                        ns)
        dependees'    (clj-reload.core/dependees files')
        topo-sort'    (topo-sort-fn dependees')
        to-load'      (->> changed-nses
                        (filter load?)
                        (transitive-closure dependees')
                        (filter load?)
                        (concat to-load)
                        (topo-sort'))]
    (assoc state
      :since     since'
      :to-unload to-unload'
      :to-load   to-load'
      :files     files')))

(defn ns-unload [ns unload-hook]
  (try
    (when unload-hook
      (when-some [ns-obj (find-ns ns)]
        (when-some [unload-fn (ns-resolve ns-obj unload-hook)]
          (unload-fn))))
    (catch Throwable t
      ;; eat up unload error
      ;; if we can’t unload there’s no way to fix that
      ;; because any changes would require reload first
      (println t)))
  (remove-ns ns)
  (dosync
    (alter @#'clojure.core/*loaded-libs* disj ns))
  (when *log-fn*
    (*log-fn* :unload ns)))

(defn unload-impl [state]
  (let [{:keys [to-unload unload-hook]} state]
    (ns-unload (first to-unload) unload-hook)
    (assoc state
      :to-unload (next to-unload))))

(defn ns-load [ns]
  (try
    (require ns :reload) ;; use load to load?
    (when *log-fn*
      (*log-fn* :load ns))
    (catch Throwable t
      (when *log-fn*
        (*log-fn* :load-fail ns t))
      ; (println t)
      (throw t))))

(defn load-impl [state]
  (let [{:keys [to-load]} state]
    (ns-load (first to-load))
    (assoc state
      :to-load (next to-load))))

(defn reload []
  (swap! *state scan-impl)
  (loop [state @*state]
    (cond
      (not-empty (:to-unload state))
      (recur (swap! *state unload-impl))
      
      (not-empty (:to-load state))
      (recur (swap! *state load-impl))
    
      :else
      state)))

(comment
  (dependencies (first-scan ["fixtures"]))
  (dependees (first-scan ["fixtures"]))
  
  (reset! *state (first-scan ["src" "dev" "test"]))
  @*state
  (scan @*state)
  (reload (scan @*state))
  
  (flatten-state @*state)
  (transitive-closure
    (dependencies @*state)
    ['clj-reload.core-test])
  (topo-sort (dependencies @*state)) ;; unload order
  (topo-sort {:a #{:b :c}
              :b #{:c}})
  (dependees @*state))
  