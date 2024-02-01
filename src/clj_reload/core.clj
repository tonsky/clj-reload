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

; {:dirs      [<string> ...]
;  :no-unload #{<symbol> ...}
;  :no-load   #{<symbol> ...}
;  :sicne     <long>
;  :files     {<file> -> File}
;  :loaded    #{<symbol>... }}
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

(defn find-files [dirs since]
  (->> dirs
    (mapcat #(file-seq (io/file %)))
    (filter file?)
    (filter #(re-matches #".*\.cljc?" (file-name %)))
    (filter #(> (last-modified %) (or since 0)))))

(defn dependees 
  "ns -> #{downstream-ns ...}"
  [state]
  (let [*m (volatile! (transient {}))]
    (doseq [[_ {nses :namespaces}] (:files state)
            [from {tos :depends}] nses]
      (vswap! *m update! from #(or % #{}))
      (doseq [to tos]
        (vswap! *m update! to conjs from)))
    (persistent! @*m)))

(defn transitive-closure
  "Starts from starts, expands using deps {ns -> #{ns ...}},
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

(defn topo-sort
  "Topologically sorts dependency map {ns -> #{ns ...}}"
  [deps]
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
        (recur (conj! res node) (dissoc deps node))))))

(defn sorted-dependees
  "Starting from changed-nses, returns namespaces to load in
   upstream -> downstream order (load order)"
  [state changed-nses]
  (let [dependees (dependees state)
        ns-set    (transitive-closure dependees changed-nses)
        sorted    (topo-sort dependees)]
    (filterv ns-set sorted)))

(defn scan
  "Returns {<file> -> File}"
  [dirs since]
  (let [changed-files (find-files dirs since)]
    (into {}
      (for [file changed-files]
        [file {:modified (last-modified file)
               :namespaces
               (with-open [rdr (reader file)]
                 (try
                   (read-file rdr)
                   (catch Exception e
                     (printf "Failed to load %s: %s\n" (.getPath ^File file) (.getMessage e)))))}]))))

(defn init-impl [opts]
  (let [dirs  (vec (:dirs opts))
        now   (now)
        files (scan dirs 0)]
    {:dirs      dirs
     :no-unload (set (:no-unload opts))
     :no-load   (set (:no-load opts))
     :files     files
     :since     now}))

(defn init [opts]
  (reset! *state (init-impl opts)))

(defn ns-unload [ns]
  (when *log-fn*
    (*log-fn* :unload ns))
  (remove-ns ns)
  (dosync
    (alter @#'clojure.core/*loaded-libs* disj ns)))

(defn ns-load [ns]
  (when *log-fn*
    (*log-fn* :load ns))
  (try
    (require ns :reload) ;; use load to load?
    (catch Throwable t
      (println t))))

(defn unload-impl [state changed-files]
  (let [{:keys [loaded no-unload no-load]} state
        loaded'       (intos loaded @@#'clojure.core/*loaded-libs*)
        unload?       #(and
                         (loaded' %)
                         (not (no-unload %))
                         (not (no-load %)))
        changed-nses  (for [[_ {nses :namespaces}] changed-files
                            [ns _] nses
                            :when (unload? ns)]
                        ns)]
    (doseq [ns (reverse (sorted-dependees state changed-nses))
            :when (unload? ns)]
      (ns-unload ns))
    (assoc state
      :loaded loaded')))

(defn load-impl [state changed-files now]
  (let [{:keys [loaded no-load]} state
        load?         #(and
                         (loaded %)
                         (not (no-load %)))
        changed-nses  (for [[_ {nses :namespaces}] changed-files
                            [ns _] nses
                            :when (load? ns)]
                        ns)]
    (doseq [ns (sorted-dependees state changed-nses)
            :when (load? ns)]
      (ns-load ns))
    (-> state
      (update :files merge changed-files)
      (assoc :since now)
      (dissoc :loaded))))

(defn reload
  ([]
   (swap! *state reload))
  ([state]
   (let [{:keys [dirs loaded since no-unload no-load]} state
         now           (now)
         changed-files (scan dirs since)
         state'        (unload-impl state changed-files)
         state''       (load-impl state' changed-files now)]
     state'')))

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
  