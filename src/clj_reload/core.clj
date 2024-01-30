(ns clj-reload.core
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File PushbackReader]))

(def reader-opts
  {:read-cond :allow
   :features  #{:clj}
   :eof       ::eof})

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
                   (recur body' (update result :depends (fnil into #{}) (parse-require-form form)))
          
                   :else
                   (recur body' result))))]
    [name body]))

(defn read-file [rdr]
  (try
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
            (recur current-ns (update result current-ns update :depends (fnil into #{}) deps)))
            
          :else
          (recur current-ns result))))
    (catch Exception e
      (when-not (= :reader-exception (:type (ex-data e)))
        (throw e)))))

(defn find-files [dirs since]
  (->> dirs
    (mapcat #(file-seq (io/file %)))
    (filter #(.isFile ^File %))
    (filter #(re-matches #".*\.cljc?" (.getName ^File %)))
    (filter #(> (.lastModified ^File %) since))))

(def *state
  (atom
    {:files {}}))

(defn group [xs key-fn value-fn]
  (reduce
    (fn [m x]
      (update m (key-fn x) (fnil conj #{}) (value-fn x)))
    {}
    xs))

(defn flatten-state [state]
  (for [[file {nses :namespaces}] (:files state)
        [from {tos :depends}] nses
        to tos]
    [from to]))

(defn dependencies [state]
  (group (flatten-state state) first second))

(defn dependees [state]
  (group (flatten-state state) second first))

(defn transitive-closure
  ([deps starts]
   (transitive-closure deps starts #{}))
  ([deps queue acc]
   (if (empty? queue)
     acc
     (let [[start & queue'] queue]
       (if (contains? acc start)
         (recur deps queue' acc)
         (recur deps (into queue (deps start)) (conj acc start)))))))

(defn topo-sort [deps]
  (loop [res  []
         deps deps]
    (if (empty? deps)
      res
      (let [node (some
                   (fn [node]
                     (when (every? #(not (% node)) (vals deps))
                       node))
                   (keys deps))]
        (recur (conj res node) (dissoc deps node))))))

(defn sorted-dependees [state changed-nses]
  (let [dependencies (dependencies state)
        dependees    (dependees state)
        sorted       (topo-sort dependencies)
        ns-set       (transitive-closure dependees changed-nses)]
    (filter ns-set sorted)))

(defn scan [state]
  (let [changed-files (find-files (:dirs state) (:since state 0))
        changed (into {}
                  (for [^File file changed-files]
                    [file {:modified   (.lastModified file)
                           :namespaces (with-open [rdr (java.io.PushbackReader. (io/reader file))]
                                         (read-file rdr))}]))]
    (-> state
      (update :changed merge changed)
      (assoc :since (System/currentTimeMillis)))))

(defn first-scan [dirs]
  (let [state (scan {:dirs dirs})]
    (-> state
      (assoc :files (:changed state))
      (assoc :changed {}))))

(def ^:dynamic *log-fn*)

(defn ns-unload [ns]
  (when *log-fn*
    (*log-fn* :unload ns))
  (println "Unloading" ns))

(defn ns-load [ns]
  (when *log-fn*
    (*log-fn* :load ns))
  (println "Loading" ns))

(defn reload [state]
  (let [changed      (:changed state)
        changed-nses (for [[file {nses :namespaces}] changed
                              [ns _] nses]
                          ns)
        _            (doseq [ns (sorted-dependees state changed-nses)]
                       (ns-unload ns))
        state'       (-> state
                       (update :files merge (:files state) changed)
                       (assoc :changed {}))
        _            (doseq [ns (reverse (sorted-dependees state' changed-nses))]
                       (ns-load ns))]
    state'))

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
  