(ns clj-reload.parse
  (:require
    [clj-reload.util :as util]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File]))

(defn expand-quotes [form]
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
          (util/log "Unexpected" (first form) "form:" (pr-str decl))
          (recur body' result))
        
        (not (symbol? (first decl)))
        (do
          (util/log "Unexpected" (first form) "form:" (pr-str decl))
          (recur body' result))
        
        (or
          (nil? (second decl))      ;; [a.b.d]
          (keyword? (second decl))) ;; [a.b.e :as e]
        (if (= :as-alias (second decl)) ;; [a.b.e :as-alias e]
          (recur body' result)
          (recur body' (conj! result (first decl))))
        
        :else ;; [a.b f [g :as g]]
        (let [prefix  (first decl)
              symbols (->> (next decl)
                        (remove #(and (sequential? %) (= :as-alias (second %)))) ;; [a.b [g :as-alias g]]
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
   (with-open [rdr (util/file-reader file)]
     (try
       (read-file rdr file)
       (catch Exception e
         (util/log "Failed to read" (.getPath ^File file) (.getMessage e))
         (ex-info (str "Failed to read " (.getPath ^File file)) {:file file} e)))))
  ([rdr file]
   (loop [ns   nil
          nses {}]
     (let [form (util/read-form rdr)
           tag  (when (list? form)
                  (first form))]
       (cond
         (= :clj-reload.util/eof form)
         nses
         
         (= 'ns tag)
         (let [[ns requires] (parse-ns-form form)
               requires      (disj requires ns)]
           (recur ns (update nses ns util/assoc-some
                       :meta     (meta ns)
                       :requires requires
                       :ns-files (util/some-set file))))
          
         (= 'in-ns tag)
         (let [[_ ns] (expand-quotes form)]
           (recur ns (update nses ns util/assoc-some 
                       :in-ns-files (util/some-set file))))
        
         (and (nil? ns) (#{'require 'use} tag))
         (throw (ex-info (str "Unexpected " tag " before ns definition in " file) {:form form}))
        
         (#{'require 'use} tag)
         (let [requires' (parse-require-form (expand-quotes form))
               requires' (disj requires' ns)]
           (recur ns (update-in nses [ns :requires] util/intos requires')))
        
         (or
           (= 'defonce tag)
           (:clj-reload/keep (meta form))
           (and
             (list? form)
             (:clj-reload/keep (meta (second form)))))
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
      (vswap! *m util/update! from #(or % #{}))
      (doseq [to tos
              :when (namespaces to)]
        (vswap! *m util/update! to util/conjs from)))
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
                       (catch Exception _
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
       (let [ends  (reduce into #{} (vals deps))
             roots (->> (keys deps) (remove ends) (sort))]
         (if (not (empty? roots))
           (recur (reduce conj! res roots) (reduce dissoc deps roots))
           (on-cycle deps all-deps)))))))
