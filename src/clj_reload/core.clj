(ns clj-reload.core
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io File]))

(def dirs
  ["src"])

(defn find-files [dirs]
  (->> dirs
    (mapcat #(file-seq (io/file %)))
    (filter #(.isFile ^File %))
    (filter #(re-matches #".*\.cljc?" (.getName ^File %)))))

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
      (let [form (read reader-opts rdr)
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

(comment  
  (-> (read (java.io.PushbackReader. (java.io.StringReader. "(require 'a.b.k)")))
    second
    type)
  (find-files ["src" "dev" "test"]))
