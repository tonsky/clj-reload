(ns clj-reload.keep
  (:require
    [clj-reload.util :as util]
    [clojure.string :as str])
  (:import
    [clojure.lang Var]
    [java.io File]))

(defn maybe-quote [form]
  (cond
    (symbol? form)
    (list 'quote form)
   
    (and (sequential? form) (not (vector? form)))
    (list 'quote form)
   
    :else
    form))

(defn meta-str [var]
  (let [meta (dissoc (meta var) :ns :file :line :column :name)]
    (when-not (empty? meta)
      (str "^" (pr-str (util/map-vals maybe-quote meta)) " "))))

(defn classname [ns sym]
  (-> (name ns)
    (str/replace "-" "_")
    (str "." sym)))

(def keep-methods-defs
  {:resolve 
   (fn [ns sym keep]
     (when-some [var (resolve (symbol (name ns) (name sym)))]
       {:var var}))
   
   :stash
   (fn [ns sym keep]
     (intern *ns* sym @(:var keep)))
   
   :patch
   (fn [ns sym keep]
     (str
       "(def " (meta-str (:var keep)) sym " @#'clj-reload.stash/" sym ")"))})

(def keep-methods-deftype
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       {:ctor ctor}))
   
   :stash
   (fn [ns sym keep]
     (intern *ns* (symbol (str "->" sym)) @(:ctor keep)))
   
   :patch
   (fn [ns sym keep]
     (str
       "(clojure.core/import " (classname ns sym) ") "
       "(def " (meta-str (:ctor keep)) "->" sym " clj-reload.stash/->" sym ")"))})

(def keep-methods-defrecord
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       (when-some [map-ctor (resolve (symbol (name ns) (str "map->" sym)))]
         {:ctor ctor
          :map-ctor map-ctor})))
   
   :stash
   (fn [ns sym keep]
     (intern *ns* (symbol (str "->" sym)) @(:ctor keep))
     (intern *ns* (symbol (str "map->" sym)) @(:map-ctor keep)))
   
   :patch
   (fn [ns sym keep]
     (str
       "(clojure.core/import " (classname ns sym) ") "
       "(def " (meta-str (:ctor keep)) "->" sym " clj-reload.stash/->" sym ") "
       "(def " (meta-str (:map-ctor keep)) "map->" sym " clj-reload.stash/map->" sym ")"))})

(defn update-protocol-method-builders [proto & vars]
  (let [mb   (:method-builders proto)
        vars (util/for-map [var vars]
               [(symbol var) var])
        mb'  (util/for-map [[var val] mb]
               [(vars (symbol var)) val])]
    (alter-var-root (:var proto) assoc :method-builders mb')))

(def keep-methods-defprotocol
  {:resolve 
   (fn [ns sym keep]
     (when-some [proto (resolve (symbol (name ns) (name sym)))]
       {:proto   proto
        :methods (util/for-map [[method-var _] (:method-builders @proto)]
                   [(.-sym ^Var method-var) method-var])}))
   
   :stash
   (fn [ns sym keep]
     (intern *ns* sym @(:proto keep))
     (doseq [[method-sym method] (:methods keep)]
       (intern *ns* method-sym @method)))
   
   :patch
   (fn [ns sym keep]
     (str
       "(def " (meta-str (:proto keep)) sym " clj-reload.stash/" sym ") "
       "(clojure.core/alter-var-root #'" sym " assoc :var #'" sym ") "
       (str/join " "
         (for [[method-sym method] (:methods keep)]
           (str "(def " (meta-str method) "^{:protocol #'" sym "} " method-sym " clj-reload.stash/" method-sym ")")))
       " (clj-reload.keep/update-protocol-method-builders " sym " "
       (str/join " "
         (for [[method-sym _] (:methods keep)]
           (str "#'" method-sym))) ") "
       ; "(-reset-methods " sym ")"
       ))})

(def keep-methods
  (delay
    @(resolve 'clj-reload.core/keep-methods)))

(defn keep-resolve [ns sym keep]
  ((:resolve (@keep-methods (:tag keep))) ns sym keep))

(defn keep-stash [ns sym keep]
  ((:stash (@keep-methods (:tag keep))) ns sym keep))

(defn keep-patch [ns sym keep]
  ((:patch (@keep-methods (:tag keep))) ns sym keep))

(defn resolve-keeps [ns syms]
  (util/for-map [[sym keep] syms
                 :let  [resolved (keep-resolve ns sym keep)]
                 :when resolved]
    [sym (merge keep resolved)]))

(defn patch-file [content patches]
  (let [rdr     (util/string-reader content)
        patched (StringBuilder.)]
    (loop []
      (.captureString rdr)
      (let [form (binding [*read-eval* false]
                   (read util/reader-opts rdr))
            text (.getString rdr)]
        (cond
          (= :clj-reload.util/eof form)
          (str patched)
          
          (and (list? form) (>= (count form) 2) (contains? patches (take 2 form)))
          (let [[_ ws]  (re-matches #"(?s)(\s*).*" text)
                _       (.append patched ws)
                text    (subs text (count ws))
                lines   (str/split-lines text)
                text'   (patches (take 2 form))
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
    ;; stash keeps in clj-reload.stash
    (binding [*ns* *ns*]
      (in-ns 'clj-reload.stash)
      (doseq [[sym keep] keeps]
        (keep-stash ns sym keep)))
    
    ;; replace keeps in file with refs to clj-reload.stash
    (let [patch   (util/for-map [[sym keep] keeps]
                    [(list (:tag keep) sym) (keep-patch ns sym keep)])
          content (patch-file (slurp file) patch)]
      ; (println content)
      ;; load modified file
      (util/ns-load-file content ns file))
    
    ;; check 
    (@#'clojure.core/throw-if (not (find-ns ns))
      "namespace '%s' not found after loading '%s'"
      ns (.getPath file))
      
    (finally
      ;; drop everything in stash
      (remove-ns 'clj-reload.stash)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj 'clj-reload.stash)))))
