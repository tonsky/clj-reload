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
      (str "^" (pr-str (util/map-vals meta maybe-quote)) " "))))

(defn classname [ns sym]
  (-> (name ns)
    (str/replace "-" "_")
    (str "." sym)))

(defn stash-ns []
  (or
    (find-ns 'clj-reload.stash)
    (binding [*ns* *ns*]
      (in-ns 'clj-reload.stash)
      *ns*)))

(def keep-methods-defs
  {:resolve 
   (fn [ns sym keep]
     (when-some [var (resolve (symbol (name ns) (name sym)))]
       {:var var}))
   
   :patch
   (fn [ns sym keep]
     (when-some [var (:var keep)]
       (intern (stash-ns) sym @var)
       (str
         "(def " (meta-str var) sym " @#'clj-reload.stash/" sym ")")))})

(def keep-methods-deftype
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       {:ctor ctor}))
   
   :patch
   (fn [ns sym keep]
     (when-some [ctor (:ctor keep)]
       (intern (stash-ns) (symbol (str "->" sym)) @ctor)
       (str
         "(clojure.core/import " (classname ns sym) ") "
         "(def " (meta-str ctor) "->" sym " clj-reload.stash/->" sym ")")))})

(def keep-methods-defrecord
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       (when-some [map-ctor (resolve (symbol (name ns) (str "map->" sym)))]
         {:ctor ctor
          :map-ctor map-ctor})))
   
   :patch
   (fn [ns sym keep]
     (when-some [ctor (:ctor keep)]
       (when-some [map-ctor (:map-ctor keep)]
         (intern (stash-ns) (symbol (str "->" sym)) @ctor)
         (intern (stash-ns) (symbol (str "map->" sym)) @map-ctor)
         (str
           "(clojure.core/import " (classname ns sym) ") "
           "(def " (meta-str ctor) "->" sym " clj-reload.stash/->" sym ") "
           "(def " (meta-str map-ctor) "map->" sym " clj-reload.stash/map->" sym ")"))))})

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
   
   :patch
   (fn [ns sym keep]
     (when-some [proto (:proto keep)]
       (when-some [methods (:methods keep)]
         (intern (stash-ns) sym @proto)
         (doseq [[method-sym method] methods]
           (intern (find-ns 'clj-reload.stash) method-sym @method))
         (str
           "(def " (meta-str proto) sym " clj-reload.stash/" sym ") "
           "(clojure.core/alter-var-root #'" sym " assoc :var #'" sym ") "
           (str/join " "
             (for [[method-sym method] methods]
               (str "(def " (meta-str method) "^{:protocol #'" sym "} " method-sym " clj-reload.stash/" method-sym ")")))
           " (clj-reload.keep/update-protocol-method-builders " sym " "
           (str/join " "
             (for [[method-sym _] methods]
               (str "#'" method-sym))) ") "
           ; "(-reset-methods " sym ")"
           ))))})

(def keep-methods
  (delay
    @(resolve 'clj-reload.core/keep-methods)))

(defn keep-resolve [ns sym keep]
  ((:resolve (@keep-methods (:tag keep))) ns sym keep))

(defn keep-patch [ns sym keep]
  ((:patch (@keep-methods (:tag keep))) ns sym keep))

(defn resolve-keeps [ns syms]
  (util/for-map [[sym keep] syms
                 :let [resolved (keep-resolve ns sym keep)]
                 :when resolved]
    [sym resolved]))

(defn patch-file [content patch-fn]
  (let [rdr     (util/string-reader content)
        patched (StringBuilder.)]
    (loop []
      (.captureString rdr)
      (let [form (util/read-form rdr)
            text (.getString rdr)]
        (cond
          (= :clj-reload.util/eof form)
          (str patched)
          
          (and (list? form) (>= (count form) 2))
          (if-some [text' (patch-fn (take 2 form))]
            (let [[_ ws] (re-matches #"(?s)(\s*).*" text)
                  _      (.append patched ws)
                  text   (subs text (count ws))
                  lines  (str/split-lines text)
                  text'' (if (= 1 (count lines))
                           (if (<= (count text) (count text'))
                             text'
                             (str text' (str/join (repeat (- (count text) (count text')) \space))))
                           (str text'
                             (str/join (repeat (dec (count lines)) \newline))
                             (str/join (repeat (count (last lines)) \space))))]
              (do
                (.append patched text'')
                (recur)))
            (do
              (.append patched text)
              (recur)))
          
          :else
          (do
            (.append patched text)
            (recur)))))))

(defn patch-fn [ns keeps]
  (fn [[tag sym]]
    (when-some [keep (get keeps sym)]
      (when (= tag (:tag keep))
        (keep-patch ns sym keep)))))

(defn ns-load-patched [ns ^File file keeps]
  (try    
    (let [content (patch-file (slurp file) (patch-fn ns keeps))]
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
