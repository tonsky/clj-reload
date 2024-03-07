(ns clj-reload.util
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    [clojure.lang LineNumberingPushbackReader]
    [java.io File StringReader]
    [java.net URLClassLoader]))

(def ^:dynamic *log-fn*
  println)

(defn log [& args]
  (when *log-fn*
    (apply *log-fn* args)))

(def reader-opts
  {:read-cond :allow
   :features  #{:clj}
   :eof       ::eof})

(def dummy-resolver
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_]
      'user)
    (resolveClass [_ sym]
      sym)
    (resolveAlias [_ sym]
      sym)
    (resolveVar [_ sym]
      sym)))

(defn read-form [reader]
  (binding [*read-eval* false
            *reader-resolver* dummy-resolver]
    (read reader-opts reader)))

(defn throwable? [o]
  (instance? Throwable o))

(defn update! [m k f & args]
  (assoc! m k (apply f (m k) args)))

(def conjs
  (fnil conj #{}))

(def intos
  (fnil into #{}))

(defn assoc-some [m & kvs]
  (reduce
    (fn [m [k v]]
      (cond-> m
        (some? v) (assoc k v)))
    m
    (partition 2 kvs)))

(defn some-map [& kvs]
  (apply assoc-some nil kvs))

(defn some-set [& vals]
  (not-empty
    (set (filter some? vals))))

(defn map-vals [f m]
  (when (some? m)
    (persistent!
      (reduce-kv
        #(assoc! %1 %2 (f %3))
        (transient (empty m))
        m))))

(defn deep-merge [& ms]
  (apply merge-with merge ms))

(defmacro for-map [& body]
  `(into {}
     (for ~@body)))

(defmacro for-set [& body]
  `(into #{}
     (for ~@body)))

(defn doeach [f xs]
  (doseq [x xs]
    (f x)))

(defn now []
  (System/currentTimeMillis))

(defn last-modified [^File f]
  (some-> f .lastModified))

(defn set-last-modified [^File f t]
  (some-> f (.setLastModified t)))

(defn file? [^File f]
  (some-> f .isFile))

(defn directory? [^File f]
  (some-> f .isDirectory))

(defn file-name [^File f]
  (some-> f .getName))

(defn file-path [^File f]
  (some-> f .getPath))

(defn file-delete [^File f]
  (some-> f .delete))

(defn file-reader ^LineNumberingPushbackReader [f]
  (LineNumberingPushbackReader.
    (io/reader (io/file f))))

(defn string-reader ^LineNumberingPushbackReader [^String s]
  (LineNumberingPushbackReader.
    (StringReader. s)))

(defn ns-load-file [content ns ^File file]
  (let [[_ ext] (re-matches #".*\.([^.]+)" (.getName file))
        path    (-> ns str (str/replace #"\-" "_") (str/replace #"\." "/") (str "." ext))]
    (Compiler/load (StringReader. content) path (.getName file))))

(defn loader-classpath []
  (->> (clojure.lang.RT/baseLoader)
    (iterate #(.getParent ^ClassLoader %))
    (take-while identity)
    (filter #(instance? URLClassLoader %))
    (mapcat #(.getURLs ^URLClassLoader %))
    (map io/as-file)
    (filter directory?)))

(defn system-classpath []
  (-> (System/getProperty "java.class.path")
    (str/split (re-pattern (System/getProperty "path.separator")))
    (->> (map io/as-file)
      (filter directory?))))

(defn classpath-dirs []
  (->> (or
         (not-empty (loader-classpath))
         (not-empty (system-classpath)))
    (distinct)
    (mapv file-path)))

(comment
  (classpath-dirs)
  (system-classpath))