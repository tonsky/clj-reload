(ns clj-reload.keep-custom)

(defmacro deftype+ [& body]
  `(deftype ~@body))

^:clj-reload/keep
(deftype+ CustomTypeKeep [t])

(def custom-type-keep
  (CustomTypeKeep. 0))
