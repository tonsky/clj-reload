(ns clj-reload.keep-test.keep-custom)

(defmacro deftype+ [& body]
  `(deftype ~@body))

^:clj-reload.core/keep
(deftype+ CustomTypeKeep [t])

(def custom-type-keep
  (CustomTypeKeep. 0))
