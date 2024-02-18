(ns clj-reload.keep-unsupported)

(defmacro defcomp [& body]
  `(def ~@body))

^:clj-reload/keep
(defcomp v 1)
