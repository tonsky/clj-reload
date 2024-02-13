(ns keep)

(def normal
  (rand-int Integer/MAX_VALUE))

(defonce *atom
  (atom nil))

(defonce just-var
  (rand-int Integer/MAX_VALUE))

(def dependent
  [just-var (rand-int Integer/MAX_VALUE)])

^:clj-reload.core/keep
(def meta-var
  (rand-int Integer/MAX_VALUE))

(def normal-2
  (rand-int Integer/MAX_VALUE))
