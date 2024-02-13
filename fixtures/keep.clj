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

(deftype TypeNormal [t]
  java.lang.Object
  (equals [_ o]
    (and (instance? TypeNormal o)
      (= (.-t ^TypeNormal o) t))))

(def type-normal-new
  (TypeNormal. 0))

(def type-normal-factory
  (->TypeNormal 0))

^:clj-reload.core/keep
(deftype TypeKeep [t]
  java.lang.Object
  (equals [_ o]
    (and (instance? TypeKeep o)
      (= (.-t ^TypeKeep o) t))))

(def type-keep-new
  (TypeKeep. 0))

(def type-keep-factory
  (->TypeKeep 0))

(def normal-2
  (rand-int Integer/MAX_VALUE))
