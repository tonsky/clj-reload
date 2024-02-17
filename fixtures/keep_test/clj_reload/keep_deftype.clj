(ns clj-reload.keep-deftype)

(deftype TypeNormal [t]
  java.lang.Object
  (equals [_ o]
    (and (instance? TypeNormal o)
      (= (.-t ^TypeNormal o) t))))

(def type-normal-new
  (TypeNormal. 0))

(def type-normal-factory
  (->TypeNormal 0))

^:clj-reload/keep
(deftype TypeKeep [t]
  java.lang.Object
  (equals [_ o]
    (and (instance? TypeKeep o)
      (= (.-t ^TypeKeep o) t))))

(def type-keep-new
  (TypeKeep. 0))

(def type-keep-factory
  (->TypeKeep 0))
