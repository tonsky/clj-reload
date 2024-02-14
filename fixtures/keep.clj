(ns keep)

(def normal
  (rand-int Integer/MAX_VALUE))

;; vars

(defonce *atom
  (atom nil))

(defonce just-var
  (rand-int Integer/MAX_VALUE))

(def dependent
  [just-var (rand-int Integer/MAX_VALUE)])

^:clj-reload.core/keep
(def ^{:k :v} meta-var
  (rand-int Integer/MAX_VALUE))

^:clj-reload.core/keep
(defn public-fn [a])

^:clj-reload.core/keep
(defn- ^String private-fn [a b c])

;; deftype

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

;; defrecord

(defrecord RecordNormal [t])

(def record-normal-new
  (RecordNormal. 0))

(def record-normal-factory
  (->RecordNormal 0))

(def record-normal-map-factory
  (map->RecordNormal {:t 0}))

^:clj-reload.core/keep
(defrecord RecordKeep [t])

(def record-keep-new
  (RecordKeep. 0))

(def record-keep-factory
  (->RecordKeep 0))

(def record-keep-map-factory
  (map->RecordKeep {:t 0}))

;; deftype+

(defmacro deftype+ [& body]
  `(deftype ~@body))

^:clj-reload.core/keep
(deftype+ CustomTypeKeep [t])

(def custom-type-keep
  (CustomTypeKeep. 0))

;; end

(def normal-2
  (rand-int Integer/MAX_VALUE))
