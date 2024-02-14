(ns clj-reload.keep-test.keep-defrecord)

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
