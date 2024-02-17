(ns clj-reload.keep-defprotocol)

^:clj-reload/keep
(defprotocol IProto
  :extend-via-metadata true
  (-method [_]))

;; ---

(defrecord RecInline []
  IProto
  (-method [_]
    :rec-inline))

(def rec-inline
  (RecInline.))

;; ---

(defrecord RecExtendProto [])

(def rec-extend-proto
  (RecExtendProto.))

(extend-protocol IProto
  RecExtendProto
  (-method [_]
    :rec-extend-proto))

;; ---

(defrecord RecExtendType [])

(def rec-extend-type
  (RecExtendType.))

(extend-type RecExtendType
  IProto
  (-method [_]
    :rec-extend-type))

;; ---

(defrecord RecExtend [])

(def rec-extend
  (RecExtend.))

(extend RecExtend
  IProto
  {:-method (fn [_]
              :rec-extend)})

;; ---

(def extend-meta
  ^{'clj-reload.keep-defprotocol/-method
    (fn [_]
      :extend-meta)} [])
