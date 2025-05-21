(ns clj-reload.keep-vars
  (:require
   [clj-reload.dependency]))

(def normal
  (rand-int Integer/MAX_VALUE))

(defonce *atom
  (atom nil))

^:clj-reload/keep
(def just-var
  (rand-int Integer/MAX_VALUE))

(def ^:clj-reload/keep just-var-2
  (rand-int Integer/MAX_VALUE))

^:clj-reload/keep
(def ^:private private-var
  (rand-int Integer/MAX_VALUE))

(def dependent
  [just-var (rand-int Integer/MAX_VALUE)])

^:clj-reload/keep
(def ^{:k :v} meta-var
  (rand-int Integer/MAX_VALUE))

^:clj-reload/keep
(defn public-fn [a])

^:clj-reload/keep
(defn- ^String private-fn [a b c])

(def normal-2
  (rand-int Integer/MAX_VALUE))
