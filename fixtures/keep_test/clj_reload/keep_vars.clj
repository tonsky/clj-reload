(ns clj-reload.keep-vars)

(def normal
  (rand-int Integer/MAX_VALUE))

(defonce *atom
  (atom nil))

(defonce just-var
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
