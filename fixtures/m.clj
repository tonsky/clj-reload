(ns m
  (:require n o))

(defn on-ns-unload []
  (swap! o/*atom conj :unload-m))