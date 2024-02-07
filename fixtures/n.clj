(ns n
  (:require o))

(defn on-ns-unload []
  (swap! o/*atom conj :unload-n))