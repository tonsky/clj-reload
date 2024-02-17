(ns n
  (:require o))

(defn before-ns-unload []
  (swap! o/*atom conj :unload-n))

(defn after-ns-reload []
  (swap! o/*atom conj :reload-n))
