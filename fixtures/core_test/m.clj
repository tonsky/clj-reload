(ns m
  (:require n o))

(defn before-ns-unload []
  (swap! o/*atom conj :unload-m))

(defn after-ns-reload []
  (swap! o/*atom conj :reload-m))
