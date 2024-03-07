(ns clj-reload.keep-downstream
  (:require
    clj-reload.keep-upstream))

(defonce downstream-var
  (rand-int Integer/MAX_VALUE))