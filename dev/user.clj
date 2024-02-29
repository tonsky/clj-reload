(ns user
  (:require
    [clojure.tools.namespace.repl :as ns]
    [clojure.tools.namespace.track :as track]
    [duti.core :as duti]))

(ns/disable-reload!)

(ns/set-refresh-dirs "src" "dev" "test")

(defn reload
  ([]
   (reload nil))
  ([opts]
   (set! *warn-on-reflection* true)
   (let [tracker (ns/scan opts)
         cnt     (count (::track/load tracker))
         res     (apply ns/refresh-scanned (mapcat vec opts))]
     (when (instance? Throwable res)
       (throw res))
     (str "Reloaded " cnt " namespace" (when (> cnt 1) "s")))))

(defn -main [& args]
  (alter-var-root #'*command-line-args* (constantly args))
  (let [{port "--port"} args]
    (duti/start-socket-repl {:port (some-> port parse-long)})))

(defn test-all []
  (reload)
  (duti/test-throw #"clj-reload\..*-test"))

(defn -test-main [_]
  (reload)
  (duti/test-exit #"clj-reload\..*-test"))

(comment
  (reload)
  (test-all))