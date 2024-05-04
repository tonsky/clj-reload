(ns user
  (:require
    [clojure.core.server :as server]
    [clojure.java.io :as io]
    [clojure.test :as test]
    [clojure.tools.namespace.repl :as ns]
    [clojure.tools.namespace.track :as track]))

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
  (let [{port "--port"} args
        port (if (or (nil? port) (zero? port))
               (+ 1024 (rand-int 64512))
               (parse-long port))]
    (println "Started Server Socket REPL on port" port)
    (let [file (io/file ".repl-port")]
      (spit file port)
      (.deleteOnExit file))
    (server/start-server
      {:name          "repl"
       :accept        'clojure.core.server/repl
       :server-daemon false
       :port          port})))

(defn test-all []
  (reload)
  (test/run-all-tests #"clj-reload\..*-test"))

(defn -test-main [_]
  (let [{:keys [fail error]} (test-all)]
    (System/exit (+ fail error))))
