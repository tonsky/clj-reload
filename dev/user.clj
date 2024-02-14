(ns user
  (:require
    [duti.all :as duti]))

(duti/set-dirs "src" "dev" "test")

(def reload
  duti/reload)

(defn -main [& args]
  (alter-var-root #'*command-line-args* (constantly args))
  (let [{port "--port"} args]
    (duti/start-socket-repl {:port (some-> port parse-long)})))

(defn test-all []
  (require 'clj-reload.core-test 'clj-reload.keep-test 'clj-reload.parse-test)
  (reload)
  (duti/test-throw #"clj-reload\..*-test"))

(defn -test-main [_]
  (require 'clj-reload.core-test 'clj-reload.keep-test 'clj-reload.parse-test)
  (duti/test-exit #"clj-reload\..*-test"))
