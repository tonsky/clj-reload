(defproject io.github.tonsky/clj-reload "0.0.0"
  :description "Smarter way to reload Clojure code"
  :license     {:name "MIT" :url "https://github.com/tonsky/clj-reload/blob/master/LICENSE"}
  :url         "https://github.com/tonsky/clj-reload"
  :dependencies
  [[org.clojure/clojure "1.11.1"]]
  :deploy-repositories
  {"clojars"
   {:url "https://clojars.org/repo"
    :username "tonsky"
    :password :env/clojars_token
    :sign-releases false}})