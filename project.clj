(defproject bookie "0.0.1-SNAPSHOT"
  :description "A Cryptocurrency Exchange Book Keeper"
  :url ""
  :license {}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.443"]
                 [keychain "0.0.1-SNAPSHOT"]]
  :profiles {:dev {:aot :all, :plugins [[lein-gorilla "0.4.0"]]}})
