(defproject ursula "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
  :main ^:skip-aot ursula.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
