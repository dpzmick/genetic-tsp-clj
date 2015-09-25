(defproject genetic-tsp-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [incanter "1.5.6"]
                 [org.clojure/math.combinatorics "0.1.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  :main ^:skip-aot genetic-tsp-clj.core
  :target-path "target/%s"
  :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]
  :profiles {:uberjar {:aot :all}})
