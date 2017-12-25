(defproject aheui-clj "0.1.0-SNAPSHOT"
  :description "Aheui implementation"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.4.0"]]
  :main ^:skip-aot aheui-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
