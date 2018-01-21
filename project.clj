(defproject aheui-clj "0.1.0-SNAPSHOT"
  :description "Aheui implementation"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.0"]
                 [hangul-utils "0.1.1-SNAPSHOT"]]
  :main ^:skip-aot aheui-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
