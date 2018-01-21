(defproject aheui-clj "0.1.0-SNAPSHOT"
  :description "Aheui implementation"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.0"]]
  :git-dependencies [["https://github.com/namenu/hangul-utils.git"
                      "c4f5897874763b88428ddb380e777ca28ea64c95"]]
  :plugins [[lein-git-deps "0.0.2-SNAPSHOT"]]
  :main ^:skip-aot aheui-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
