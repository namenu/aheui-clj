(ns aheui-clj.core-test
  (:require [clojure.test :refer :all]
            [aheui-clj.core :refer :all]
            [clojure.java.io :as io]))

(deftest hello-world-test
  (testing "Hello, World!"
    (let [hello-world (read-code (io/resource "hello_world.aheui"))]
      (is (= (with-out-str (run hello-world initial-machine))
             "Hello, world!\n")))))
