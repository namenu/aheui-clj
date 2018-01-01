(ns aheui-clj.core-test
  (:require [clojure.test :refer :all]
            [aheui-clj.machine :as aheui]
            [aheui-clj.core :refer [read-code]]
            [clojure.java.io :as io]))

(deftest hello-world-test
  (testing "Hello, World!"
    (let [hello-world (read-code (io/resource "hello_world.aheui"))]
      (is (= (with-out-str (aheui/run hello-world))
             "Hello, world!\n")))))

(deftest pi-test
  (testing "PI Puzzlet"
    (let [pi (read-code (io/resource "pi.puzzlet.aheui"))]
      (is (= (with-out-str (aheui/run pi))
             "314")))))
