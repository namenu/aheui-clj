(ns aheui-clj.core-test
  (:require [clojure.test :refer :all]
            [aheui-clj.machine :refer [아희]]
            [clojure.java.io :as io]))

(deftest hello-world-test
  (testing "Hello, World!"
    (let [hello-world (slurp (io/resource "hello_world.aheui"))]
      (is (= (with-out-str (아희 hello-world))
             "Hello, world!\n")))))

(deftest pi-test
  (testing "PI Puzzlet"
    (let [pi (slurp (io/resource "pi.puzzlet.aheui"))]
      (is (= (with-out-str (아희 pi))
             "314")))))
