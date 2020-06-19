(ns aheui.core-test
  (:require [clojure.test :refer :all]
            [aheui.core :refer [run]]
            [clojure.java.io :as io]))

(deftest basic-in-out-test
  (testing "in"
    (is (= (with-out-str (with-in-str "45678" (run "방맣희")))
           "뉮")))
  (testing "out"
    (is (= (with-out-str (run "밝밦따망희"))
           "42"))))

(deftest hello-world-test
  (testing "Hello, World!"
    (let [hello-world (slurp (io/resource "hello_world.aheui"))]
      (is (= (with-out-str (run hello-world))
             "Hello, world!\n")))))

(deftest pi-test
  (testing "PI Puzzlet"
    (let [pi (slurp (io/resource "pi.puzzlet.aheui"))]
      (is (= (with-out-str (run pi))
             "314")))))

(run-tests)