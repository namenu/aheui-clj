(ns aheui-clj.core
  (:require [aheui-clj.machine :refer [아희]]
            [clojure.java.io :as io])
  (:gen-class))

(def hello-world
  (slurp (io/resource "hello_world.aheui")))
(def pi
  (slurp (io/resource "pi.puzzlet.aheui")))

;(아희 "밝밦따망희")
;(with-in-str "45678" (아희 "방맣희"))
;(아희 hello-world)
;(아희 pi)
