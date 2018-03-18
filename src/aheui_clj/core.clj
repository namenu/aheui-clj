(ns aheui-clj.core
  (:require [aheui-clj.machine :refer [run]]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn 아희 [raw]
  (let [code (->> (str/split-lines raw)
                  (map vec)
                  vec)]
    (run code)))

(comment

  (with-in-str "45678" (아희 "방맣희"))
  (with-out-str (아희 "밝밦따망희"))

  (def hello-world
    (slurp (io/resource "hello_world.aheui")))
  (def pi
    (slurp (io/resource "pi.puzzlet.aheui")))

  (아희 hello-world)
  (아희 pi))
