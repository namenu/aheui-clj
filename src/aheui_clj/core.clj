(ns aheui-clj.core
  (:require [aheui-clj.machine :as aheui]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn read-code [res]
  (vec (map vec (str/split-lines (slurp res)))))

(def hello-world
  (read-code (io/resource "hello_world.aheui")))
(def pi
  (read-code (io/resource "pi.puzzlet.aheui")))
  
;(aheui/run hello-world)
;(aheui/run pi)
