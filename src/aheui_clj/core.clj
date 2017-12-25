(ns aheui-clj.core
  (:require [aheui-clj.machine :as aheui]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn read-code [res]
  (vec (map vec (str/split-lines (slurp res)))))

(def hello-world
  (read-code (io/resource "hello_world.aheui")))

(def initial-machine (aheui/generate-machine))
;(aheui/reset-storages initial-machine)
;(aheui/run hello-world initial-machine)

(defn -main
  [& args]
  (aheui/run hello-world initial-machine))
