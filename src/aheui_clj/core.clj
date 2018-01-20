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

(defmacro ㅏ [body]
  "Polyglotting macro.
  You can think of virtual character 'ㅏ' coming at (0, 1)."
  `(아희 (name '~body)))

(def hello-world
  (slurp (io/resource "hello_world.aheui")))
(def pi
  (slurp (io/resource "pi.puzzlet.aheui")))

;(with-in-str "45678" (ㅏ 방맣희))
;(with-out-str (ㅏ 밝밦따망희))
;(아희 hello-world)
;(아희 pi)
