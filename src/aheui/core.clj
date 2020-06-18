(ns aheui.core
  (:require [hangul-utils.core :refer [korean-syllable? deconstruct]]
            [clojure.string :as str]))

; Copied form hangul-utils
(def final-jaeums
  "The jaeums (consonants) which end Korean characters in modern usage. `nil` is
  included for the characters only made up of two jamo."
  [nil \ㄱ \ㄲ \ㄳ \ㄴ \ㄵ \ㄶ \ㄷ \ㄹ \ㄺ \ㄻ \ㄼ \ㄽ \ㄾ \ㄿ \ㅀ \ㅁ \ㅂ \ㅄ
   \ㅅ \ㅆ \ㅇ \ㅈ \ㅊ \ㅋ \ㅌ \ㅍ \ㅎ])

(def 받침값
  (zipmap final-jaeums [0 2 4 4 2 5 5 3 5 7 9 9 7 9 9 8 4 4 6 2 4 0 3 4 3 4 4 0]))

(defn movement [홀소리 v]
  (get {\ㅏ [1 0]
        \ㅑ [2 0]
        \ㅓ [-1 0]
        \ㅕ [-2 0]
        \ㅗ [0 -1]
        \ㅛ [0 -2]
        \ㅜ [0 1]
        \ㅠ [0 2]
        \ㅡ [(v 0) (- (v 1))]
        \ㅢ [(- (v 0)) (- (v 1))]
        \ㅣ [(- (v 0)) (v 1)]}
       홀소리
       v))

(defn decode [str]
  (loop [[line & lines] (str/split-lines str)
         y    0
         code {}]
    (if line
      (let [code' (->> line
                       (map-indexed (fn [x v]
                                      [[x y] (if (korean-syllable? v)
                                               (deconstruct v)
                                               nil)]))
                       (remove (comp nil? second))
                       (into code))]
        (recur lines (inc y) code'))
      code)))

(defn ->machine [code]
  (let [init-storage #(if (= % \ㅇ)
                        clojure.lang.PersistentQueue/EMPTY
                        [])]
    {:code          code
     :cursor        {:pos [0 0], :dir [0 1]}
     :storages      (zipmap final-jaeums (map init-storage final-jaeums))
     :storage-index nil
     :halted        false
     :exit-code     0}))

(defn move-cursor [cursor 홀소리]
  (let [dv     (movement 홀소리 (:v cursor))
        dv     (if (:reverse cursor) (map - dv) dv)
        newpos (map + (:pos cursor) dv)]
    {:pos newpos :v dv :reverse false}))

(defn 뽑기
  "1. storage -> [storage, value]
   2. storages, index -> [storages, value]"
  ([storage]
   ((juxt pop peek) storage))
  ([storages index]
   ((juxt #(update % index pop)
          #(peek (get % index))) storages)))

(defn 집어넣기
  ([storage value]
   (conj storage value))
  ([storages index value]
   (update storages index conj value)))

(defn 중복 [storages index]
  (let [storage (get storages index)
        top     (peek storage)]
    (update storages index 집어넣기 top)))

(defn 바꿔치기 [storages index]
  (let [s (get storages index)
        [s x] (뽑기 s)
        [s y] (뽑기 s)
        s (-> s (집어넣기 x) (집어넣기 y))]
    (assoc storages index s)))

(defn 셈하기 [storages index op]
  (let [s (get storages index)]
    (assert (>= (count s) 2) (str "storage underflow: index of" index))
    (let [[s x] (뽑기 s)
          [s y] (뽑기 s)
          s (집어넣기 s (op y x))]
      (assoc storages index s))))

#_(defn 이동 [machine 받침]
    (let [from (current-storage machine)
          to   (get (:storages machine) 받침)]
      (집어넣기 to (뽑기 from))))

#_(defn 비교 [storage]
    (let [x (뽑기 storage)
          y (뽑기 storage)]
      (집어넣기 (if (>= y x) 1 0))))

#_(defn 조건 [machine storage]
    (if (= 0 (뽑기 storage))
      (assoc-in machine [:cursor :reverse] true)
      machine))

(defn 끝냄 [{:keys [storages storage-index] :as machine}]
  (let [storage (get storage-index storages)]
    (if (empty? storage)
      (assoc machine :halted true)
      (let [[s v] (뽑기 storage)]
        (-> (assoc machine :halted true)
            (update :storages assoc storage-index s)
            (assoc :exit-code v))))))

(defn exec [{:keys [storages storage-index] :as machine} [닿소리 _ 받침]]
  (case 닿소리
    ; ㅇ 묶음
    \ㅇ machine
    \ㅎ (끝냄 machine)
    ; ㄷ 묶음
    \ㄷ (update machine :storages 셈하기 storage-index +)
    \ㄸ (update machine :storages 셈하기 storage-index *)
    \ㅌ (update machine :storages 셈하기 storage-index -)
    \ㄴ (update machine :storages 셈하기 storage-index quot)
    \ㄹ (update machine :storages 셈하기 storage-index mod)
    ; ㅁ 묶음
    \ㅁ (let [[storages popped] (뽑기 storages storage-index)]
         (case 받침
           \ㅇ (print popped)
           \ㅎ (print (char popped))
           :no-op)
         (assoc machine :storages storages))
    \ㅂ (let [input (case 받침
                     \ㅇ (Integer. (read-line))
                     \ㅎ (.read *in*)
                     (받침값 받침))]
         (update machine :storages 집어넣기 storage-index input))
    \ㅃ (update machine :storages 중복 storage-index)
    \ㅍ (update machine :storages 바꿔치기 storage-index)
    ; ㅅ 묶음
    \ㅅ (assoc machine :storage-index 받침)
    ;\ㅆ (이동 machine 받침)
    ;\ㅈ (비교 storage)
    ;\ㅊ (조건 machine storage)
    ; else
    (prn "몰라요😅" 닿소리)
    ))

(require '[clojure.string :as str])

(defn get-inst [{:keys [code cursor]}]
  (code (:pos cursor)))

(defn update-cursor [{:keys [dir reverse pos]} [_ 홀소리 _]]
  (let [dv (movement 홀소리 dir)]
    {:pos     (mapv (if reverse - +) pos dv)
     :dir     dv
     :reverse false}))

(defn instruction-cycle [machine]
  (let [inst (get-inst machine)]
    ;(prn "> " inst)
    (-> (exec machine inst)
        (update :cursor update-cursor inst))))

(defn run
  ([source it-cnt]
   (let [machine (->machine (decode source))]
     (->> (iterate instruction-cycle machine)
          (drop it-cnt)
          (first))))
  ([source]
   (let [machine (->machine (decode source))]
     (->> (iterate instruction-cycle machine)
          (drop-while #(not (:halted %)))
          (first)))))


;; must-have scratch pad
(comment
  (require '[clojure.java.io :as io])
  (let [source (slurp (io/resource "hello_world.aheui"))
        #_#_source (slurp (io/resource "pi.puzzlet.aheui"))]
    (run source)
    0
    ))
