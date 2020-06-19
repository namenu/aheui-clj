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

(def required-operands
  {\ㄷ 2 \ㄸ 2 \ㅌ 2 \ㄴ 2 \ㄹ 2 \ㅁ 1 \ㅃ 1 \ㅍ 2 \ㅆ 1 \ㅈ 2 \ㅊ 1})

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

(defn update-cursor [{:keys [dir reverse pos]} 홀소리]
  ;(prn 홀소리 pos)
  (let [dv (movement 홀소리 dir)]
    {:pos     (mapv (if reverse - +) pos dv)
     :dir     dv
     :reverse false}))

(defn reverse-cursor [cursor]
  (assoc cursor :reverse true))

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
    (let [[s x] (뽑기 s)
          [s y] (뽑기 s)
          v (op y x)
          s (집어넣기 s v)]
      (assoc storages index s))))

(defn 이동 [storages from to]
  (let [[storages' v] (뽑기 storages from)]
    (집어넣기 storages' to v)))

(defn 비교 [x y]
  (if (>= x y) 1 0))

(defn 조건 [{:keys [storages storage-index] :as machine}]
  (let [[storages' popped] (뽑기 storages storage-index)
        machine' (assoc machine :storages storages')]
    (if (= 0 popped)
      (update machine' :cursor reverse-cursor)
      machine')))

(defn 끝냄 [{:keys [storages storage-index] :as machine}]
  (let [storage (get storages storage-index)]
    (if (empty? storage)
      (assoc machine :halted true)
      (let [[s v] (뽑기 storage)]
        (-> (assoc machine :halted true)
            (update :storages assoc storage-index s)
            (assoc :exit-code v))))))

(defn underflow? [{:keys [storages storage-index]} cmd]
  (if-let [required (required-operands cmd)]
    (let [storage (get storages storage-index)]
      (< (count storage) required))
    false))

(defn exec [{:keys [storages storage-index] :as machine} 닿소리 받침]
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
    \ㅆ (update machine :storages 이동 storage-index 받침)
    \ㅈ (update machine :storages 셈하기 storage-index 비교)
    \ㅊ (조건 machine)
    ; else
    machine
    ))

(require '[clojure.string :as str])

(defn get-inst [{:keys [code cursor]}]
  (code (:pos cursor)))

(defn instruction-cycle [machine]
  (let [[닿소리 홀소리 받침 :as inst] (get-inst machine)]
    ;(prn "> " inst (get (:storages machine) (:storage-index machine)))
    (if (underflow? machine 닿소리)
      (-> (update machine :cursor reverse-cursor)
          (update :cursor update-cursor 홀소리))
      (-> (exec machine 닿소리 받침)
          (update :cursor update-cursor 홀소리)))))

(defn run
  ([source]
   (let [machine (->machine (decode source))]
     (->> (iterate instruction-cycle machine)
          (drop-while #(not (:halted %)))
          (first))))
  ([source it-cnt]
   (let [machine (->machine (decode source))]
     (->> (iterate instruction-cycle machine)
          (drop it-cnt)
          (first)))))


;; must-have scratch pad
(comment
  (require '[clojure.java.io :as io])
  (let [#_#_source (slurp (io/resource "hello_world.aheui"))
        source (slurp (io/resource "pi.puzzlet.aheui"))]
    (-> (run source)
        (dissoc :code))))
