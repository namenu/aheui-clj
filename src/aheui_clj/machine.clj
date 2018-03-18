(ns aheui-clj.machine
  (:require [clojure.tools.logging :as log]
            [hangul-utils.core :refer [korean-syllable? deconstruct]]))

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

(defn decode
  "Decodes a valid Hangle character or '왜'."
  [ch]
  (if (korean-syllable? ch)
    (deconstruct ch)
    (decode \왜)))

(defn get-inst [code [x y]]
  (let [c (get-in code [y x] \space)]
    (decode c)))

(defn gen-storage [받침]
  (atom (if (= 받침 \ㅇ)
          clojure.lang.PersistentQueue/EMPTY
          [])))

(def gen-storages
  (zipmap final-jaeums (map gen-storage final-jaeums)))

(defn gen-machine []
  {:cursor {:pos [0 0], :v [0 1]}
   :storages gen-storages
   :storage-index nil
   :halted false})

(defn current-storage [machine]
  (get (:storages machine) (:storage-index machine)))

(defn move-cursor [cursor 홀소리]
  (let [dv     (movement 홀소리 (:v cursor))
        dv     (if (:reverse cursor) (map - dv) dv)
        newpos (map + (:pos cursor) dv)]
    {:pos newpos :v dv :reverse false}))

(defn 뽑기
  ([storage]
   (뽑기 storage nil))
  ([storage action]
   (let [popped (peek @storage)]
     (swap! storage pop)
     (case action
       \ㅇ (print popped)
       \ㅎ (print (char popped))
       (log/debug "버리기" popped))
     popped)))

(defn 집어넣기
  ([storage value]
   (집어넣기 storage value nil))
  ([storage value action]
   (let [input (case action
                 \ㅇ (Integer. (read-line))
                 \ㅎ (.read *in*)
                 value)]
     (swap! storage conj input))))

(defn 중복 [storage]
  (집어넣기 storage (peek @storage)))

(defn 바꿔치기 [storage]
  (let [x (뽑기 storage)
        y (뽑기 storage)]
    (집어넣기 storage x)
    (집어넣기 storage y)))

(defn 셈하기 [storage op]
  (if (< (count @storage) 2)
    (throw (java.lang.IllegalStateException. (str "Not enough operands for " op))))
  (let [x (뽑기 storage)
        y (뽑기 storage)]
    (집어넣기 storage (op y x))))

(defn 이동 [machine 받침]
  (let [from (current-storage machine)
        to (get (:storages machine) 받침)]
    (집어넣기 to (뽑기 from))))

(defn 비교 [storage]
  (let [x (뽑기 storage)
        y (뽑기 storage)]
    (집어넣기 (if (>= y x) 1 0))))

(defn 조건 [machine storage]
  (if (= 0 (뽑기 storage))
    (assoc-in machine [:cursor :reverse] true)
    machine))

(defn 끝냄 [machine]
  (let [storage (current-storage machine)]
    (if (empty? @storage) 0 (뽑기 storage))))

(defmacro m-cmd [cmd]
  `(do
     ~cmd
     ~'machine))

(defn 명령 [machine 닿소리 받침]
  (let [storage (current-storage machine)]
    (case 닿소리
      \ㅇ (m-cmd "없음")
      ; ㄷ 묶음
      \ㄷ (m-cmd (셈하기 storage +))
      \ㄸ (m-cmd (셈하기 storage *))
      \ㅌ (m-cmd (셈하기 storage -))
      \ㄴ (m-cmd (셈하기 storage quot))
      \ㄹ (m-cmd (셈하기 storage mod))
      ; ㅁ 묶음
      \ㅁ (m-cmd (뽑기 storage 받침))
      \ㅂ (m-cmd (집어넣기 storage (받침값 받침) 받침))
      \ㅃ (m-cmd (중복 storage))
      \ㅍ (m-cmd (바꿔치기 storage))
      ; ㅅ 묶음
      \ㅅ (assoc machine :storage-index 받침)
      \ㅆ (m-cmd (이동 machine 받침))
      \ㅈ (m-cmd (비교 storage))
      \ㅊ (조건 machine storage)
      ; 기타
      (m-cmd (log/debug "몰라요😅" 닿소리)))))

(defn- 실행
  [machine [닿소리 홀소리 받침]]
  (try
    (-> machine
        (명령 닿소리 받침)
        (update :cursor move-cursor 홀소리))
    (catch java.lang.IllegalStateException e
      ;; keep previous direction when operation failed
      (update machine :cursor move-cursor \ㅐ))))

(defn run [code]
  (loop [machine (gen-machine)]
    (let [[닿소리 홀소리 받침 :as inst] (get-inst code (get-in machine [:cursor :pos]))]
      (if (= \ㅎ 닿소리)
        (끝냄 machine)
        (recur (실행 machine inst))))))
