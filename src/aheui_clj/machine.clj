(ns aheui-clj.machine
  (:require [clojure.tools.logging :as log]))

(def 처음
  [\ㄱ \ㄲ \ㄴ \ㄷ \ㄸ \ㄹ \ㅁ \ㅂ \ㅃ \ㅅ \ㅆ \ㅇ \ㅈ \ㅉ \ㅊ \ㅋ \ㅌ \ㅍ \ㅎ])
(def 가운데
  [\ㅏ \ㅐ \ㅑ \ㅒ \ㅓ \ㅔ \ㅕ \ㅖ \ㅗ \ㅘ \ㅙ \ㅚ \ㅛ \ㅜ \ㅝ \ㅞ \ㅟ \ㅠ \ㅡ \ㅢ \ㅣ])
(def 끝
  [\0 \ㄱ \ㄲ \ㄳ \ㄴ \ㄵ \ㄶ \ㄷ \ㄹ \ㄺ \ㄻ \ㄼ \ㄽ \ㄾ \ㄿ \ㅀ \ㅁ \ㅂ \ㅄ \ㅅ \ㅆ \ㅇ \ㅈ \ㅊ \ㅋ \ㅌ \ㅍ \ㅎ])
(def 받침
  [0 2 4 4 2 5 5 3 5 7 9 9 7 9 9 8 4 4 6 2 4 0 3 4 3 4 4 0])

(defn decode
  "Decodes a valid Hangle character or decodes '왜' for invalid one."
  [ch]
  (if (re-matches #"[가-힇]" (str ch))
    (let [c (- (int ch) 0xAC00)
          끝idx (mod c 28)
          가idx (-> c
                   (- 끝idx)
                   (/ 28)
                   (mod 21))
          첫idx (-> c
                   (- 끝idx)
                   (/ 28)
                   (- 가idx)
                   (/ 21))]
      {:첫 (처음 첫idx)
       :가 (가운데 가idx)
       :끝 (끝 끝idx)
       :값 (받침 끝idx)})
    (decode \왜)))

;;;

(defn gen-storage [받침]
  (atom (if (= 받침 \ㅇ)
          clojure.lang.PersistentQueue/EMPTY
          [])))

(def gen-storages
  (zipmap 끝 (map gen-storage 끝)))

(defn gen-machine []
  {:cursor {:x 0
            :y 0
            :v [0 1]}
   :storages gen-storages
   :storage-index (atom \0)})

(defn current-storage [machine]
  (get (:storages machine) @(:storage-index machine)))

(defn move-cursor [{x :x y :y v :v} 홀소리]
  (let [dv (case 홀소리
             \ㅏ [1 0]
             \ㅑ [2 0]
             \ㅓ [-1 0]
             \ㅕ [-2 0]
             \ㅗ [0 -1]
             \ㅛ [0 -2]
             \ㅜ [0 1]
             \ㅠ [0 2]
             \ㅡ [(v 0) (- (v 1))]
             \ㅢ [(- (v 0)) (- (v 1))]
             \ㅣ [(- (v 0)) (v 1)]
             v)]
    {:x (+ x (dv 0))
     :y (+ y (dv 1))
     :v dv}))

(defn 뽑기 [storage action]
  (let [popped (peek @storage)]
    (swap! storage pop)
    (case action
      \ㅇ (print popped)
      \ㅎ (print (char popped))
      (log/debug "버리기" popped))
    popped))

(defn 집어넣기
  ([storage value]
   (집어넣기 storage \0 value))
  ([storage action value]
   (let [input (case action
                  \ㅇ (Integer. (read-line))
                  \ㅎ (.read *in*)
                  value)]
      (swap! storage conj input))))

(defn 중복 [storage]
  (집어넣기 storage (peek @storage)))

(defn 바꿔치기 [storage]
  (let [x (뽑기 storage nil)
        y (뽑기 storage nil)]
    (집어넣기 storage x)
    (집어넣기 storage y)))

(defn 셈하기 [storage op]
  (if (< (count @storage) 2)
    (throw (java.lang.IllegalStateException. (str "Not enough operands for " op))))
  (let [x (뽑기 storage nil)
        y (뽑기 storage nil)]
    (집어넣기 storage (op y x))))

(defn 선택 [machine 받침]
  (reset! (:storage-index machine) 받침))

(defn 이동 [machine 받침]
  (let [from (current-storage machine)
        to (get (:storages machine) 받침)]
    (집어넣기 to (뽑기 from nil))))

(defn 끝냄 [machine]
  (let [storage (current-storage machine)]
    (if (empty? @storage) 0 (뽑기 storage nil))))

(defn- exec!
  "Executes the instruction and returns 홀소리"
  [machine ins]
  (let [storage (current-storage machine)
        {명령 :첫, 홀소리 :가, 받침 :끝, 값 :값} (decode ins)]
    (if (= \ㅎ 명령) ;; 끝냄
      nil ;; halt
      (try
        (case 명령
          ; ㅇ 묶음
          \ㅇ "없음"
          ; ㄷ 묶음
          \ㄷ (셈하기 storage +)
          \ㄸ (셈하기 storage *)
          \ㅌ (셈하기 storage -)
          \ㄴ (셈하기 storage quot)
          \ㄹ (셈하기 storage mod)
          ; ㅁ 묶음
          \ㅁ (뽑기 storage 받침)
          \ㅂ (집어넣기 storage 받침 값)
          \ㅃ (중복 storage)
          \ㅍ (바꿔치기 storage)
          ; ㅅ 묶음
          \ㅅ (선택 machine 받침)
          \ㅆ (이동 machine 받침)
          (log/debug "몰라요😅" ins))
        홀소리
        (catch java.lang.IllegalStateException e
          ;; keep previous direction when operation failed
          \ㅐ)))))

(defn run [code]
  (loop [machine (gen-machine)]
    (let [cursor (:cursor machine)
          ins (get-in code [(:y cursor) (:x cursor)])]
      (if-let [홀소리 (exec! machine ins)]
        (recur (update machine :cursor move-cursor 홀소리))
        (끝냄 machine)))))
