(ns gmorpheme.aoc22.day2
  (:require [gmorpheme.aoc22 :refer [lines]]))

(defn sign [n]
  (case n
    0 :rock
    1 :paper
    2 :scissors))

(defn result [[l r]]
  (case (mod (- r l) 3)
    0 :=
    1 :r
    2 :l))

(defn score-round [[_ me :as round]]
  (+ (case (sign me)
       :rock 1
       :paper 2
       :scissors 3)
     (case (result round)
       :l 0
       := 3
       :r 6)))

(defn read-rounds [lines]
  (map (fn [s] [(- (int (first s)) 65) (- (int (nth s 2)) 88)]) lines))

(defn tactic [n]
  (case n
    0 :lose
    1 :draw
    2 :win))

(defn score-round-b [[elf t :as round]]
  (let [me (mod (case (tactic t)
                   :win (inc elf)
                   :draw elf
                   :lose (dec elf)) 3)]
    (score-round [elf me])))

(defn day2 [text-lines score-fn]
  (->> text-lines
       (read-rounds)
       (map score-fn)
       (apply +)))

(defn test-day2 []
  (let [test-input ["A Y" "B X" "C Z"]]
    (assert (= (day2 test-input score-round) 15))
    (assert (= (day2 test-input score-round-b) 12))
    :ok))

(defn day2a []
  (day2 (lines "day2.txt") score-round))

(defn day2b []
  (day2 (lines "day2.txt") score-round-b))
