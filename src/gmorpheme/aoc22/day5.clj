(ns gmorpheme.aoc22.day5
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def test-input (str/split-lines "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"))

(def inst-regex #"move (\d+) from (\d+) to (\d+)")

(defn parse-instructions [instructions]
  (for [inst instructions]
    (into [] (map parse-long) (rest (re-matches inst-regex inst)))))

(defn parse-stacks [stack-lines]
  (into []
        (let [width (apply max (map count stack-lines))
              indices (map #(inc (* 4 %)) (range))]
          (for [index indices :while (< index width)]
            (sequence
             (comp (map #(get % index))
                   (filter #(and (some? %) (Character/isUpperCase (int %)))))
             stack-lines)))))

(defn parse-puzzle [lines]
  (let [[stacks instructions] (remove #(= % [""]) (partition-by str/blank? lines))]
    [(parse-stacks stacks) (parse-instructions instructions)]))

(defn apply-instruction [pop-xform stacks [n from to]]
  (let [from (dec from)
        to (dec to)
        [popped new-from] (split-at n (nth stacks from))
        new-to (concat (pop-xform popped) (nth stacks to))]
    (assoc stacks
           from new-from
           to new-to)))

(def apply-instruction-9000 (partial apply-instruction reverse))
(def apply-instruction-9001 (partial apply-instruction identity))

(defn do-day5 [lines instruction-impl]
  (let [[stacks instructions] (parse-puzzle lines)]
    (->> instructions
         (reduce instruction-impl stacks)
         (map first)
         (apply str))))

(defn day5a [] (do-day5 (lines "day5.txt") apply-instruction-9000))
(defn test-day5a [] (do-day5 test-input) apply-instruction-9000)
(defn day5b [] (do-day5 (lines "day5.txt") apply-instruction-9001))
(defn test-day5b [] (do-day5 test-input apply-instruction-9001))
