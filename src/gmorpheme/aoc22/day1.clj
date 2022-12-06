(ns gmorpheme.aoc22.day1
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))

(defn read-calories [lines]
  (->> lines
       (partition-by str/blank?)
       (remove #(= % [""]))
       (map (fn [group] (map parse-long group)))))

(defn max-elf [elves]
  (apply max (map #(apply + %) elves)))

(defn topn-elves [elves n]
  (let [sums (sort-by - (map #(apply + %) elves))]
    ;; nb complete sort not actually required - jdk PriorityQueue perhaps?
    (apply + (take n sums))))

(defn day1a []
  (-> (lines "day1.txt")
      (read-calories)
      (max-elf)))

(defn day1b []
  (-> (lines "day1.txt")
      (read-calories)
      (topn-elves 3)))
