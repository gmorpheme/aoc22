(ns gmorpheme.aoc22.day3
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.set :as set]))

(def test-input ["vJrwpWtwJgWrhcsFMMfFFhFp"
                 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                 "PmmdzqPrVvPwwTWBwg"
                 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                 "ttgJtRGJQctTZtZT"
                 "CrZsJsPPZsGzwwsLwLmpwMDw"])

(defn compartments [rucksack]
  (let [half-count (/ (count rucksack) 2)]
    (partition half-count rucksack)))

(defn amazing-check [[left right]]
  (let [s (set right)]
    (->> (for [letter left]
           (s letter))
         (remove nil?)
         (first))))

(defn greg-check [[left right]]
  (first (set/intersection (set left) (set right))))

(defn to-priority [letter]
  (let [num (int letter)]
    (if (>= num 97)
      (- num 96)
      (- num 38))))

(defn do-day3a [lines]
  (->> lines
       (map compartments)
       (map amazing-check)
       (map to-priority)
       (apply +)))

(defn test-day3a []
  (do-day3a test-input))

(defn day3a []
  (do-day3a (lines "day3.txt")))

(defn find-badge [[a b c]]
  (first (set/intersection (set a) (set b) (set c))))

(defn do-day3b [lines]
  (->> lines
       (partition 3)
       (map find-badge)
       (map to-priority)
       (apply +)))

(defn test-day3b []
  (do-day3b test-input))

(defn day3b []
  (do-day3b (lines "day3.txt")))
