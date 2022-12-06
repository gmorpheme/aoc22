(ns gmorpheme.aoc22.day4
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))

(def test-input (str/split-lines "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"))

(def ranges-regex #"(\d+)-(\d+),(\d+)-(\d+)")

(defn read-ranges [line]
  (let [[_ a-left a-right b-left b-right] (map parse-long (re-matches ranges-regex line))]
    [[a-left a-right] [b-left b-right]]))

(defn non-containment [[a-left a-right] [b-left b-right]]
  (or (and (< a-left b-left) (< a-right b-right))
      (and (> a-left b-left) (> a-right b-right))))

(def full-containment (complement non-containment))

(comment
  ;; was beautiful in my head but this one is just silly
  (defn mathsy-full-containment [[al ar] [bl br]]
    (let [sign (fn [n] (/ n (abs n)))]
      (not= 2 (abs (+ (sign (compare al ar))
                      (sign (compare bl br))))))))

(defn disjoint [[a-left a-right] [b-left b-right]]
  (or (< a-right b-left)
      (< b-right a-left)))

(def overlapping (complement disjoint))

(defn do-day4a [lines]
  (->> lines
       (map read-ranges)
       (map #(apply full-containment %))
       (filter identity)
       (count)))

(defn do-day4b [lines]
  (->> lines
       (map read-ranges)
       (map #(apply overlapping %))
       (filter identity)
       (count)))

(defn day4a []
  (do-day4 (lines "day4.txt")))

(defn test-day4a []
  (do-day4 test-input))

(defn day4b []
  (do-day4b (lines "day4.txt")))

(defn test-day4b []
  (do-day4b test-input))
