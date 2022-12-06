(ns gmorpheme.aoc22.day6
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))

(def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def test-input-2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn find-distinct [n input]
  (->> (partition n 1 input)
       (map frequencies)
       (map count)
       (take-while #(< % n))
       (count)
       (+ n)))

(def find-lock (partial find-distinct 4))
(def find-message (partial find-distinct 14))

(defn day6a [] (find-lock (first (lines "day6.txt"))))
(defn test-day6a [] (find-lock test-input))

(defn day6b [] (find-message (first (lines "day6.txt"))))
(defn test-day6b [] (find-message test-input-2))
