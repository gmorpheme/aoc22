(ns gmorpheme.aoc22.day15
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))

(def test-input (str/split-lines ""))

(defn do-day15a [lines])

(defn test-day15a []
  (do-day15a test-input))

(defn day15a []
  (do-day15a (lines "day15.txt")))

(defn do-day15b [lines])

(defn test-day15b []
  (do-day15b test-input))

(defn day15b []
  (do-day15b (lines "day15.txt")))
