(ns gmorpheme.aoc22.day13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [gmorpheme.aoc22 :refer [lines]]))

(defn read-pairs [lines]
  (->> lines
       (partition-by str/blank?)
       (remove #(= % [""]))
       (map (fn [group] (map edn/read-string group)))))

(defn read-all [lines]
  (->> lines
       (remove str/blank?)
       (map edn/read-string)))

(defn packet-compare [left right]
  (if (and (not (sequential? left))
           (not (sequential? right)))
    (compare left right)
    (let [[lx & lr] (if (sequential? left) left [left])
          [rx & rr] (if (sequential? right) right [right])
          first (packet-compare lx rx)]
      (if (not (zero? first))
        first
        (packet-compare lr rr)))))

(defn find-correct [lines]
  (->> lines
       (read-pairs)
       (map (partial apply packet-compare))
       (keep-indexed #(when (<= %2 0) (inc %1)))
       (apply +)))

(def divider-set #{[[2]] [[6]]})

(defn order-packets [lines]
  (->> lines
       (read-all)
       (concat divider-set)
       (sort packet-compare)
       (keep-indexed #(when (divider-set %2) (inc %1)))
       (apply *)))

(defn test-day13a []
  (find-correct (lines "day13-test.txt")))

(defn day13a []
  (find-correct (lines "day13.txt")))

(defn test-day13b []
  (order-packets (lines "day13-test.txt")))

(defn day13b []
  (order-packets (lines "day13.txt")))
