(ns gmorpheme.aoc22.day10
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]]))

(defn parse-instruction-a [line]
  (if-let [[_ n] (re-matches #"addx (.+)$" line)]
    [:addx (parse-long n)]
    [:noop]))

(defn cycle-impacts [[i n :as instruction]]
  (if (= i :addx)
    [identity, #(+ % n)]
    [identity]))

(defn do-day10a [lines]
  (->> lines
       (map parse-instruction-a)
       (mapcat cycle-impacts)
       (reductions #(%2 %1) 1)
       (map vector (map inc (range)))
       (drop 19)
       (take-nth 40)
       (map (partial apply *))
       (apply +)))

(defn test-day10a []
  (do-day10a (lines "day10-test.txt")))

(defn day10a []
  (do-day10a (lines "day10.txt")))

(defn visible [cycle x]
  (let [crt-xpos (mod (dec cycle) 40)]
    (< (abs (- crt-xpos x)) 2)))

(defn do-day10b [lines]
  (->> lines
       (map parse-instruction-a)
       (mapcat cycle-impacts)
       (reductions #(%2 %1) 1)
       (map vector (map inc (range)))
       (map (partial apply visible))
       (render)))

(defn render [xs]
  (doseq [row (partition 40 xs)]
    (println (apply str (map {true \# false \.} row)))))

(defn test-day10b []
  (do-day10b (lines "day10-test.txt")))

(defn day10b []
  (do-day10b (lines "day10.txt")))
