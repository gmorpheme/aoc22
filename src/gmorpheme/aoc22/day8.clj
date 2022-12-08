(ns gmorpheme.aoc22.day8
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]]))

(def test-input (str/split-lines "30373
25512
65332
33549
35390"))

(defn parse-grid [lines]
  (apply vector
         (for [line lines]
           (mapv #(- (int %) 48) (vec line)))))

(defn transpose [rows]
  (apply map vector rows))

(defn reflect [rows]
  (map reverse rows))

(defn visible-over-threshold [threshold [x & xs]]
  "nil if below threshold or blocked"
  (when x
    (cons (if (< x threshold) nil x)
          (visible-over-threshold (max (inc x) threshold) (seq xs)))))

(defn visibles-in-row [row]
  (visible-over-threshold -1 row))

(defn visible-grid [rows]
  (map visibles-in-row rows))

(defn combine-rows [rows]
  (map #(first (filter some? %)) (transpose rows)))

(defn overlay-visibility-grids [& grids]
  (map combine-rows (transpose grids)))

(defn calculate-visible [grid]
  (let [grid-r (reflect grid)
        grid-t (transpose grid)
        grid-tr (reflect grid-t)
        vis (visible-grid grid)
        vis-r (reflect (visible-grid grid-r))
        vis-t (transpose (visible-grid grid-t))
        vis-tr (transpose (reflect (visible-grid grid-tr)))]
    (overlay-visibility-grids vis vis-r vis-t vis-tr)))

(defn count-trees [grid]
  (count (mapcat #(filter some? %) grid)))

(defn eastward-scores [[x & xs :as row]]
  (when x
    (let [blocking-tree-distance (first (keep-indexed
                                         #(and %2 (inc %1))
                                         (visible-over-threshold x xs)))]
        (cons (or blocking-tree-distance (count xs))
              (eastward-scores xs)))))

(defn eastward-scores-grid [rows]
  (map eastward-scores rows))

(defn combine-row-scores [rows]
  (map #(apply * (filter some? %)) (transpose rows)))

(defn overlay-score-grids [& grids]
  (map combine-row-scores (transpose grids)))

(defn calculate-scores [grid]
  (let [grid-r (reflect grid)
        grid-t (transpose grid)
        grid-tr (reflect grid-t)
        scores (eastward-scores-grid grid)
        scores-r (reflect (eastward-scores-grid grid-r))
        scores-t (transpose (eastward-scores-grid grid-t))
        scores-tr (transpose (reflect (eastward-scores-grid grid-tr)))]
    (overlay-score-grids scores scores-r scores-t scores-tr)))

(defn max-score [grid]
  (apply max (apply concat grid)))

(defn day8a []
  (count-trees (calculate-visible (parse-grid (lines "day8.txt")))))

(defn test-day8a []
  (count-trees (calculate-visible (parse-grid test-input))))

(defn day8b []
  (max-score (calculate-scores (parse-grid (lines "day8.txt")))))

(defn test-day8b []
  (max-score (calculate-scores (parse-grid test-input))))
