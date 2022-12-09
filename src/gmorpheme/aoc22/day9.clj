(ns gmorpheme.aoc22.day9
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]]))

(def test-input (str/split-lines "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"))

(def test-input-b (str/split-lines "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"))

(defrecord Point [x y])

(defn north [loc] (update loc :y inc))
(defn south [loc] (update loc :y dec))
(defn east [loc] (update loc :x inc))
(defn west [loc] (update loc :x dec))

(defn follow [{qx :x qy :y :as quarry} {:keys [x y] :as loc}]
  (if (and (< (abs (- qx x)) 2)
           (< (abs (- qy y)) 2))
    loc
    (cond->> loc
      (> qx x) (east)
      (< qx x) (west)
      (> qy y) (north)
      (< qy y) (south))))

(defn parse-command [line]
  (let [[_ dir dist] (re-matches #"(\w) (\d+)" line)
        n (parse-long dist)]
    (case dir
      "U" (repeat n (partial north))
      "D" (repeat n (partial south))
      "L" (repeat n (partial west))
      "R" (repeat n (partial east))
      (throw (ex-data {:foo :bar})))))

(defn read-commands [lines]
  (mapcat parse-command lines))

(defn trail [moves]
  (reductions #(follow %2 %1) (->Point 0 0) moves))

(defn do-day9a [lines]
  (->> lines
       read-commands
       (reductions #(%2 %1) (->Point 0 0))
       trail
       (into #{})
       count))

(defn do-day9b [lines]
  (->> lines
       read-commands
       (reductions #(%2 %1) (->Point 0 0))
       (iterate trail)
       (take 10)
       last
       (into #{})
       count))

(defn test-day9a []
  (do-day9a test-input))

(defn day9a []
  (do-day9a (lines "day9.txt")))

(defn test-day9b []
  (do-day9b test-input-b))

(defn day9b []
  (do-day9b (lines "day9.txt")))
