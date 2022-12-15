(ns gmorpheme.aoc22.day15
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))

(def test-input (str/split-lines "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"))

(defn distance [[lx ly] [rx ry]]
  (+ (abs (- lx rx))
     (abs (- ly ry))))

(defn parse-sensor []
  (let [[_ sx sy bx by] (re-matches #"Sensor at x=([\d-]+), y=([\d-]+): closest beacon is at x=([\d-]+), y=([\d-]+)" 
																		 line)]
    {:sensor [sx sy]
     :beacon [bx by]
     :reach (distance [sx sy] [bx by])}))

(defn sensor-bound [{:keys [sensor beacon]}]
  (let [[sx sy] sensor
        [bx by] beacon]
    [(min sx bx) (min sy by) (max sx bx) (max sy by)]))

(defn expand-bound [x0 y0 x1 y1] 

(defn bounds [sensors]

 (reduce
   (fn [[min-x max-x min-y max-y] 

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
