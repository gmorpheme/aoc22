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

(defn parse-sensor [line]
  (let [[sx sy bx by]
        (map parse-long (rest (re-matches #".*at x=([\d-]+), y=([\d-]+):.*at x=([\d-]+), y=([\d-]+)" line)))]
    {:sensor [sx sy]
     :beacon [bx by]
     :reach (distance [sx sy] [bx by])}))

(defn parse-sensors [lines]
  (map parse-sensor lines))

(defn sensor-bound [{:keys [sensor beacon]}]
  (let [[sx sy] sensor
        [bx by] beacon]
    [(min sx bx) (min sy by) (max sx bx) (max sy by)]))

(defn sensor-relevant [{:keys [sensor reach]} y]
  (<= (abs (- (second sensor) y)) reach))

(defn expand-bound [[lx0 ly0 lx1 ly1] [rx0 ry0 rx1 ry1]]
  [(min lx0 rx0) (min ly0 ry0) (max lx1 rx1) (max ly1 ry1)])

(defn bounds [sensors]
  (reduce expand-bound (map sensor-bound sensors)))

(defn sensor-y-intersection
  "The start an end (inclusive) where sensor coverage intersects y"
  [{:keys [sensor reach]} y]
  (let [[sx sy] sensor
        y-dist (abs (- y sy))
        x-reach (- reach y-dist)
        start (- sx x-reach)
        end (+ sx x-reach)]
    (when (pos? x-reach) [start end])))

(defn combine-intersects [[s0 e0] [s1 e1]]
  (if (and (<= s0 s1) (<= s1 e0))
    [[s0 (max e0 e1)]]
    [[s0 e0] [s1 e1]]))

(defn condense-intersects [intersects]
  (let [intersects (vec (sort intersects))]
    (reduce
     (fn [is i] (concat (butlast is) (combine-intersects (last is) i)))
     [(first intersects)]
     (rest intersects))))

(defn count-members [[s e]]
  (inc (- e s)))

(defn beacons-on-y [sensors y]
  (->> sensors
       (filter (fn [{:keys [beacon]}] (= y (second beacon))))
       (map :beacon)
       (into #{})
       (count)))

(defn do-day15a [lines y]
  (let [sensors (parse-sensors lines)
        relevant-sensors (filter #(sensor-relevant % y) sensors)
        intersects (remove nil? (map #(sensor-y-intersection % y) relevant-sensors))
        condensed (condense-intersects intersects)
        covered (apply + (map count-members condensed))
        beacons (beacons-on-y sensors y)]
    (- covered beacons)))



(defn test-day15a []
  (do-day15a test-input 10))

(defn day15a []
  (do-day15a (lines "day15.txt") 2000000))

(defn bound-intersects [intersects [min-x max-x]]
  (map (fn [[s e]] [(max s min-x) (min e max-x)])
       intersects))

(defn find-beacon [lines [min-x min-y max-x max-y]]
  (first
   (remove nil?
           (for [y (range min-y (inc max-y))]
             (let [sensors (parse-sensors lines)
                   relevant-sensors (filter #(sensor-relevant % y) sensors)
                   intersects (remove nil? (map #(sensor-y-intersection % y) relevant-sensors))
                   condensed (bound-intersects (condense-intersects intersects) [min-x max-x])
                   covered (apply + (map count-members condensed))
                   total (inc (- max-x min-x))]
               (when (pos? (- total covered))
                 (let [[[_ l] [h _]] condensed]
                   [(inc l) y])))))))

(defn frequency [[x y]]
  (+ (* 4000000 x) y))

(defn test-day15b []
  (frequency (find-beacon test-input [0 0 20 20])) )

(defn day15b []
  (frequency (find-beacon (lines "day15.txt") [0 0 4000000 4000000])))
