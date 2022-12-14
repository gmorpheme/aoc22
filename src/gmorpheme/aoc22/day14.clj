(ns gmorpheme.aoc22.day14
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]
            [clojure.set :as set]))


(def test-input (str/split-lines "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"))

(defn parse-coords [s]
  (vec (map parse-long (str/split s #","))))

(defn parse-rock-structures [lines]
  (map (comp (partial map parse-coords) #(str/split % #" -> ")) lines))

(defn bounds [structures]
  (let [[min-x max-x max-y] (reduce
                             (fn [[min-x max-x max-y] [x y]]
                               [(min min-x x) (max max-x x) (max max-y y)])
                             [500 500 0]
                             (apply concat structures))]
    {:min-x (dec min-x)
     :max-x (inc max-x)
     :max-y max-y}))

(defn structure->set [corners]
  (let [fill (fn [l r]
               (let [[[x0 y0] [x1 y1]] (sort [l r])]
                 (if (= x0 x1)
                   (map #(vector x0 %) (range y0 (inc y1)))
                   (map #(vector % y0) (range x0 (inc x1))))))]
    (into #{}
          (mapcat (partial apply fill))
          (partition 2 1 corners))))

(defn rock-set [structures]
  (apply set/union (map structure->set structures)))

(defrecord Grid [cols start-column min-x max-x max-y exit-column])

(defn make-grid [{:keys [min-x max-x max-y]} rock?]
  (->Grid
   (apply vector
          (for [x (range min-x (inc max-x))]
            (mapv #(when (rock? [x %]) :rock) (range 0 (inc max-y)))))
   500
   min-x
   max-x
   max-y
   nil))

(defn get-at [{:keys [cols min-x] :as grid} [x y]]
  (get-in cols [(- x min-x) y]))

(defn set-at [{:keys [cols min-x] :as grid} [x y] value]
  (update grid :cols assoc-in [(- x min-x) y] value))

(defn count-grains [{:keys [cols] :as grid}]
  (reduce + (map #(count (keep #{:sand} %)) cols)))

(defn drop-sand
  "Simulate falling sand and return next grid or nil if blocked.
  Sets :exit-column if the sand exits."
  [{:keys [start-column max-y] :as grid}]
  (when (nil? (get-at grid [start-column 0]))
    (loop [x start-column y 0]
      (if (= y max-y)
        (assoc grid :exit-column x)
        (if (nil? (get-at grid [x (inc y)]))
          (recur x (inc y))
          (if (nil? (get-at grid [(dec x) (inc y)]))
            (recur (dec x) (inc y))
            (if (nil? (get-at grid [(inc x) (inc y)]))
              (recur (inc x) (inc y))
              (set-at grid [x y] :sand))))))))

(defn dump-grid [{:keys [cols max-y] :as grid}]
  (letfn [(to-str [val] (case val :rock \# :sand \o \.))]
    (doseq [y (range 0 (inc max-y))]
      (println (apply str (map #(to-str (nth % y)) cols))))))

(defn prepare-grid [lines]
  (let [structures (parse-rock-structures lines)
        shape (bounds structures)
        rocks (rock-set structures)
        grid (make-grid shape rocks)]
    grid))

(defn run-simulation [grid]
  (first (drop-while #(nil? (:exit-column %)) (iterate drop-sand grid))))

(defn do-day14a [lines]
  (count-grains (run-simulation (prepare-grid lines))))

(defn test-day14a []
  (do-day14a test-input))

(defn day14a []
  (do-day14a (lines "day14.txt")))

(defn prepare-grid-b [lines]
  (let [structures (parse-rock-structures lines)
        shape (update (bounds structures) :max-y #(+ % 2))
        shape (-> shape
                  (update :min-x #(- % (:max-y shape))) ; lazy...!
                  (update :max-x #(+ % (:max-y shape))))
        rocks (rock-set structures)
        rock? (fn [[x y :as loc]] (or (rocks loc) (= y (:max-y shape))))
        grid (make-grid shape rock?)]
    grid))

(defn run-sim-b [grid]
  (last (take-while some? (iterate drop-sand grid))))

(defn do-day14b [lines]
  (count-grains (run-sim-b (prepare-grid-b lines))))

(defn test-day14b []
  (do-day14b test-input))

(defn day14b []
  (do-day14b (lines "day14.txt")))
