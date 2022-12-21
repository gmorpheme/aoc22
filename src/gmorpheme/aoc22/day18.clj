(ns gmorpheme.aoc22.day18
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def test-input (str/split-lines "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"))

(defn parse-coord [line]
  (vec (map parse-long (str/split line #","))))

(defn parse-input [lines]
  (map parse-coord lines))

(defn take-face [[surface [candidate candidate-faces]] neighbour-coord]
  (if-let [faces (get surface neighbour-coord)]
    (if (<= faces 1)
      [(dissoc surface neighbour-coord) [candidate (dec candidate-faces)]]
      [(update surface neighbour-coord dec) [candidate (dec candidate-faces)]])
    [surface [candidate candidate-faces]]))

(defn take-face-leave-volume [[surface [candidate candidate-faces]] neighbour-coord]
  (if-let [faces (get surface neighbour-coord)]
    [(update surface neighbour-coord dec) [candidate (dec candidate-faces)]]
    [surface [candidate candidate-faces]]))

(defn neighbours [[x y z]]
  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

(defn in-bounds [min-x min-y min-z max-x max-y max-z [x y z]]
  (and (<= min-x x (dec max-x))
       (<= min-y y (dec max-y))
       (<= min-z z (dec max-z))))

(defn add [surface candidate]
  (let [[surface [candidate faces-exposed]] (reduce take-face [surface [candidate 6]] (neighbours candidate))]
    (if (pos? faces-exposed)
      (assoc surface candidate faces-exposed)
      surface)))

(defn add-volume [surface candidate]
  (let [[surface [candidate faces-exposed]] (reduce take-face-leave-volume [surface [candidate 6]] (neighbours candidate))]
    (assoc surface candidate faces-exposed)))

(defn build-droplet [coords]
  (reduce add {} coords))

(defn surface-area [surface]
  (apply + (vals surface)))

(defn bounds [droplet]
  (let [lava-set (into #{} (keys droplet))
        xs (map #(get % 0) lava-set)
        ys (map #(get % 1) lava-set)
        zs (map #(get % 2) lava-set)
        min-x (apply min xs)
        min-y (apply min ys)
        min-z (apply min zs)
        max-x (inc (apply max xs))
        max-y (inc (apply max ys))
        max-z (inc (apply max zs))]
    [min-x min-y min-z max-x max-y max-z]))

(defn test-day18a []
  (surface-area (build-droplet (parse-input test-input))))

(defn day18a []
  (surface-area (build-droplet (parse-input (lines "day18.txt")))))

(defn remove-boundary-surfaces [volume [min-x min-y min-z max-x max-y max-z]]
  (into {}
        (map (fn [[[x y z] v]]
               [[x y z] (cond-> v
                          (= x min-x) dec
                          (= y min-y) dec
                          (= z min-z) dec
                          (= x (dec max-x)) dec
                          (= y (dec max-y)) dec
                          (= z (dec max-z)) dec)]))
        volume))

(defn flood [droplet]
  (let [lava-set (into #{} (keys droplet))
        [min-x min-y min-z max-x max-y max-z] (bounds droplet)
        min-x (dec min-x)
        min-y (dec min-y)
        min-z (dec min-z)
        max-x (inc max-x)
        max-y (inc max-y)
        max-z (inc max-z)]
    (loop [water {}
           queue #{[min-x min-y min-z]}]
      (let [[x & xs] (remove water queue)]
        (if (some? x)
          (let [water (add-volume water x)
                gaps (->> (neighbours x)
                          (filter (partial in-bounds min-x min-y min-z max-x max-y max-z))
                          (remove lava-set)
                          (remove water))]
            (recur water (set/union (set xs) (set gaps))))
          (remove-boundary-surfaces
           water
           [min-x min-y min-z max-x max-y max-z]))))))

(defn test-day18a []
  (surface-area (flood (build-droplet (parse-input test-input)))))

(defn day18a []
  (surface-area (flood (build-droplet (parse-input (lines "day18.txt"))))))
