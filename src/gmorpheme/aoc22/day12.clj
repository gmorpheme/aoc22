(ns gmorpheme.aoc22.day12
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]])
  (:import [java.util PriorityQueue]
           [java.util.function Predicate]))

(set! *warn-on-reflection* true)

(def test-input (str/split-lines "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"))

(defn index-of [coll item]
  (first (keep-indexed #(when (= %2 item) %1) coll)))

(defn manhattan-distance [[lx ly] [rx ry]]
  (+ (abs (- rx lx)) (abs (- ry ly))))

;;
;; The grid, together with start and end points and dimensions
;;

(defrecord Grid [rows start end height width size])

(def empty-grid (->Grid [] nil nil 0 0 0))

(defn add-row [grid [i row]]
  (let [start (index-of row \S)
        end (index-of row \E)
        row (into-array (map #(or ({\S 0 \E 26} %) (- (int %) 97)) (seq row)))
        width (count row)
        height (inc i)]
    (-> grid
        (update :rows #(conj % row))
        (assoc :width width :height height :size (* width height))
        (cond->
          start (assoc :start [start i])
          end (assoc :end [end i])))))

(defn parse-grid [lines]
  (let [grid (->> lines
                  (map-indexed vector)
                  (reduce add-row empty-grid))]
    (update grid :rows into-array)))

(defn height ^long [{:keys [rows] :as grid} [x y]]
  (aget rows y x))

(defn neighbours [{:keys [height width] :as grid} [x y]]
  (remove
   (fn [[x y]] (or (neg? x) (neg? y) (>= x width) (>= y height)))
   [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(defn start-candidates [{:keys [rows] :as grid}]
  (apply concat (for [[i row] (map-indexed vector rows)]
                  (->> row
                       (keep-indexed #(when (zero? %2) %1))
                       (map #(vector % i))))))

;;
;; The A* fringe of options that advances towards the solution
;;

(defrecord ExtensionOption [path cost heuristic])

(defn better
  "A cheapness comparison on ExtensionOption"
  [{lc :cost lh :heuristic} {rc :cost rh :heuristic}]
  (< (+ lc lh) (+ rc rh)))

(defn init-fringe
  "Create a (mutable) heap for tracking the fringe"
  ^PriorityQueue
  [grid]
  (PriorityQueue. (* (:size grid) (:size grid)) better))

(defn add-or-replace!
  "Update the fringe on generation of new options"
  [^PriorityQueue fringe options]
  (doseq [option options]
    (let [superseded (reify Predicate (test [_ {:keys [path cost]}]
                                        (and (= (last path) (last (:path option)))
                                             (>= cost (:cost option)))))]
      (.removeIf fringe superseded)
      (.add fringe option))))

;;
;; A grid for tracking best costs for each location to avoid rework
;;

(defn best-cost-grid [{:keys [width height] :as grid}]
  (vec (repeat height (vec (repeat width nil)))))

(defn track-cost [cost-grid [x y] cost]
  (update-in cost-grid [y x] (fnil #(min cost %) Integer/MAX_VALUE)))

(defn best-cost [cost-grid [x y]]
  (get-in cost-grid [y x]))

(defn dump-cost-grid [cost-grid]
  (doseq [row cost-grid]
    (println (apply str (for [item row]
                          (if (nil? item)
                            "  ."
                            (format "%3d" item)))))))

(defn beats-best-cost [cost-grid loc cost]
  (if-let [prev (best-cost cost-grid loc)]
    (< cost prev)
    true))

;;
;; The main A* stuff
;;

(defn options
  "Generate the options available to extend path (and cost them)"
  [grid path cost-grid]
  (let [loc (last path)
        h (height grid loc)
        climb (fn [loc] (- (height grid loc) h))
        expended-cost (count path)
        new-cost (constantly (inc expended-cost))
        heuristic (fn [opt] (min (- 26 (height grid opt))
                                (manhattan-distance loc opt)))]
    (sequence
     (comp
      (filter #(<= (climb %) 1))
      (remove (into #{} path))
      (filter #(beats-best-cost cost-grid % (new-cost %)))
      (map (juxt #(conj path %) new-cost heuristic))
      (map (partial apply ->ExtensionOption)))
     (neighbours grid loc))))

(defn a-star
  "Run the A* algorithm - return path or nil for no route"
  [grid]
  (let [initial-path [(:start grid)]
        fringe (init-fringe grid)
        cost-grid (best-cost-grid grid)] ; mutable java queue!
    (loop [path initial-path
           n 0
           cost-grid cost-grid]
      (do

        ;; to visualise...
        (comment
          (when (zero? (mod n 1))
            (println n (.size fringe) (count path))
            (dump-cost-grid cost-grid)))

        (let [extensions (options grid path cost-grid)]
          (add-or-replace! fringe extensions))

        (when-let [{:keys [path cost] :as selected-option} (.poll fringe)]
          (if (= (:end grid) (last path))
            path
            (recur path (inc n) (track-cost cost-grid (last path) cost))))))))

(defn find-path [grid]
  (a-star grid))

(defn step-count [path]
  (dec (count path)))

(defn test-day12a []
  (step-count (find-path (parse-grid test-input))))

(defn day12a []
  (step-count (find-path (parse-grid (lines "day12.txt")))))

(defn route-lengths [grid]
  (sequence (comp
             (map #(find-path (assoc grid :start %)))
             (remove nil?)
             (map step-count))
            (start-candidates grid)))

(defn test-day12b []
  (apply min (route-lengths (parse-grid test-input))))

(defn day12b []
  (apply min (route-lengths (parse-grid (lines "day12.txt")))))
