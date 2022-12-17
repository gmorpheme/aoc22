(ns gmorpheme.aoc22.day17
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.set :as set]))

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defprotocol Bounded
  (bounding-box [self]))

(defprotocol HasTemplate
  (template [self]))

(defprotocol RockSet
  (rock-set [self]))

(defprotocol Rocky
  (has-rock? [self coord]))

(defn apply-template [[x0 y0] template]
  (into #{}
        (map (fn [[x y]] [(+ x x0) (+ y y0)]))
        template))

(defrecord Horizontal [x y]
  Bounded
  (bounding-box [_] [x  y (+ 4 x) (inc y)])

  HasTemplate
  (template [_] #{[0 0] [1 0] [2 0] [3 0]})

  RockSet
  (rock-set [self] (apply-template [x y] (template self)))

  Rocky
  (has-rock? [self coord] ((rock-set self) coord)))

(defrecord Cross [x y]
  Bounded
  (bounding-box [_] [x y (+ 3 x) (+ 3 y)])

  HasTemplate
  (template [_] #{[1 0] [0 1] [1 1] [1 2] [2 1]})

  RockSet
  (rock-set [self] (apply-template [x y] (template self)))

  Rocky
  (has-rock? [self coord] ((rock-set self) coord)))

(defrecord Elbow [x y]
  Bounded
  (bounding-box [_] [x y (+ 3 x) (+ 3 y)])

  HasTemplate
  (template [_] #{[0 0] [1 0] [2 0] [2 1] [2 2]})

  RockSet
  (rock-set [self] (apply-template [x y] (template self)))

  Rocky
  (has-rock? [self coord] ((rock-set self) coord)))

(defrecord Vertical [x y]
  Bounded
  (bounding-box [_] [x y (inc x) (+ 4 y)])

  HasTemplate
  (template [_] #{[0 0] [0 1] [0 2] [0 3]})

  RockSet
  (rock-set [self] (apply-template [x y] (template self)))

  Rocky
  (has-rock? [self coord] ((rock-set self) coord)))

(defrecord Square [x y]
  Bounded
  (bounding-box [_] [x y (+ 2 x) (+ 2 y)])

  HasTemplate
  (template [_] #{[0 0] [0 1] [1 0] [1 1]})

  RockSet
  (rock-set [self] (apply-template [x y] (template self)))

  Rocky
  (has-rock? [self coord] ((rock-set self) coord)))

(defrecord Wall [x]
  Bounded
  (bounding-box [_] [x nil (inc x) nil])

  Rocky
  (has-rock? [self [x-other _]] (= x x-other)))

(defrecord Floor [y]
  Bounded
  (bounding-box [_] [nil y nil (inc y)])

  Rocky
  (has-rock? [self [_ y-other]] (= y y-other)))

(def min-x #(nth % 0))
(def min-y #(nth % 1))
(def max-x #(nth % 2))
(def max-y #(nth % 3))

(extend-type clojure.lang.PersistentVector
  Bounded
  (bounding-box [self] self))

(defn loftiness-order [shape-1 shape-2]
  (compare (max-y (bounding-box shape-1))
           (max-y (bounding-box shape-2))))

(defn origin-order [shape-1 shape-2]
  (let [[x0 y0 _ _] (bounding-box shape-1)
        [x1 y1 _ _] (bounding-box shape-2)]
    (compare [x0 y0] [x1 y1])))

(defn bounding-box-intersection [shape-1 shape-2]
  (let [[minx1 miny1 maxx1 maxy1] (bounding-box shape-1)
        [minx2 miny2 maxx2 maxy2] (bounding-box shape-2)
        right-collision (or (nil? minx1) (nil? minx2) (< minx2 maxx1 (inc maxx2)))
        left-collision (or (nil? minx1) (nil? minx2) (< minx1 maxx2 (inc maxx1)))
        bottom-collision (or (nil? miny1) (nil? miny2) (< miny1 maxy2 (inc maxy1)))
        top-collision (or (nil? miny1) (nil? miny2) (< miny2 maxy1 (inc maxy2)))]
    (and (or right-collision
             left-collision)
         (or bottom-collision
             top-collision))))

(comment
  (assert (bounding-box-intersection (->Square 0 0) (->Square 1 0)))
  (assert (bounding-box-intersection (->Square 0 0) (->Square 0 1)))
  (assert (bounding-box-intersection (->Square 0 0) (->Square -1 0)))
  (assert (bounding-box-intersection (->Square 0 0) (->Square 0 -1)))
  (assert (not (bounding-box-intersection (->Square 0 0) (->Square 2 0))))
  (assert (not (bounding-box-intersection (->Square 0 0) (->Square 0 2))))
  (assert (not (bounding-box-intersection (->Square 0 0) (->Square -2 0))))
  (assert (not (bounding-box-intersection (->Square 0 0) (->Square 0 -2)))))

(defn rock-set-intersection [shape-1 shape-2]
  (set/intersection (rock-set shape-1) (rock-set shape-2)))

(defn intersects? [shape-1 shape-2]
  (if (and (extends? RockSet (type shape-1))
           (extends? RockSet (type shape-2)))
    (not (empty? (rock-set-intersection shape-1 shape-2)))
    (bounding-box-intersection shape-1 shape-2)))

(defrecord Chamber
    [left-wall
     right-wall
     floor
     rock-stack
     falling-shape])                       ; ordered by heighest point

(defn configure-chamber []
  (->Chamber
   (->Wall 0)
   (->Wall 8)
   (->Floor 0)
   (sorted-set-by (fn [l r] (let [lo (- (loftiness-order l r))]
                             (if (zero? lo)
                               (let [oo (- (origin-order l r))]
                                 (if (zero? oo)
                                   (throw (ex-info "should never compare equal" {:lt (type l) :l l :rt (type r) :r r}))
                                   oo))
                               lo))))
   nil))

(defn depth [{:keys [rock-stack floor]}]
  (if (empty? rock-stack)
    (inc (:y floor))
    (nth (bounding-box (first rock-stack)) 3)))

(defn place-shape [{:keys [left-wall] :as chamber} shape-fn]
  (assoc chamber :falling-shape (shape-fn
                                 (+ 3 (:x left-wall))
                                 (+ 3 (depth chamber)))))

(defn relevant-shapes [rock-stack shape]
  (->> rock-stack
       (take-while #(< (abs (- (:y %) (or (:y shape) (second shape)))) 50))
       (filter #(bounding-box-intersection shape %))))

(defn fall [{:keys [floor rock-stack falling-shape] :as chamber}]
  (let [projected (update falling-shape :y dec)
        relevant-shapes (concat [floor] (relevant-shapes rock-stack projected))
        collision? (some (fn [shape] (intersects? shape projected)) relevant-shapes)]
    (when-not collision?
      (assoc chamber :falling-shape projected))))

(defn jet [{:keys [left-wall right-wall rock-stack falling-shape] :as chamber} x-perturbation-fn]
  (let [projected (update falling-shape :x x-perturbation-fn)
        relevant-shapes (concat [left-wall right-wall] (relevant-shapes rock-stack projected))
        collision? (some (fn [shape] (intersects? shape projected)) relevant-shapes)]
    (when-not collision?
      (assoc chamber :falling-shape projected))))

(defn fix [{:keys [falling-shape rock-stack] :as chamber}]
  (assoc chamber
         :falling-shape nil
         :rock-stack (conj rock-stack falling-shape)))

(defrecord State [chamber jet-stream shape-stream])

(defn initialise [{:keys [chamber jet-stream shape-stream] :as state}]
  (->State (place-shape chamber (first shape-stream))
           jet-stream
           (rest shape-stream)))

(defn step
  [{:keys [chamber jet-stream shape-stream]}]
  (if (zero? (mod (count (:rock-stack chamber)) 100000))
    (println (depth chamber)))
  (let [chamber (or (jet chamber (if (= (first jet-stream) \<) dec inc)) chamber)
        jet-stream (rest jet-stream)]
    (if-let [fallen (fall chamber)]
      (->State fallen jet-stream shape-stream)
      (let [chamber (place-shape (fix chamber) (first shape-stream))]
        (->State chamber jet-stream (rest shape-stream))))))

(defn drop-rocks [input n]
  (let [state (initialise (->State (configure-chamber)
                                   (cycle (seq input))
                                   (cycle [->Horizontal ->Cross ->Elbow ->Vertical ->Square])))]
    (->> (iterate step state)
         (drop-while (fn [s]  (< (count (get-in s [:chamber :rock-stack])) n)))
         (first))))

(defn test-day17a []
  (let [end-state (drop-rocks test-input 2022)]
    (dec (depth (:chamber end-state))))) ; (for the floor)

(defn day17a []
  (let [end-state (drop-rocks (first (lines "day17.txt")) 2022)]
    (dec (depth (:chamber end-state)))))

(defrecord Cycle [start period])

(defrecord CycleDetector [jet-stream-modulus signature-store last-step-check])

(defn make-detector [jet-stream-modulus]
  (->CycleDetector jet-stream-modulus {} nil)) ; would love a bloom filter

(defn signature [{:keys [chamber jet-stream shape-stream] :as state} step-count jet-stream-modulus]
  (let [depth (depth chamber)
        shapes (cons (:falling-shape chamber) (take 10 (:rock-stack chamber)))
        y-independent (map #(update % :x (- % depth)) shapes)]
    (hash [shapes (mod step-count jet-stream-modulus)])))

(defn seen-state
  ;; return vec of new detector state and cycle or nil
  [{:keys [jet-stream-modulus signature-store last-step-check] :as detector} state step-count]
  (let [sig (signature state step-count jet-stream-modulus)]
    (if-let [prev-step-count (get signature-store sig)]
      (let [potential-cycle (->Cycle prev-step-count (- step-count prev-step-count))]
        (if (and last-step-check
                 #_(= (:period last-step-check) (:period potential-cycle))
                 (= (:start last-step-check) (dec (:start potential-cycle))))
          ;; cycle confirmed
          [detector last-step-check]
          ;; cycle tentative
          [(assoc detector
                  :last-step-check potential-cycle
                  :signature-store (assoc signature-store sig step-count)) nil]))
      ;; no cycle
      [(assoc detector
              :last-step-check nil
              :signature-store (assoc signature-store sig step-count))
       nil])))

(defn cycle-checking-step
  [{:keys [chamber jet-stream shape-stream] :as state} detector step-count]
  (let [[detector cycle] (seen-state detector state step-count)]
    [(step state) detector cycle]))

(defn state-after [input step-count]
  (let [state (initialise (->State (configure-chamber)
                                   (cycle (seq input))
                                   (cycle [->Horizontal ->Cross ->Elbow ->Vertical ->Square])))]
    (->> (iterate step state)
         (drop step-count)
         (first))))

(defn run-cycle-checking [input-line target]
  (loop [state (initialise (->State (configure-chamber)
                                    (cycle (seq input-line))
                                    (cycle [->Horizontal ->Cross ->Elbow ->Vertical ->Square])))
         detector (make-detector (count input-line))
         step-count 0]
    (let [[new-state new-detector cycle] (cycle-checking-step state detector step-count)]
      (if cycle
        cycle
        (recur new-state new-detector (inc step-count))))))

(defn test-day17b []
  (run-cycle-checking test-input 1000000000000)) ; (for the floor)

(defn day17a []
  )

(defn render-y [{:keys [floor left-wall right-wall rock-stack falling-shape] :as chamber} y]
  (let [row-box [(:x left-wall) y (inc (:x right-wall)) (inc y)]
        xs (range (:x left-wall) (inc (:x right-wall)))
        relevant-shapes (concat [left-wall right-wall floor] (relevant-shapes rock-stack row-box))
        fixed-rock? (fn [coord] (not (every? (fn [shape] (not (has-rock? shape coord))) relevant-shapes)))
        falling-rock? (fn [coord] (and (some? falling-shape) (has-rock? falling-shape coord)))]
    (apply str
           (for [x xs]
             (cond
               (fixed-rock? [x y]) "#"
               (falling-rock? [x y]) "@"
               :else ".")))))

(defn dump-chamber [{:keys [left-wall right-wall floor rock-stack falling-shape] :as chamber}]
  (let [top-y (or (and (some? falling-shape) (nth (bounding-box falling-shape) 3))
                  (depth chamber))]
    (doseq [y (range (inc top-y) (max (dec (:y floor)) (- top-y 20)) -1)]
      (println (render-y chamber y)))))

(defn dump-state [state]
  (dump-chamber (:chamber state))
  (println "Next Jet: " (first (:jet-stream state))))
