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

(defn find-depths [input n m]
  (loop [state (initialise (->State (configure-chamber)
                                    (cycle (seq input))
                                    (cycle [->Horizontal ->Cross ->Elbow ->Vertical ->Square])))
         depths []]
    (let [prev-rocks (count (:rock-stack (:chamber state)))
          new-state (step state)
          new-rocks (count (:rock-stack (:chamber new-state)))]
      (if (>= prev-rocks n)
        depths
        (let [new-depth (dec (depth (:chamber new-state)))]
          (if (and (> new-rocks prev-rocks) (zero? (mod new-rocks m)))
            (do (println new-depth (- new-depth (or (last depths) 0)))
                (recur new-state (conj depths new-depth)))
            (recur new-state depths)))))))

(defn test-day17b []
  (let [depths (find-depths test-input 2000000 100000)
        increments (map (fn [[l r]] (- r l)) (partition 2 depths))]
    increments)) ; (for the floor)

;; [151434 302861 454288 605720 757147 908577 1060007 1211434 1362861 1514288]

(defn detect-cycle [increments increment-rocks])

(def test-increments [151427
                      151432
                      151430
                      151427
                      151427
                      151427
                      151430
                      151427
                      151432
                      151430
                      151427
                      151427
                      151427
                      151430
                      151427
                      151432
                      151430
                      151427
                      151427
                      151427
                      151430
                      151427
                      151432
                      151430
                      151427])

(def test-cycle [0 700000])

(defn infer-from-cycle [increments grain [cycle-start cycle-period] target]
  (let [factor (quot (- target cycle-start) cycle-period)
        remainder (mod (- target cycle-start) cycle-period)
        body (drop (quot cycle-start grain) increments)
        depth-per-cycle (apply + (take (quot cycle-period grain) increments))
        depth-in-rest (apply + (take (quot (+ cycle-start remainder) grain) increments))]
    (+ (* factor depth-per-cycle) depth-in-rest)))

(def test-result (infer-from-cycle test-increments 100000 test-cycle 1000000000000))

(defn advance-steps [state step-count]
  (->> (iterate step state)
       (drop step-count)
       (first)))

(defn drop-n-more [state n]
  (let [rocks (count (:rock-stack (:chamber state)))
        target (+ rocks n)]
    (->> (iterate step state)
         (drop-while #(< (count (:rock-stack (:chamber %))) target))
         (first))))

(defn find-epoch-increments-up-to-cycle [input]
  (loop [state (initialise (->State (configure-chamber)
                                    (cycle (seq input))
                                    (cycle [->Horizontal ->Cross ->Elbow ->Vertical ->Square])))
         depths '()
         increments '()
         rock-counts '()
         rock-count-increments '()]
    (let [epoch (* 5 (count input))
          new-state (advance-steps state epoch)
          new-depth (depth (:chamber new-state))
          new-count (count (:rock-stack (:chamber new-state)))
          increment (- new-depth (or (first depths) 0))
          rock-count-increment (- new-count (or (first rock-counts) 0))]
      (println new-depth increment new-count rock-count-increment)
      (if (and (> (count depths) 5) (apply = (take 5 increments)))
        [epoch (vec (reverse (drop 4 increments))) (vec (reverse (drop 4 rock-count-increments)))]
        (recur new-state
               (cons new-depth depths)
               (cons increment increments)
               (cons new-count rock-counts)
               (cons rock-count-increment rock-count-increments))))))

(defn do-day17b [input]
  (let [[epoch-len increments rc-increments] (find-epoch-increments-up-to-cycle input)
        target 1000000000000
        prefix-epochs (dec (count increments))
        depth-prefix (apply + (butlast increments))
        depth-stable (last increments)
        rc-prefix (apply + (butlast rc-increments))
        rc-stable (last rc-increments)
        required-stable-epochs (quot (- target rc-prefix) rc-stable)
        extra-rocks-required (rem (- target rc-prefix) rc-stable)]

    (println "After" prefix-epochs "epochs (depth" depth-prefix "rocks" rc-prefix ")")
    (println "Require" required-stable-epochs "more (each depth" depth-stable "rocks" rc-stable ")")
    (println "Plus" extra-rocks-required "more rocks...")

    (let [state (initialise (->State (configure-chamber)
                                     (cycle (seq input))
                                     (cycle [->Horizontal ->Cross ->Elbow ->Vertical ->Square])))
          pre-cycle-state (advance-steps state (* prefix-epochs epoch-len))
          depth-before (depth (:chamber pre-cycle-state))
          topped-up-state (drop-n-more pre-cycle-state extra-rocks-required)
          depth-after (depth (:chamber topped-up-state))
          depth-suffix (- depth-after depth-before)]

      (println extra-rocks-required "more rocks adds" depth-suffix "depth so...")
      (dec (+ depth-prefix
              (* required-stable-epochs depth-stable)
              depth-suffix))
      ;; subtract the floor
      )))

(defn test-day17b []
  (do-day17b test-input))

(defn day17b []
  (do-day17b (first (lines "day17.txt"))))

;; [153720 307446 461115 614821 768513 922220 1075939 1229616 1383317
;; 1537005]

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
