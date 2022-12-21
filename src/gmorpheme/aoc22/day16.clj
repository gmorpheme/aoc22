(ns gmorpheme.aoc22.day16
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))


(defrecord GraphMetrics [vertices shortest-paths])

;; all pairs shortest paths algo
(defn apsp [valves]
  (let [vs (vec (sort (keys valves)))
        n (count vs)
        matrix (vec (repeat n (vec (repeat n Integer/MAX_VALUE))))
        es (mapcat #(map (fn [t] [% t]) (get-in valves [% :tunnels])) vs)
        add-edge (fn [matrix [s e]] (assoc-in matrix [(.indexOf vs s) (.indexOf vs e)] 1))
        add-vertex (fn [matrix v] (assoc-in matrix [(.indexOf vs v) (.indexOf vs v)] 0))
        matrix (reduce add-vertex (reduce add-edge matrix es) vs)
        dist (fn [matrix i j] (get-in matrix [i j]))
        upd (fn [matrix i j val] (assoc-in matrix [i j] val))
        calc (fn [matrix k i j] (let [ij (dist matrix i j)
                                     ikj (+ (dist matrix i k) (dist matrix k j))]
                                 (if (>  ij ikj) (upd matrix i j ikj) matrix)))
        iter (for [k (range (count vs))
                   i (range (count vs))
                   j (range (count vs))]
               [k i j])]
    (->GraphMetrics vs (reduce (fn [m [k i j]] (calc m k i j)) matrix iter))))

(defn get-shortest-path [{:keys [vertices shortest-paths]} from to]
  (let [i (.indexOf vertices from)
        j (.indexOf vertices to)]
    (get-in shortest-paths [i j])))

;;

(defn parse-valve [line]
  (when-let [[_ id rate to-valves] (re-matches #"Valve (\w+) has flow rate=(\d+);.*to valves? (.+)$" line)]
    (let [valve (keyword id)
          rate (parse-long rate)
          tunnels (into #{} (map (comp keyword str/trim)) (str/split to-valves #","))]
      [valve {:rate rate :tunnels tunnels}])))

(defn parse-valves [lines]
  (into {} (map parse-valve) lines))

;; never open a 0 valve
;; so opening valve sets rate to 0 and increases value

;; how to avoid cycles - it's legitimate to travel the same tunnel
;; more than once as subsequent decisions may be made

(defprotocol Costable
  (cost-key [self]))

(defprotocol Transitionable
  (transition [self command metrics]))

(defrecord State [valves location time-remaining value-accumulated best-still-available]
  Costable
  (cost-key [self] (:location self)))

;; a heuristic to narrow the search candidates
(defn best-value-achievable
  [{:keys [location valves time-remaining] :as state} metrics]
  (let [remaining-valves (->> valves
                              (map (fn [[k {:keys [rate]}]] [k rate]))
                              (filter (comp pos? second)))
        ideal-values (for [[valve rate] remaining-valves]
                       (* (max 0 (- time-remaining
                                    (inc (get-shortest-path metrics location valve))))
                          rate)
                       )]
    (apply + ideal-values)))

(defn update-best-still-available [state metrics]
  (assoc state :best-still-available (best-value-achievable state metrics)))

(defn parse-state [lines]
  (let [valves (parse-valves lines)
        time-remaining 30]
    (->State valves :AA time-remaining 0 0)))

(defn move [{:keys [location] :as state} dest metrics]
  (-> state
      (assoc :location dest)
      (update :time-remaining dec)
      (update-best-still-available metrics)))

(defn open [{:keys [location valves time-remaining] :as state} metrics]
  (let [value-accumulated (* (dec time-remaining)
                             (get-in valves [location :rate]))]
    (-> state
        (assoc-in [:valves location :rate] 0)
        (update :value-accumulated + value-accumulated)
        (update :time-remaining dec)
        (update-best-still-available metrics))))

(extend State
  Transitionable
  {:transition (fn [self command metrics]
                 (if (#{:open} command)
                   (open self metrics)
                   (move self command metrics)))})

(defn options [{:keys [location valves time-remaining best-still-available] :as state}]
  (when (and (pos? time-remaining)
             (pos? best-still-available))
    (let [moves (get-in valves [location :tunnels])]
      (if (pos? (get-in valves [location :rate]))
        (conj moves :open)
        moves))))

(defn option-states [state metrics]
  (map #(transition state % metrics) (options state)))

;;
;; The A* fringe of options that advances towards the solution
;;

(defn not-worse
  [{aval :value-accumulated astill :best-still-available :as a}
   {bval :value-accumulated bstill :best-still-available :as b}]
  (or
   (> aval bval)
   (if (= aval bval)
     (> astill bstill)
     (> astill (- bval aval)))))

(defn search-preference
  "Need a complete ordering for sorted set consistent with not-worse"
  [a b]
  (let [f (juxt (comp - :value-accumulated)
                (comp - :best-still-available)
                (comp - :time-remaining)
                cost-key)]
    (compare (f a) (f b))))

(defn supersede [bests {:keys [value-accumulated best-still-available] :as state}]
  (->> bests
       (filter (fn [[v r]] (not-worse {:value-accumulated v :best-still-available r} state)))
       (cons [value-accumulated best-still-available])))

(defn record-costs [best-cost-map state]
  (update best-cost-map (cost-key state) #(supersede % state)))

(defn worthwhile [best-cost-map state-option]
  (if-let [bests (get best-cost-map (cost-key state-option))]
    (every?
     (fn [[v r]] (not-worse state-option {:value-accumulated v :best-still-available r}) )
     bests)
    true))

;; find-path
(defn find-best-a
  [state]
  (let [metrics (apsp (:valves state))
        state (update-best-still-available state metrics)]
    (loop [state state
           cost-map {}
           fringe (sorted-set-by search-preference)
           best 0]
      (let [opts (filter #(worthwhile cost-map %) (option-states state metrics))
            fringe (into fringe opts)]
        (if-let [selected (first fringe)]
          (let [new-best (max best (:value-accumulated selected))]
            (recur selected (record-costs cost-map selected) (disj fringe selected) new-best))
          best)))))

(defn test-day16a []
  (find-best-a (parse-state (lines "day16-test.txt"))))

(defn day16a []
  (find-best-a (parse-state (lines "day16.txt"))))


;;

(defn best-value-achievable-using-elephant [{:keys [me elephant valves time-remaining] :as state} metrics]
  (let [remaining-valves (->> valves
                              (map (fn [[k {:keys [rate]}]] [k rate]))
                              (filter (comp pos? second)))
        ideal-values-from-me (for [[valve rate] remaining-valves]
                               (* (max 0 (- time-remaining
                                            (inc (get-shortest-path metrics me valve))))
                                  rate))
        ideal-values-from-elephant (for [[valve rate] remaining-valves]
                                     (* (max 0 (- time-remaining
                                                  (inc (get-shortest-path metrics elephant valve))))
                                        rate))]
    (apply + (map max ideal-values-from-me ideal-values-from-elephant))))

(defrecord ElephantineState [me elephant time-remaining value-accumulated best-still-available valves path]
  Costable
  (cost-key [self] (into #{} [me elephant]) ))

(defn make-elephantine [state]
  (-> state
      (assoc :time-remaining 26)
      (dissoc :location)
      (assoc :elephant :AA)
      (assoc :me :AA)
      (assoc :path [])
      (map->ElephantineState)))

(defn open-valve [{:keys [time-remaining valves] :as state} valve]
  (-> state
      (assoc-in [:valves valve :rate] 0)
      (update :value-accumulated #(+ % (* time-remaining (get-in valves [valve :rate]))))))

(extend ElephantineState
  Transitionable
  {:transition (fn [{:keys [me elephant values time-remaining] :as state} [my-move elephant-move] metrics]
                 (let [i-open (#{:open} my-move)
                       elephant-opens (#{:open} elephant-move)]
                   (-> state
                       (update :path #(conj % [my-move elephant-move]))
                       (update :time-remaining dec)
                       ;; sequential so no race when we both open the same valve
                       (cond-> i-open  (open-valve me)
                               elephant-opens (open-valve elephant)
                               (not i-open) (assoc :me my-move)
                               (not elephant-opens) (assoc :elephant elephant-move))
                       (as-> state
                           (assoc state :best-still-available (best-value-achievable-using-elephant state metrics))))))})

(defn moves [{:keys [valves]} loc]
  (cond-> (get-in valves [loc :tunnels])
    (pos? (get-in valves [loc :rate])) (conj :open)))

(defn elephantine-options [{:keys [me elephant valves time-remaining best-still-available] :as state}]
  (when (and (pos? time-remaining)
             (pos? best-still-available))
    (for [my-move (moves state me)
          el-move (moves state elephant)]
      [my-move el-move])))

(defn elephantine-option-states [state metrics]
  (map #(transition state % metrics) (elephantine-options state)))

;; me and the elephant are entirely equivalent so use set of our
;; positions in the cost map

(defn elephantine-search-preference
  "Need a complete ordering for sorted set consistent with not-worse"
  [a b]
  (let [f (juxt (comp - :value-accumulated)
                (comp - :best-still-available)
                (comp - :time-remaining)
                :me
                :elephant)]
    (compare (f a) (f b))))

(defn poll [sset ok?]
  (loop [remaining sset]
    (when-let [selected (first remaining)]
      (if (ok? selected)
        [selected (disj remaining selected)]
        (recur (disj remaining selected))))))

(defn might-beat [best {:keys [value-accumulated best-still-available]}]
  (> (+ value-accumulated best-still-available) best))

(defn find-elephantine-best
  [state]
  (let [metrics (apsp (:valves state))
        state (assoc state :best-still-available (best-value-achievable-using-elephant state metrics))]
    (loop [state state
           fringe (sorted-set-by elephantine-search-preference)
           best 0
           best-path []]
      (when (zero? (mod (count fringe) 100)) (println "FRINGE:" (count fringe)))
      (let [opts (elephantine-option-states state metrics)
            opts (filter (partial might-beat best) opts)
            fringe (into fringe opts)]
        (if-let [[selected new-fringe] (poll fringe (partial might-beat best))]
          (let [new-best (max best (:value-accumulated selected))
                new-path (if (> new-best best) (do (println new-best) (:path selected)) best-path)]
            (recur selected new-fringe new-best new-path))
          [best best-path])))))

(defn test-day16b []
  (find-elephantine-best (make-elephantine (parse-state (lines "day16-test.txt")))))

(defn day16b []
  (find-elephantine-best (make-elephantine (parse-state (lines "day16.txt")))))
