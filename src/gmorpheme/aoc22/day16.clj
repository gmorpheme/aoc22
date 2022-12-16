(ns gmorpheme.aoc22.day16
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]))

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

(defrecord State [valves location time-remaining value-accumulated best-still-available])

(defn best-value-achievable [{:keys [location valves time-remaining] :as state}]
  (let [this-rate (get-in valves [location :rate])
        other-rates (remove nil? (map (fn [[k v]] (when (not= k location) (:rate v))) valves))]
    (apply +
           (map *
                ;; if no rate, we'd start with a move
                (range (cond-> time-remaining (zero? this-rate) dec) 0 -2)
                ;; if rate, we'd realise it before others
                (cond->> (sort > other-rates) (pos? this-rate) (cons this-rate))))))

(defn update-best-still-available [state]
  (assoc state :best-still-available (best-value-achievable state)))

(defn parse-state [lines]
  (let [valves (parse-valves lines)
        time-remaining 30]
    (update-best-still-available (->State valves :AA time-remaining 0 0))))

(defn move [{:keys [location] :as state} dest]
  (-> state
      (assoc :location dest)
      (update :time-remaining dec)
      (update-best-still-available)))

(defn open [{:keys [location valves time-remaining] :as state}]
  (let [value-accumulated (* (dec time-remaining)
                             (get-in valves [location :rate]))]
    (-> state
        (assoc-in [:valves location :rate] 0)
        (update :value-accumulated + value-accumulated)
        (update :time-remaining dec)
        (update-best-still-available))))

(defn options [{:keys [location valves time-remaining best-still-available] :as state}]
  (when (and (pos? time-remaining)
             (pos? best-still-available))
    (let [moves (get-in valves [location :tunnels])]
      (if (pos? (get-in valves [location :rate]))
        (conj moves :open)
        moves))))

(defn option-states [state]
  (map
   #(if (#{:open} %) (open state) (move state %))
   (options state)))

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
                :location)]
    (compare (f a) (f b))))

(defn supersede [bests {:keys [value-accumulated best-still-available] :as state}]
  (->> bests
       (filter (fn [[v r]] (not-worse {:value-accumulated v :best-still-available r} state)))
       (cons [value-accumulated best-still-available])))

(defn record-costs [best-cost-map state]
  (update best-cost-map (:location state) #(supersede % state)))

(defn worthwhile [best-cost-map {:keys [location] :as state-option}]
  (if-let [bests (get best-cost-map location)]
    (every?
     (fn [[v r]] (not-worse state-option {:value-accumulated v :best-still-available r}) )
     bests)
    true))

;; find-path
(defn a-star
  [state]
  (loop [state state
         cost-map {}
         fringe (sorted-set-by search-preference)
         best 0]
    (let [opts (filter #(worthwhile cost-map %) (option-states state))
          fringe (into fringe opts)]
      (if-let [selected (first fringe)]
        (let [new-best (max best (:value-accumulated selected))]
          (recur selected (record-costs cost-map selected) (disj fringe selected) new-best))
        best))))



(defn test-day16a []
  (a-star (parse-state (lines "day16-test.txt"))))

(defn day16a []
  (a-star (parse-state (lines "day16.txt"))))


;;

(defn best-value-achievable-using-elephant [{:keys [me elephant valves time-remaining] :as state}]
  (let [me-rate (get-in valves [me :rate])
        el-rate (get-in valves [elephant :rate])
        our-locs (into #{} [me elephant])
        our-takes (if (= me elephant) [me-rate 0] [me-rate el-rate])
        other-rates (sort > (remove nil? (map (fn [[k v]] (when (not (our-locs k)) (:rate v))) valves)))
        projected-takes (concat our-takes (cond
                                            (= our-takes [0 0]) (->> (partition 2 other-rates)
                                                                     (interpose [0 0])
                                                                     (apply concat))
                                            (zero? (first our-takes)) (cons 0 (interpose 0 other-rates))
                                            (zero? (second our-takes)) (interpose 0 other-rates)
                                            :else (concat [0 0] (->> (partition 2 other-rates)
                                                                     (interpose [0 0])
                                                                     (apply concat)))))]
    (apply +
           (map *
                (mapcat (fn [x] [x x]) (range (dec time-remaining) 0 -1))
                projected-takes))))

(defrecord ElephantineState [me elephant time-remaining value-accumulated best-still-available valves path])

(defn make-elephantine [state]
  (-> state
      (assoc :time-remaining 26)
      (dissoc :location)
      (assoc :elephant :AA)
      (assoc :me :AA)
      (as-> state
          (assoc state :best-still-available (best-value-achievable-using-elephant state)))
      (assoc :path [])
      (map->ElephantineState)))

(defn open-valve [{:keys [time-remaining valves] :as state} valve]
  (-> state
      (assoc-in [:valves valve :rate] 0)
      (update :value-accumulated #(+ % (* time-remaining (get-in valves [valve :rate]))))))

(defn transition [{:keys [me elephant values time-remaining] :as state} my-move elephant-move]
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
            (assoc state :best-still-available (best-value-achievable-using-elephant state))))))

(defn moves [{:keys [valves]} loc]
  (cond-> (get-in valves [loc :tunnels])
    (pos? (get-in valves [loc :rate])) (conj :open)))

(defn elephantine-options [{:keys [me elephant valves time-remaining best-still-available] :as state}]
  (when (and (pos? time-remaining)
             (pos? best-still-available))
    (for [my-move (moves state me)
          el-move (moves state elephant)]
      [my-move el-move])))

(defn elephantine-option-states [state]
  (map (partial apply transition state) (elephantine-options state)))

;; me and the elephant are entirely equivalent so use set of our
;; positions in the cost map

(defn record-elephantine-costs [best-cost-map {:keys [me elephant time-remaining] :as state}]
  (let [k [time-remaining (into #{} [me elephant])]]
    (update best-cost-map k #(supersede % state))))

(defn worthwhile-elephantly [best-cost-map {:keys [me elephant time-remaining] :as state-option}]
  (let [k [time-remaining (into #{} [me elephant])]]
    (if-let [bests (get best-cost-map k)]
      (every?
       (fn [[v r]] (not-worse state-option {:value-accumulated v :best-still-available r}) )
       bests)
      true)))

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

(defn find-elephantine-best
  [state]
  (loop [state state
         cost-map {}
         fringe (sorted-set-by elephantine-search-preference)
         best 0
         best-path []]
    (let [opts (elephantine-option-states state)
          opts (filter #(worthwhile-elephantly cost-map %) opts)
          fringe (into fringe opts)]
      (if-let [[selected new-fringe] (poll fringe #(worthwhile-elephantly cost-map %))]
        (let [new-best (max best (:value-accumulated selected))
              new-path (if (> new-best best) (:path selected) best-path)]
          (recur selected (record-elephantine-costs cost-map selected) new-fringe new-best new-path))
        [best best-path]))))

(defn test-day16b []
  (make-elephantine (parse-state (lines "day16-test.txt"))))

(defn day16b []
  (make-elephantine (parse-state (lines "day16.txt"))))
