(ns gmorpheme.aoc22.day19
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def test-input (str/split-lines "Blueprint 1:  Each ore robot costs 4 ore.  Each clay robot costs 2 ore.  Each obsidian robot costs 3 ore and 14 clay.  Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2:  Each ore robot costs 2 ore.  Each clay robot costs 3 ore.  Each obsidian robot costs 3 ore and 8 clay.  Each geode robot costs 3 ore and 12 obsidian."))

(def REGEX #"Blueprint (\d+):.*ore robot costs (\d+) ore.*clay robot costs (\d+) ore.*obsidian robot costs (\d+) ore and (\d+) clay.*geode robot costs (\d+) ore and (\d+) obsidian.")

(defn parse-blueprint [line]
  (let [[id robot-ore clay-ore obsidian-ore obsidian-clay geode-ore geode-obsidian]
        (map parse-long
             (rest
              (re-matches REGEX line)))]
    {:id id
     :ore {:ore robot-ore}
     :clay {:ore clay-ore}
     :obsidian {:ore obsidian-ore :clay obsidian-clay}
     :geode {:ore geode-ore :obsidian geode-obsidian}}))

(defn parse-blueprints [lines]
  (map parse-blueprint lines))

;; state and evolution

(defrecord State [blueprint robots minerals time-remaining])

(defn robot-count [state mineral]
  (or (get-in state [:robots mineral]) 0))

(defn mineral-count [state mineral]
  (or (get-in state [:minerals mineral]) 0))

(defn start-state [blueprint]
  (->State blueprint {:ore 1} {} 24))

(defn spend [minerals cost]
  (reduce-kv (fn [m k v] (update m k #(- (or % 0) v))) minerals cost))

(defn can-afford [minerals cost]
  (every? (comp not neg?) (vals (spend minerals cost))))

(defn multiply-map [cost x]
  (into {}
        (map (fn [[k v]] [k (* x v)]))
        cost))

(defn cost-orders [blueprint orders]
  (apply merge-with
         +
         (map
          (fn [[robot n]] (multiply-map (get blueprint robot) n))
          orders)))

(defn evolve-state
  [{:keys [blueprint robots minerals time-remaining]} orders]
  (let [costs (cost-orders blueprint orders)
        minerals (spend minerals costs)
        minerals (merge-with + minerals robots)
        robots (merge-with + robots orders)]
    (->State blueprint robots minerals (dec time-remaining))))

(defn order-compare [l r]
  (let [sig (juxt :geode :obsidian :clay :ore)]
    (- (compare (sig l) (sig r)))))

(defn heuristic-state-compare [l r]
  (let [robot-order (order-compare (:robots l) (:robots r))]
    (if (zero? robot-order)
      (let [mineral-order (order-compare (:minerals l) (:minerals r))]
        (if (zero? mineral-order)
          (compare (:time-remaining l) (:time-remaining r))
          mineral-order))
      robot-order)))

(defn possible-orders
  [blueprint minerals]
  (letfn [(orders-with [mineral]
            (when (can-afford minerals (get blueprint mineral))
              (let [order {mineral 1}
                    minerals (spend minerals (get blueprint mineral))]
                (into (sorted-set-by order-compare order)
                      (map (partial merge-with + order))
                      (possible-orders blueprint minerals)))))]
    (set/union (orders-with :geode)
               (orders-with :obsidian)
               (orders-with :clay)
               (orders-with :ore)
               #{{}})))

(defn sane-orders
  [blueprint minerals]
  (letfn [(orders-with [mineral]
            (when (can-afford minerals (get blueprint mineral))
              (let [order {mineral 1}
                    minerals (spend minerals (get blueprint mineral))]
                (into (sorted-set-by order-compare order)
                      (map (partial merge-with + order))
                      (possible-orders blueprint minerals)))))]
    (let [orders-with-geode (orders-with :geode)]
      (if (not (empty? orders-with-geode))
        orders-with-geode
        (set/union
         (orders-with :obsidian)
         (orders-with :clay)
         (orders-with :ore)
         #{{}})))))

(defn options [{:keys [blueprint minerals]}]
  (possible-orders blueprint minerals))

(defn sane-options [{:keys [blueprint minerals]}]
  (sane-orders blueprint minerals))

(defn open-geodes [state]
  (or (:geode (:minerals state)) 0))

;; evaluation and strategy

;; need strategy to reduce search space...

(defn robots-cover [state-a state-b]
  (every?
   #(> (robot-count state-a %) (robot-count state-b %))
   [:geode :obsidian :clay :ore]))

(defn robots-outweigh-deficit-over-time [robots t minerals-a minerals-b]
  (can-afford
   (merge-with + (multiply-map robots t) minerals-a)
   minerals-b))

(defn obsoletes [state-a state-b]
  (let [ta (:time-remaining state-a)
        tb  (:time-remaining state-b)]
    (and (>= ta tb)
         (robots-cover state-a state-b)
         (robots-outweigh-deficit-over-time (:robots state-a) (- ta tb) (:minerals state-a) (:minerals state-b)))))

(defn prune-states-from-prior [prior-levels states]
  (let [seen (apply concat prior-levels)]
    (reduce (fn [ss i] (remove #(obsoletes i %) ss)) states seen)))

(defn prune-states-internally [states]
  (loop [states states
         pruned '()]
    (let [[s & ss] states]
      (if (some? s)
        (recur (remove #(obsoletes s %) ss) (cons s pruned))
        (reverse pruned)))))

(defn state-set [& ks]
  (apply sorted-set-by heuristic-state-compare ks))

(defn sane-child-states [state]
  (map #(evolve-state state %) (sane-options state)))

(defn build-maximisation-tree [start-state]
  (loop [levels [(state-set start-state)]]
    (if (zero? (:time-remaining (first (last levels))))
      levels
      (let [next-level (into (state-set) (mapcat sane-child-states) (last levels))
            next-level (prune-states-from-prior levels next-level)
            next-level (into (state-set) (prune-states-internally next-level))
            ]
        (recur (conj levels next-level))))))

(defn value [{:keys [blueprint time-remaining] :as state}]
  (if (zero? time-remaining)
    (open-geodes state)
    (let [options (sane-options state)
          child-values (->> options
                            (map #(evolve-state state %))
                            (map value))]
      (apply max child-values))))

(defn evaluate-tree [levels]
  (println (count (last levels)))
  (apply max (map value (last levels))))

(defn evaluate-blueprint [bp]
  (evaluate-tree (build-maximisation-tree (start-state bp))))

(defn blueprint-geode-production-ore-minutes [bp]
  (let [obsidian (get-in bp [:geode :obsidian])
        obsidian-clay (get-in bp [:obsidian :clay])
        obsidian-ore (get-in bp [:obsidian :ore])
        clay (* clay obsidian-clay)
        clay-ore (get-in bp [:clay :ore])
        ore-minutes (+ (get-in bp [:geode :ore])
                       (* clay-ore clay)
                       (* obsidian-ore obsidian))]
    ore-minutes))

(defn test-day19a []
  (let [bp1 (first (parse-blueprints test-input))]
    (-> (start-state bp1)
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (build-maximisation-tree)
        (evaluate-tree))))

(defn day19a []
  (parse-blueprints (lines "day19.txt")))


(comment

  (let [bp1 (first (parse-blueprints test-input))]
    (-> (start-state bp1)
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:obsidian 1})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:obsidian 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:geode 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:geode 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {})))

  (let [bp1 (first (parse-blueprints test-input))]
    (-> (start-state bp1)
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:obsidian 1})
        (evolve-state {:clay 1})
        (evolve-state {})
        (evolve-state {})
        (evolve-state {:obsidian 1})
        (evolve-state {})
        (evolve-state {})
        (sane-options)))

  )
