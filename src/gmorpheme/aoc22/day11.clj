(ns gmorpheme.aoc22.day11
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]]))

(defn parse-monkey [lines]
  (let [[_ id] (re-matches #"Monkey (\d+):" (first lines))
        items (mapv (comp parse-long second)
                    (re-seq #"(\d+)" (second lines)))
        [l op r] (mapv #(or (parse-long %) (symbol %))
                       (rest (re-matches
                              #"\s+Operation: new = (\w+)\s+(\S)\s+(\w+)"
                              (nth lines 2))))
        [_ d] (re-matches #"\D+(\d+)" (nth lines 3))
        [_ t] (re-matches #"\D+(\d+)" (nth lines 4))
        [_ f] (re-matches #"\D+(\d+)"  (nth lines 5))]
    {:id (parse-long id)
     :items items
     :op (list op l r)
     :divisor (parse-long d)
     :t (parse-long t)
     :f (parse-long f)
     :inspections 0}))

(defn parse-monkeys [lines]
  (->> lines
       (partition-by str/blank?)
       (remove #(= % [""]))
       (mapv parse-monkey)))

;; nb... would never have lazily used `eval' if I knew what was coming
;; in part b; this is all _horribly_ slow

(declare ^:dynamic old)

(defn turn [dampen i monkeys]
  (loop [monkeys monkeys]
    (let [{:keys [items op divisor t f] :as monkey} (nth monkeys i)]
      (if (seq items)
        (let [val (binding [old (first items)] (dampen (eval op)))
              dest (if (zero? (mod val divisor)) t f)
              monkey (-> monkey
                         (update :inspections inc)
                         (assoc :items (vec (rest items))))
              recipient (-> (nth monkeys dest)
                            (update :items conj val))
              new-monkeys (-> monkeys
                              (assoc i monkey)
                              (assoc dest recipient))]
          (recur new-monkeys))
        monkeys))))

(defn round [calm-fn monkeys]
  (print ".")
  (let [turns (->> (range 0 (count monkeys))
                   (map #(partial calm-fn %)))]
    (reduce #(%2 %1) monkeys turns)))

(defn do-day11 [lines limit calm-fn]
  (let [monkeys (parse-monkeys lines)
        modulus (apply * (map :divisor monkeys))
        dampen (comp  #(mod % modulus))
        round-f (partial round (partial turn (comp calm-fn dampen)))]
    (->> monkeys
         (iterate round-f)
         (drop limit)
         first
         (map :inspections)
         (sort >)
         (take 2)
         (apply *))))

(defn test-day11a []
  (do-day11 (lines "day11-test.txt") 20 #(int (/ % 3))))

(defn day11a []
  (do-day11 (lines "day11.txt") 20 #(int (/ % 3))))

(defn test-day11b []
  (do-day11 (lines "day11-test.txt") 10000 identity))

(defn day11b []
  (do-day11 (lines "day11.txt") 10000 identity))
