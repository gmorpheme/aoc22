(ns gmorpheme.aoc22.day20
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]]))

(def test-input (str/split-lines "1
2
-3
3
-2
0
4"))

(defn parse [lines]
  (mapv parse-long lines))

(defn shift-pos [from to pos]
  (cond
    (= pos from) to
    ;; rightward
    (> to from) (if (<= (inc from) pos to) (dec pos) pos)
    ;; leftward
    (< to from ) (if (<= to pos (dec from)) (inc pos) pos)
    ;; null
    :else pos))

(defn run [posvec]
  (let [len (count posvec)]
    (loop [i 0
           positions posvec]
      (if (< i len)
        (let [[value old-pos]  (positions i)
              new-pos (mod (+ old-pos value) (dec len))
              new-pos (if (zero? new-pos) (dec len) new-pos)
              shifted-positions (mapv (fn [[v pos]] [v (shift-pos old-pos new-pos pos)]) positions)]
          (recur (inc i) shifted-positions))
        positions))))

(defn check [v i]
  (let [len (count v)
        positions (mapv vector v (range))]
    (let [[value old-pos]  (positions i)
          new-pos (mod (+ old-pos value) (dec len))
          new-pos (if (zero? new-pos) (dec len) new-pos)
          shifted-positions (mapv (fn [[v pos]] [v (shift-pos old-pos new-pos pos)]) positions)]
      (positions->vec shifted-positions))))

(defn vec->positions [v]
  (mapv vector v (range)))

(defn positions->vec [positions]
  (let [blank (vec (repeat (count positions) nil))]
    (reduce (fn [vc [val pos]] (assoc vc pos val)) blank positions)))

(defn coords [positions]
  (let [len (count positions)
        zero-pos (second (first (filter #(zero? (first %)) positions)))
        pos-1k (mod (+ zero-pos 1000) len)
        pos-2k (mod (+ zero-pos 2000) len)
        pos-3k (mod (+ zero-pos 3000) len)]
    ((juxt #(get % pos-1k) #(get % pos-2k) #(get % pos-3k)) (positions->vec positions))))

(defn test-day20a []
  (apply + (coords (run (vec->positions (parse test-input))))))

(defn preprocess [input]
  (mapv #(* 811589153 %) input))

(defn do-day20b [input]
  (->> input
       (parse)
       (preprocess)
       (vec->positions)
       (iterate run)
       (drop 10)
       (first)
       (coords)
       (apply +)))

(defn test-day20b []
  (do-day20b test-input))

(defn day20b []
  (do-day20b (lines "day20.txt")))
