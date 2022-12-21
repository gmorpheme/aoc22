(ns gmorpheme.aoc22.day21
  (:require [clojure.string :as str]
            [gmorpheme.aoc22 :refer [lines]]))

(def test-input (str/split-lines "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"))

(defrecord Lit [val])
(defrecord Var [monkey])
(defrecord BinOp [op lhs rhs])

(defn parse-monkey [line]
  (if-let [m (re-matches #"(\w+): (\d+).*" line)]
    (let [[monkey n] (rest m)]
      [(keyword monkey) (->Lit (parse-long n))])
    (let [[monkey lhs op rhs] (rest (re-matches #"(\w+): (\w+) (.) (\w+).*" line))]
      [(keyword monkey) (->BinOp (keyword op) (->Var (keyword lhs)) (->Var (keyword rhs)))])))

(defn parse-monkeys [lines]
  (into {} (map parse-monkey lines)))

(def ops
  {:+ +
   :- -
   :/ /
   :* *
   := =
   :< <
   :> >})

(defn monkey-eval [expr env]
  (cond
    (instance? Lit expr) (:val expr)
    (instance? Var expr) (monkey-eval (env (:monkey expr)) env)
    (instance? BinOp expr) (let [{:keys [lhs rhs op]} expr
                                 left (monkey-eval lhs env)
                                 right (monkey-eval rhs env)
                                 f (ops op)]
                             (f left right))))

(defn test-day21a []
  (let [env (parse-monkeys test-input)]
    (monkey-eval (->Var :root) env)))

(defn day21a []
  (let [env (parse-monkeys (lines "day21.txt"))]
    (monkey-eval (->Var :root) env)))

(defn patch-root [env op]
  (update env :root assoc :op op))

(defn feed [env humn op]
  (let [env (-> env
                (patch-root op)
                (assoc :humn (->Lit humn)))]
    (monkey-eval (->Var :root) env)))

(defn sign [n]
  (Math/signum (float n)))

(defn find-initial-bounds [env]
  (loop [attempt [-100 100]]
    (let [[lv rv] (map #(feed env % :-) attempt)]
      (if (= (sign lv) (sign rv))
        (recur [(* 10 (first attempt))
                (* 10 (second attempt))])
        [[(first attempt) lv]
         [(second attempt) rv]]))))

(defn crude-solve [env]
  (loop [[[lbound lval] [rbound rval]] (find-initial-bounds env)]
    (println lbound rbound)
    (cond
      (= lbound rbound) lbound
      (= lbound (dec rbound)) (if (zero? lval) lbound rbound)
      :else (let [mid (quot (+ rbound lbound) 2)
                  midval (feed env mid :-)]
              (cond
                (zero? midval) mid
                (= (sign midval) (sign lval)) (recur [[mid midval] [rbound rval]])
                (= (sign midval) (sign rval)) (recur [[lbound lval] [mid midval]]))))))

(defn test-day21b []
  (let [env (parse-monkeys test-input)]
    (crude-solve env)))

(defn day21b []
  (let [env (parse-monkeys (lines "day21.txt"))]
    (crude-solve env)))
