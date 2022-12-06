(ns gmorpheme.aoc22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn lines [filename]
  (str/split-lines (slurp (io/resource filename))))
