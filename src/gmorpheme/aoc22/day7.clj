(ns gmorpheme.aoc22.day7
  (:require [gmorpheme.aoc22 :refer [lines]]
            [clojure.string :as str]
            [clojure.zip :as zip]))

(def test-input (str/split-lines "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"))

;; zipper for tree of nodes [name {...kids...}] or [name file-size]
(defn fs-navigator [init]
  (zip/zipper
   (fn [z] (and (vector? z) (map? (second z))))                    ; branch?
   (fn [z] (seq (second z)))                                       ; children
   (fn [[key m] kids] [key (into m kids)])                         ; new node
   init))

(defn cmd-add-directory [dir] #(zip/append-child % [dir {}]))
(defn cmd-add-file [file size] #(zip/append-child % [file (parse-long size)]))
(defn cmd-cd [tgt] (fn [z] (->> (zip/down z)
                               (iterate zip/right)
                               (take-while some?)
                               (drop-while #(not= (first (zip/node %)) tgt))
                               (first))))
(def cmd-cd-root (fn [z] (->> (iterate zip/up z)
                             (take-while some? )
                             (last))))
(def cmd-cd-up zip/up)

;; parse shell text to zipper commands

(defn parse-dir [txt]
  (when-let [[_ dir] (re-matches #"dir (.+)$" txt)]
    (cmd-add-directory dir)))

(defn parse-file [txt]
  (when-let [[_ size file] (re-matches #"(\d+) (.+)$" txt)]
    (cmd-add-file file size)))

(defn parse-cd [txt]
  (when-let [[_  target] (re-matches #"\$ cd (.+)$" txt)]
    (condp = target
      ".." cmd-cd-up
      "/" cmd-cd-root
      (cmd-cd target))))

(defn parse-command [txt]
  (or (parse-dir txt) (parse-file txt) (parse-cd txt) nil))

(defn parse-commands [lines]
  (->> lines
       (map parse-command)
       (remove nil?)))

(defn reconstruct-filesystem [commands]
  (zip/root (reduce #(%2 %1) (fs-navigator ["/" {}]) commands)))

(defn directory-sizes [[me kids]]
  (let [files (filter (fn [[_ v]] (number? v)) kids)
        dirs (filter (fn [[_ v]] (map? v)) kids)
        subdir-sizes (map directory-sizes dirs)
        self (apply + (concat (map second files) (map first subdir-sizes)))
        all (apply concat subdir-sizes)]
    (cons self all)))

(defn do-day7a [lines]
  (let [fs (reconstruct-filesystem (parse-commands lines))
        sizes (directory-sizes fs)]
    (apply + (filter #(<= % 100000) sizes))))

(defn do-day7b [lines]
  (let [fs (reconstruct-filesystem (parse-commands lines))
        sizes (directory-sizes fs)
        total (first sizes)
        free (- 70000000 total)
        to-scavenge (- 30000000 free)]
    (apply min (filter #(>= % to-scavenge) sizes))))

(defn day7a [] (do-day7a (lines "day7.txt")))
(defn test-day7a [] (do-day7a test-input))

(defn day7b [] (do-day7b (lines "day7.txt")))
(defn test-day7b [] (do-day7b test-input))
