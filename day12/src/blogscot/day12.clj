(ns blogscot.day12
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn is-lowercase? [val]
  (= (.toLowerCase (name val)) (name val)))

(defn build-graph [filename]
  (let [lines (str/split-lines (slurp filename))
        paths (reduce #(concat % [%2] [(vec (reverse %2))]) []
                      (map #(str/split % #"-") lines))]
    (apply merge-with into
           (map (fn [[k v]] (assoc {} (keyword k) [(keyword v)])) paths))))

(defn find-neighbours
  "Returns the sequence of neighbors for the given node.
   Small caves can be visited only once."
  [coll visited]
  (let [start (if (empty? visited) :start (peek visited))
        small-caves (filter is-lowercase? visited)
        all-neighbours (get coll start)]
    (vec (set/difference (set all-neighbours) (set small-caves)))))

(defn build-tree [find graph branch]
  (if (= :end (peek branch))
    branch
    (let [children (find graph branch)
          branches (map #(conj branch %) children)]
      (mapcat #(build-tree find graph %) branches))))

(defn solver [find filename]
  (let [graph (build-graph filename)]
    (count (filter (partial = :end) (build-tree find graph [:start])))))

(comment
  (solver find-neighbours "data3.txt")
  (solver find-neighbours "puzzle.txt"))

;; part 2

(defn valid-neighbour? [path neighbour]
  (let [freqs (frequencies (filter is-lowercase? path))
        visited-twice (= 2 (apply max (vals freqs)))
        times-visited (or (freqs neighbour) 0)]
    (or (false? visited-twice) (= times-visited 0))))

(defn find-neighbours' [coll visited]
  (let [all-neighbours (remove #(= % :start) (get coll (peek visited)))]
    (filter #(valid-neighbour? visited %) all-neighbours)))

(comment
  (solver find-neighbours' "data3.txt")
  (solver find-neighbours' "puzzle.txt"))