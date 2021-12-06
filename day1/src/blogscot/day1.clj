(ns blogscot.day1
  (:gen-class)
  (:require [clojure.string :as string]))

(defn my-partition
  [filename n]
  (partition n 1 (map #(Integer/parseInt %)
                      (string/split-lines (slurp filename)))))

(defn- my-count
  [seq]
  (count (filter #(true? %)
                 (map (fn [a] (< (first a) (nth a 1)))
                      seq))))


(my-count (my-partition "data.txt" 2))
(my-count (my-partition "puzzle.txt" 2))

(my-count (partition 2 1 (map #(reduce + %) (my-partition "data.txt" 3))))
(my-count (partition 2 1 (map #(reduce + %) (my-partition "puzzle.txt" 3))))
