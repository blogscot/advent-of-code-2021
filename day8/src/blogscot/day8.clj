(ns blogscot.day8
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-readings [filename]
  (let [lines (str/split-lines (slurp filename))]
    (->> lines
         (map #(str/split % #" "))
         (map #(vector (subvec % 0 10) (subvec % 11))))))

(defn solver [readings]
  (count (filter #(or (< (count %) 5) (= (count %) 7)) (mapcat second readings))))

(def solve-part1 (comp solver get-readings))

(solve-part1 "data.txt")
(solve-part1 "puzzle.txt")

;; part 2

(defn process-reading [reading]
  (let [[line output-values] (mapv #(map set %) reading)
        groups (group-by count line)
        {[one] 2 [seven] 3 [four] 4 five-segments 5 six-segments 6 [eight] 7} groups
        c-and-f (set/intersection seven one)
        three (first (filter #(set/superset? % c-and-f) five-segments))
        six (first (remove #(set/superset? % c-and-f) six-segments))
        third-segment (first (set/difference eight six))
        two-or-five (remove (partial = three) five-segments)
        zero-or-nine (remove (partial = six) six-segments)
        five (first (remove #(contains? % third-segment) two-or-five))
        two (first (filter #(contains? % third-segment) two-or-five))
        nine (conj five third-segment)
        zero (first (remove (partial = nine) zero-or-nine))
        mappings (assoc {} zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9)]
    (map mappings output-values)))

(defn as-integer [nums]
  (reduce (fn [acc value] (+ (* 10 acc) value)) nums))

(defn solve-part2 [filename]
  (apply + (map (comp as-integer process-reading) (get-readings filename))))

(solve-part2 "data.txt")
(solve-part2 "puzzle.txt")
