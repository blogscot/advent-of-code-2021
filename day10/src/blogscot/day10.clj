(ns blogscot.day10
  (:gen-class)
  (:require [clojure.string :as str]))

(defn find-match [char]
  (case char
    \( \)
    \[ \]
    \{ \}
    \< \>))

(defn matcher [[a b c d] char]
  (case char
    \) a
    \] b
    \} c
    \> d))

(def score-corrupt (partial matcher [3 57 1197 25137]))
(def score-incomplete (partial matcher [1 2 3 4]))

(defn check-chunk [chunk]
  (let [length (count chunk)]
    (loop [n 0
           stack []
           error false]
      (if (= n length)
        (if (empty? stack)
          :valid
          {:incomplete (reverse stack)})
        (let [char (nth chunk n)
              expected (last stack)]
          (if (true? error)
            {:expected expected :found char}
            (if (contains? (set "([{<") char)
              (recur (inc n) (conj stack (find-match char)) false)
              (if (= expected char)
                (recur (inc n) (vec (drop-last stack)) false)
                (recur n stack true)))))))))


(defn solve-part1 [filename]
  (let [chunks (str/split-lines (slurp filename))]
    (apply + (map score-corrupt (remove nil? (map :found (map check-chunk chunks)))))))

(solve-part1 "data.txt")
(solve-part1 "puzzle.txt")

(defn total-score [scores]
  (reduce (fn [acc val] (+ val (* 5 acc))) 0 scores))

(defn solve-part2 [filename]
  (let [chunks (str/split-lines (slurp filename))
        incomplete-chunks (remove nil? (map :incomplete (map check-chunk chunks)))
        total-scores (sort (map (comp total-score (partial map score-incomplete)) incomplete-chunks))
        number-items (count total-scores)]
    (nth total-scores (quot number-items 2))))

(solve-part2 "data.txt")
(solve-part2 "puzzle.txt")
