(ns blogscot.day13
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-data [filename]
  (let [parts (partition-by #(= % "") (str/split-lines (slurp filename)))
        coords (mapv (comp #(mapv read-string %) #(str/split % #",")) (first parts))
        folds  (map (comp (fn [[_ b c]] [b (read-string c)]) first #(re-seq #"([xy])=(\d+)" %)) (nth parts 2))]
    {:coords coords :folds folds}))

(defn fold-at [edge pos]
  (if (> pos edge) (- edge (- pos edge)) pos))

(defn build-dots [{coords :coords folds :folds} steps]
  (loop [n 0
         coords coords]
    (if (= n steps)
      coords
      (let [[fold at] (nth folds n)
            new-coords (case fold
                         "x" (map (fn [[x y]] [(fold-at at x) y]) coords)
                         "y" (map (fn [[x y]] [x (fold-at at y)]) coords))]
        (recur (inc n) new-coords)))))

(defn solve-part1 [filename]
  (count (set (build-dots (get-data filename) 1))))

(solve-part1 "data.txt")
(solve-part1 "puzzle.txt")

;; part 2

(defn build-dots' [filename]
  (let [data (get-data filename)]
    (build-dots data (count (data :folds)))))

(comment
  ;; draw the letters with external library, incanter.
  (use '(incanter core stats charts))
  (def dots (build-dots' "puzzle.txt"))
  ;; flip the y-axis to read the letters more easily
  (view (scatter-plot (map first dots) (map #(- 5 (second %)) dots))))

