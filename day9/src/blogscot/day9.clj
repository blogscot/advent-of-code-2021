(ns blogscot.day9
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-height-map [filename]
  (let [lines (str/split-lines (slurp filename))]
    (mapv #(mapv read-string (str/split % #"")) lines)))

(defn find-adjacent-points [[height width] [row col]]
  (let [adjacent [[(dec row) col] [row (inc col)] [(inc row) col] [row (dec col)]]]
    (filter (fn [[x y]] (and (nat-int? x) (nat-int? y) (< x width) (< y height))) adjacent)))

(defn low-point? [height-map dimensions point]
  (let [value (get-in height-map point)
        neighbours-values (map (partial get-in height-map)
                               (find-adjacent-points dimensions point))]
    (every? (partial < value) neighbours-values)))

(defn find-low-points [height-map]
  (let [height (count height-map)
        width (count (first height-map))
        coords (for [y (range height)
                     x (range width)]
                 [y x])]
    (filter (partial low-point? height-map [width height]) coords)))

(defn solve-part1 [filename]
  (let [height-map (get-height-map filename)
        low-points (find-low-points height-map)]
    (apply + (map (comp inc (partial get-in height-map)) low-points))))

(solve-part1 "data.txt")
(solve-part1 "puzzle.txt")


(defn find-neighbours [visited-map dimensions point]
  (remove (fn [[x y]] (get-in visited-map [x y :visited]))
          (find-adjacent-points dimensions point)))

(defn find-basin [visited-map dimensions [x y :as point] basin]
  (if (get-in @visited-map [x y :visited])
    []
    (let [neighbours (find-neighbours @visited-map dimensions point)
          new-basin (conj basin point)]
      (if (empty? neighbours)
        new-basin
        (do
          (swap! visited-map update-in [x y :visited] not)
          (set (mapcat #(find-basin visited-map dimensions % new-basin) neighbours)))))))

(defn solve-part2 [filename]
  (let [height-map (get-height-map filename)
        low-points (find-low-points height-map)
        visited-map (atom (mapv (fn [row] (mapv #(assoc {} :visited (= % 9)) row)) height-map))
        dimensions [(count @visited-map) (count (first @visited-map))]
        basins (map #(find-basin visited-map dimensions % #{}) low-points)]
    (apply * (take-last 3 (sort (map count basins))))))

(solve-part2 "data.txt")
(solve-part2 "puzzle.txt")
