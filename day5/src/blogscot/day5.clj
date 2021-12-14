(ns blogscot.day5
  (:gen-class)
  (:require [clojure.string :as str])
  #_(:require [clojure.pprint :as pp]))

(defn get-all-points
  [filename]
  (let [data (str/join " " (str/split-lines (slurp filename)))]
    (map (fn [coord]
           {:x (Integer/parseInt (nth coord 1))
            :y (Integer/parseInt (nth coord 2))})
         (re-seq #"(\d+),(\d+)" data))))

(defn get-dimenions
  "Calculates the dimension of a domain that would contain all
   the given points."
  [points]
  (loop [n 0
         max-x 0
         max-y 0]
    (if (= n (count points))
      [(inc max-x) (inc max-y)]
      (let [point (nth points n)]
        (recur (inc n) (max max-x (point :x)) (max max-y (point :y)))))))

(defn get-horizontal-vertical-lines
  [lines]
  (filter (fn [{start :start end :end}]
            (or (= (start :x) (end :x))
                (= (start :y) (end :y)))) lines))

(defn calculate-delta [num]
  (cond
    (zero? num) 0
    (pos? num) -1
    :else 1))

(defn build-points [{p1 :start p2 :end}]
  (let [dx (calculate-delta (- (p1 :x) (p2 :x)))
        dy (calculate-delta (- (p1 :y) (p2 :y)))]
    (loop [point p1
           points [p1]]
      (if (= point p2)
        points
        (let [new-point {:x (+ (point :x) dx) :y (+ (point :y) dy)}]
          (recur new-point (conj points new-point)))))))

(defn solve-with [filter-fn filename]
  (let [all-points (get-all-points filename)
        [width height] (get-dimenions all-points)
        all-lines (map (fn [[x y]] {:start x :end y})
                       (partition 2 all-points))
        points (mapcat build-points (filter-fn all-lines))
        domain (vec (repeat height (vec (repeat width 0))))
        updated-domain (reduce (fn [domain point]
                                 (update-in domain [(:y point) (:x point)] inc))
                               domain points)]
    (count (filter #(> % 1) (apply concat updated-domain)))))

(def solve-part1 (partial solve-with get-horizontal-vertical-lines))

(solve-part1 "data.txt")
(solve-part1 "puzzle.txt")

(def solve-part2 (partial solve-with identity))

(solve-part2 "data.txt")
(solve-part2 "puzzle.txt")
