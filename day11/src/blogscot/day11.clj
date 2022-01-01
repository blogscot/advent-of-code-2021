(ns blogscot.day11
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-coords [state]
  (for [y (range (count state))
        x (range (count (first state)))]
    [y x]))

(defn update-octupi [octupi action]
  (reduce (fn [state [row col]] (action state row col)) octupi (get-coords octupi)))

(defn add1 [state row col]
  (update-in state [row col :value] inc))

(defn reset [state row col]
  (let [current (get-in state [row col :value])]
    (if (> current 9)
      (assoc-in state [row col :value] 0)
      state)))

(defn find-adjacent-points [[height width] [row col]]
  (let [offsets (remove #(= % [0 0])
                        (for [y [-1 0 1]
                              x [-1 0 1]]
                          [y x]))
        adjacents (map (fn [[r c]] [(+ row r) (+ col c)]) offsets)]
    (filter (fn [[y x]] (and (< -1 x width) (< -1 y height))) adjacents)))

(defn flash-at [state row col]
  (let [{value :value flashed :flashed} (get-in state [row col])]
    (if (and (> value 9) (false? flashed))
      (let [height (count state)
            width (count (first state))
            neighbours (find-adjacent-points [height width] [row col])
            new-state (update-in state [row col] (fn [{value :value}] {:value (inc value) :flashed true}))]
        (reduce (fn [acc [row col]] (update-in acc [row col :value] inc)) new-state neighbours))
      state)))

(defn flash [state]
  (let [coords (get-coords state)
        reset-flashing (reduce (fn [state [row col]] (assoc-in state [row col :flashed] false)) state coords)]
    (loop [current-state reset-flashing]
      (if (not-any? #(and (> (:value %) 9) (false? (:flashed %))) (flatten current-state))
        current-state
        (recur (reduce (fn [state [row col]] (flash-at state row col)) current-state coords))))))

(defn count-flashes [state]
  (count (filter :flashed (flatten state))))

(defn solve-part1 [filename steps]
  (let [octupi (mapv (comp (partial mapv read-string) #(str/split % #"")) (str/split-lines (slurp filename)))
        state (mapv (fn [row] (mapv #(assoc {} :value % :flashed false) row)) octupi)]
    (loop [step 0
           flashed 0
           state state]
      (if (= step steps)
        (+ flashed (count-flashes state))
        (recur (inc step)
               (+ flashed (count-flashes state))
               (-> state
                   (update-octupi add1)
                   flash
                   (update-octupi reset)))))))

(solve-part1 "data.txt" 100)
(solve-part1 "puzzle.txt" 100)

(defn solve-part2 [filename]
  (let [octupi (mapv (comp (partial mapv read-string) #(str/split % #"")) (str/split-lines (slurp filename)))
        state (mapv (fn [row] (mapv #(assoc {} :value % :flashed false) row)) octupi)
        all-flashing (count (flatten state))]
    (loop [step 0
           state state]
      (if (= (count-flashes state) all-flashing)
        step
        (recur (inc step)
               (-> state
                   (update-octupi add1)
                   flash
                   (update-octupi reset)))))))

(solve-part2 "data.txt")
(solve-part2 "puzzle.txt")
