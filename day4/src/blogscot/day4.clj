(ns blogscot.day4
  (:gen-class)
  (:require [clojure.string :as str]))

(defn load-data [filename]
  (let [lines (str/split-lines (slurp filename))
        bingo-numbers (map read-string (re-seq #"\d+" (first lines)))
        digits (re-seq #"\d+" (str/join " " (rest lines)))
        numbers (map read-string digits)
        boards (mapv vec (partition 5 (map vec (partition 5 numbers))))]
    [bingo-numbers boards]))

(defn update-board [board bingo-num]
  (let [height (count board)
        width (count (first board))]
    (loop [row 0
           col 0]
      (if (>= row height)
        board
        (if (>= col width)
          (recur (inc row) 0)
          (if (= bingo-num (get-in board [row col]))
            (update-in board [row col] :nil)
            (recur row (inc col))))))))

(defn update-boards [boards bingo-num]
  (map #(update-board % bingo-num) boards))

(defn check-rows [board]
  (loop [n 0]
    (if (= n (count board))
      false
      (if (every? nil? (nth board n))
        true
        (recur (inc n))))))

(defn transpose [board]
  (apply (partial mapv vector) board))

(defn check-board [board]
  (or (check-rows board) (check-rows (transpose board))))

(defn total-board [board]
  (apply + (remove nil? (flatten board))))

(defn find-winner [boards bingo-num]
  (let [new-boards (update-boards boards bingo-num)]
    (loop [n 0]
      (if (= n (count boards))
        [false new-boards 0]
        (let [board (nth new-boards n)]
          (if (true? (check-board board))
            [true nil (total-board board)]
            (recur (inc n))))))))

(defn solve-part1 [filename]
  (let [[bingo-numbers boards] (load-data filename)]
    (loop [n 0
           boards boards]
      (if (= n (count bingo-numbers))
        nil
        (let [bingo-num (nth bingo-numbers n)
              [found new-boards total] (find-winner boards bingo-num)]
          (if (true? found)
            (* bingo-num total)
            (recur (inc n) new-boards)))))))

(solve-part1 "data.txt")
(solve-part1 "puzzle.txt")

(defn remove-winner [boards bingo-num]
  (let [new-boards (update-boards boards bingo-num)]
    (loop [n 0]
      (if (= n (count boards))
        [false new-boards]
        (let [board (nth new-boards n)]
          (if (true? (check-board board))
            [true (vec (concat (subvec (vec new-boards) 0 n) (subvec (vec new-boards) (inc n))))]
            (recur (inc n))))))))

(defn solve-part2 [filename]
  (let [[bingo-numbers boards] (load-data filename)]
    (loop [n 0
           boards boards]
      (let [bingo-num (nth bingo-numbers n)
            [found new-boards] (remove-winner boards bingo-num)]
        (if (true? found)
          (if (zero? (count new-boards))
            (* (- (total-board boards) bingo-num) bingo-num)
            (recur n new-boards))
          (recur (inc n) new-boards))))))

(solve-part2 "data.txt")
(solve-part2 "puzzle.txt")
