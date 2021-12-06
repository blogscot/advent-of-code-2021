(ns blogscot.day2
  (:gen-class)
  (:require [clojure.string :as string]))


(defn get_commands [filename]
  (let [lines (string/split-lines (slurp filename))]
    (for [[command value] (map #(string/split % #" ") lines)]
      (list command (Integer/parseInt value)))))

(defn calculateDepth [filename]
  (let [[horizontal depth]
        (reduce (fn [[horizontal depth]
                     [command value]]
                  (case command
                    "forward" (list (+ horizontal value) depth)
                    "down" (list horizontal (+ depth value))
                    "up" (list horizontal (- depth value))))
                [0 0] (get_commands filename))]
    (* horizontal depth)))

(calculateDepth "data.txt")
(calculateDepth "puzzle.txt")

(defn calculateAim [filename]
  (let [[horizontal depth]
        (reduce (fn [[horizontal depth aim]
                     [command value]]
                  (case command
                    "forward" (list (+ horizontal value) (+ depth (* aim value)) aim)
                    "down" (list horizontal depth (+ aim value))
                    "up" (list horizontal depth (- aim value))))
                [0 0 0] (get_commands filename))]
    (* horizontal depth)))

(calculateAim "data.txt")
(calculateAim "puzzle.txt")