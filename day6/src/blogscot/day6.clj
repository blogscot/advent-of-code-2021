(ns blogscot.day6
  (:gen-class)
  (:require [clojure.string :as str]))

(defn solve [filename days]
  (let [laternfish (map read-string (str/split (slurp filename) #","))
        domain (vec (repeat 9 0))
        freqs (map seq (frequencies laternfish))
        initial-state (reduce (fn [states [pos val]]
                                (update-in states [pos] #(+ % val))) domain freqs)]
    (loop [n (dec days)
           state initial-state]
      (let [day0 (first state)
            new-state (vec (rest (conj (update-in state [7] #(+ % day0)) day0)))]
        (if (zero? n)
          (apply + new-state)
          (recur (dec n) new-state))))))

(solve "puzzle.txt" 80)
(solve "puzzle.txt" 256)
