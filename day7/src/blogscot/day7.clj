(ns blogscot.day7
  (:gen-class)
  (:require [clojure.string :as str]))

(defn power-rate [align-to positions]
  (map #(Math/abs (- % align-to)) positions))

(defn power-rate' [align-to positions]
  (map #(apply + (range (inc (Math/abs (- % align-to))))) positions))

(defn solver [filename power-rate]
  (let [positions (map read-string (str/split (slurp filename) #","))
        calculate-fuel #(apply + (power-rate % positions))
        start (apply min positions)
        end (apply max positions)
        costs (for [pos (range start end)]
                [pos (calculate-fuel pos)])
        min-cost (reduce (fn [[_ cost-old :as old] [_ cost :as new]]
                           (if (< cost cost-old) new old)) costs)]
    min-cost))

(solver "data.txt" power-rate)
(time (solver "puzzle.txt" power-rate)) ;; "Elapsed time: 10906.162868 msecs"

(solver "data.txt" power-rate')
(time (solver "puzzle.txt" power-rate')) ;; "Elapsed time: 31990.655793 msecs"

;; Solutions are slow. Can I go faster?

(defn power-fn [align-to positions n]
  (Math/abs (- align-to (nth positions n))))

(defn power-fn' [align-to positions n]
  (apply + (range (inc (Math/abs (- align-to (nth positions n)))))))

(defn calc [positions align-to current-min power-fn]
  (let [limit (count positions)]
    (loop [n 0
           total 0]
      (if (or (> total current-min) (= n limit))
        total
        (recur (inc n) (+ total (power-fn align-to positions n)))))))

(defn solve-faster [filename power-fn]
  (let [positions (map read-string (str/split (slurp filename) #","))
        min (apply min positions)
        max (apply max positions)]
    (loop [n min
           current-min Integer/MAX_VALUE
           min-index 0
           index 0]
      (if (= n max)
        [min-index current-min]
        (let [new-min (calc positions n current-min power-fn)]
          (if (< new-min current-min)
            (recur (inc n) new-min index (inc index))
            (recur (inc n) current-min min-index (inc index))))))))

(time (solve-faster "puzzle.txt" power-fn)) ;; "Elapsed time: 14114.84124 msecs"
(time (solve-faster "puzzle.txt" power-fn')) ;; "Elapsed time: 20299.091633 msecs"

;; When the power rate is linear the second algorithm is slower, possibly because the in-built map 
;; function is more efficient. However, with the corrected power-rate, having the calc function 
;; return early when a no new mininum is possible improves the performance considerably. 
;; In summary, not the result I was hoping for, but time to move on.