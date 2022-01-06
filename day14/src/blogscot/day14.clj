(ns blogscot.day14
  (:gen-class)
  (:require [clojure.string :as str]))

(defrecord Pair [count left right])

(defn into-pairs [letters]
  (map (fn [[a b]] (->Pair 1 a b)) (partition-all 2 1 letters)))

(defn get-insertion-rules [raw-rules]
  (apply merge-with into
         (map (comp (fn [[k v]] (assoc {} (seq k) (first v)))
                    (partial drop 1) first #(re-seq #"(\S+) -> (\S)" %)) raw-rules)))

(defn mutate [raw-rules pairs]
  (let [pair-insertion-rules (get-insertion-rules raw-rules)]
    (flatten (map (fn [{:keys [count left right] :as pair}]
                    (if (nil? right)
                      pair
                      (let [new-element (pair-insertion-rules [left right])]
                        [(->Pair count left new-element)
                         (->Pair count new-element right)]))) pairs))))

(defn update-pairs [pairs]
  (map (fn [[pair freq]] (assoc pair :count (* (:count pair) freq))) (frequencies pairs)))

(defn nth-mutation [lines steps]
  (loop [n 0
         polymer (into-pairs (first lines))]
    (if (= n steps)
      (let [mutations (apply merge-with +
                             (map (fn [{:keys [count left]}] (assoc {} left count)) polymer))
            freqs (sort (vals mutations))]
        (- (last freqs) (first freqs)))
      (recur (inc n) (update-pairs (mutate (drop 2 lines) polymer))))))

(defn solver [filename steps]
  (let [lines (str/split-lines (slurp filename))]
    (nth-mutation lines steps)))

(solver "puzzle.txt" 10) ;; part 1
(solver "puzzle.txt" 40) ;; part 2