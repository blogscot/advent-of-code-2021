(ns blogscot.day14
  (:gen-class)
  (:require [clojure.string :as str]))

(defn into-pairs [letters]
  (map (fn [[a b]] (assoc {} :count 1 :left a :right b)) (partition-all 2 1 letters)))

(defn get-insertion-rules [raw-rules]
  (apply merge-with into
         (map (comp (fn [[k v]] (assoc {} (seq k) (first v)))
                    (partial drop 1) first #(re-seq #"(\S+) -> (\S)" %)) raw-rules)))

(defn mutate [raw-rules pairs]
  (let [pair-insertion-rules (get-insertion-rules raw-rules)]
    (flatten (map (fn [{count :count left :left right :right :as pair}]
                    (if (nil? right)
                      pair
                      (let [new-element (pair-insertion-rules [left right])]
                        [{:count count :left left :right new-element}
                         {:count count :left new-element :right right}]))) pairs))))

(defn update-pairs [pairs]
  (map (fn [[pair freq]] (assoc pair :count (* (pair :count) freq))) (frequencies pairs)))

(defn nth-mutation [lines steps]
  (loop [n 0
         polymer (into-pairs (first lines))]
    (if (= n steps)
      (let [mutations (apply merge-with +
                             (map (fn [{count :count left :left}] (assoc {} left count)) polymer))
            freqs (sort (vals mutations))]
        (- (last freqs) (first freqs)))
      (recur (inc n) (update-pairs (mutate (drop 2 lines) polymer))))))

(defn solver [filename steps]
  (let [lines (str/split-lines (slurp filename))]
    (nth-mutation lines steps)))

(solver "puzzle.txt" 10) ;; part 1
(solver "puzzle.txt" 40) ;; part 2