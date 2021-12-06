(ns blogscot.day3
  (:gen-class)
  (:require [clojure.string :as string]))

(defn parse-line [reading]
  (for [pos (range (count reading))]
    (if (= (nth reading pos) \1)
      1
      -1)))

(defn load-report
  [filename]
  (string/split-lines (slurp filename)))

(defn calculate-total
  [readings]
  (apply map + (map #(parse-line %) readings)))

(defn calculate-rate [predicate positive negative]
  (fn [reading]
    (for [pos (range (count reading))]
      (if (predicate (nth reading pos) 0)
        positive
        negative))))

(defn to-decimal
  "Expects a list of binary digits e.g. '(1 0 1 0 1)"
  [binary]
  (reduce (fn [acc bit]
            (if (pos? bit)
              (+ (* 2 acc) 1)
              (* 2 acc))) 0 binary))

(defn calculate-power-consumption [filename]
  (let [total (calculate-total (load-report filename))
        gamma ((calculate-rate > 1 0) total)
        epsilon ((calculate-rate < 1 0) total)]
    (* (to-decimal #break gamma) (to-decimal epsilon))))

(calculate-power-consumption "data.txt")
(calculate-power-consumption "puzzle.txt")

(defn find-readings [readings n positive negative]
  (nth ((calculate-rate >= positive negative) (calculate-total readings)) n))

(defn find-majority [readings n] (find-readings readings n \1 \0))
(defn find-minority [readings n] (find-readings readings n \0 \1))

(defn filter-readings
  [find-fn
   readings
   position]
  (if (= (count readings) 1)
    (first readings)
    (let [value (find-fn readings position)
          new-readings (filter (fn [reading] (= (nth reading position) value)) readings)]
      (filter-readings find-fn new-readings (inc position)))))

(defn binary-str-to-decimal [binary]
  (to-decimal (map #(Integer/parseInt %) (string/split binary #""))))

(defn life-support-rating [filename]
  (let [readings (load-report filename)
        oxygen-generator-rating (filter-readings find-majority readings 0)
        co2-scrubber-rating (filter-readings find-minority readings 0)]
    (* (binary-str-to-decimal oxygen-generator-rating) (binary-str-to-decimal co2-scrubber-rating))))

(life-support-rating "data.txt")
(life-support-rating "puzzle.txt")