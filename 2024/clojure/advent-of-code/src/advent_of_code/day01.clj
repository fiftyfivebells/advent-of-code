(ns advent-of-code.day01
  (:require [clojure_utils.utils :as u]))

;; Takes the input (a list of strings separated by spaces within each line with a newline between lines)
;; and turns it into a list of lists of longs. The transpose at the end turns this from a long list of
;; pairs of integers to a length two list of integers.
(defn transform-input
  [input]
  (->> input
       u/read-file
       u/to-lines
       (mapv u/strings->longs)
       u/transpose-matrix))

(defn pair-difference
  [[a b]]
  (abs (- a b)))

;; Used in the reduce function, this just gets the difference between the two numbers in a pair
;; and then adds them to the accumulator.
(defn reducer
  [acc pair]
  (+ acc (pair-difference pair)))

;; This function takes a list of pairs of integers and returns the sum of the absolute differences
(defn total-distance
  [pairs]
  (let [left (sort (first pairs))
        right (sort (second pairs))
        zipped (map vector left right)]
    (reduce reducer 0 zipped)))


(defn day01-part-1
  [input]
  (->> input
       transform-input
       total-distance))

(defn similarity-score
  [freq val]
  (* val (or (freq val) 0)))

(defn day01-part-2
  [input]
  (let [pairs (transform-input input)
        freqs (frequencies (second pairs))]
    (->> (first pairs)
         (map #(similarity-score freqs %))
         (reduce +))))

(day01-part-1 "day01.txt")
(day01-part-2 "day01.txt")

