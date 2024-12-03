(ns advent-of-code.day02
  (:require [clojure_utils.utils :as u]))

(defn transform-input
  [input]
  (->> input
       u/read-file
       u/to-lines
       (mapv u/strings->longs)))

(def test (transform-input "day02test.txt"))


(defn all-increasing
  [lst]
  (apply < lst))

(defn all-decreasing
  [lst]
  (apply > lst))

(defn report-sequence-safe?
  [report]
  (or (all-increasing report)
      (all-decreasing report)))

(defn diff-less-than-4?
  [[a b]]
  (< (abs (- b a)) 4))

(defn valid-differences?
  [report]
  (let [pairs (partition 2 1 report)]
    (every? diff-less-than-4? pairs)))

(defn report-safe?
  [report]
  (and (report-sequence-safe? report)
       (valid-differences? report)))

(defn day02-part-1
  [input]
  (->> input
       transform-input
       (filter report-safe?)
       count))

(def unsafe
  (->> test
       (filter (complement report-safe?))))

(day02-part-1 "day02.txt")
