(ns advent-of-code.day02
  (:require [clojure_utils.utils :as u]))

(defn transform-input
  [input]
  (->> input
       u/read-file
       u/to-lines
       (mapv u/strings->longs)))

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

;; this function takes a report and steps through it one by one. At each step, it rebuilds
;; the report with the number at the current index missing and checks to see if it's safe.
;; if yes, it returns true, and if no it continues the iteration. If it makes it through the
;; whole report without a safe subreport, then I know that this report has more than one
;; bad level and can't be made safe.
(defn only-one-bad-level?
  [report]
  (loop [i (dec (count report))]
    (let [left (subvec report 0 i)
          right (subvec report (inc i))
          new-report (into left right)]
      (cond
        (report-safe? new-report) true
        (zero? i) false
        :else (recur (dec i))))))

(defn report-safe-2?
  [report]
  (or (report-safe? report)
      (only-one-bad-level? report)))

(defn day02-part-1
  [input]
  (->> input
       transform-input
       (filter report-safe?)
       count))

(defn day02-part-2
  [input]
  (->> input
       transform-input
       (map vec)
       (filter report-safe-2?)
       count))

(day02-part-1 "day02.txt")
(day02-part-2 "day02.txt")
