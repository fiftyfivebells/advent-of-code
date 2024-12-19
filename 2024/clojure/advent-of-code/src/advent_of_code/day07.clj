(ns advent-of-code.day07
  (:require [clojure_utils.utils :as u]))

(defn transform-input
  [file]
  (->> file
       u/read-file
       u/to-lines
       (map u/strings->longs)))

(def || (comp parse-long str))

(defn is-valid?
  "Takes in a boolean which determines whether to use the || operator, and then a list where the
  first item is the total and the rest of the items are operands in the equation. It recursively
  checks the different operators to see if the operands can be combined into the total."
  [concat? [total a b & others]]
  (let [check-operation (fn [operator]
                          (let [new-first (operator a b)]
                            (is-valid? concat? (conj others new-first total))))]
    (if (nil? b)
      (when (= total a)
        total)
      (or (check-operation +)
          (check-operation *)
          (when concat?
            (check-operation ||))))))


(defn do-calculations
  [input concat?]
  (->> input
       transform-input
       (pmap #(is-valid? concat? %))
       (filter some?)
       (reduce +)))

(defn day-7-part-1
  [input]
  (do-calculations input false))

(defn day-7-part-2
  [input]
  (do-calculations input true))

(day-7-part-1 "day07.txt")
(day-7-part-2 "day07.txt")
