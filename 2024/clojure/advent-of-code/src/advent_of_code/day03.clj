(ns advent-of-code.day03
  (:require [clojure_utils.utils :as u]))

(def sample (u/read-file "day03test.txt"))

(def pattern (re-pattern "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)"))

(defn multiply
  [[_ a b]]
  (* (parse-long a) (parse-long b)))

(defn day03-part-1
  [filename]
  (->> (u/read-file filename)
       (re-seq pattern)
       (map multiply)
       (reduce +)))

(defn filter-numbers
  [state acc list-of-instructions]
  (let [instruction (first list-of-instructions)]
    (cond
      (empty? list-of-instructions) acc
      (= (first instruction) "do()") (filter-numbers :on acc (rest list-of-instructions))
      (= (first instruction) "don't()") (filter-numbers :off acc (rest list-of-instructions))
      :else (if (= state :on)
              (filter-numbers state (+ acc (multiply instruction)) (rest list-of-instructions))
              (filter-numbers state acc (rest list-of-instructions))))))

(defn day03-part-2
  [filename]
  (->> (u/read-file filename)
       (re-seq pattern)
       (filter-numbers :on 0)))

(day03-part-1 "day03.txt")
(day03-part-2 "day03.txt")
