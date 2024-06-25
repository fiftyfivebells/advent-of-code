(ns advent-of-code.day01
  (:require [clojure_utils.utils :as u]))

(def stair-value-map {\( 1 \) -1})

(defn day-01-part-1
  [filename]
  (->> (u/read-file filename)
       (map #(stair-value-map %))
       (reduce +)))

(defn day-01-part-2
  [filename]
  (->> (u/read-file filename)
       (map #(stair-value-map %))
       (reductions + 0)
       (take-while #(not= % -1))
       (count)))

(day-01-part-1 "day01.txt")
(day-01-part-2 "day01.txt")
