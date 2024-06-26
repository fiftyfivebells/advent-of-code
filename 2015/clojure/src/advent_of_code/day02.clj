(ns advent-of-code.day02
  (:require
   [clojure_utils.utils :as u]
   [clojure.string :as str]))

(defn third
  [l]
  (nth l 2))

(defn string->dimensions
  [s]
  (map parse-long (str/split s #"x")))

(defn calculate-wrapping
  [[length width height]]
  (let [x (* length width)
        y (* length height)
        z (* width height)]
    (+ (* 2 (+ x y z))
       (reduce min [x y z]))))

(defn day-02-part-1
  [filename]
  (->> (u/read-file filename)
       (u/to-lines)
       (map string->dimensions)
       (map calculate-wrapping)
       (reduce +)))

(defn calculate-ribbon
  [[length width height]]
  (+ (* 2 length)
     (* 2 width)
     (* length width height)))

(defn day-02-part-2
  [filename]
  (->> (u/read-file filename)
       (u/to-lines)
       (map string->dimensions)
       (map sort)
       (map calculate-ribbon)
       (reduce +)))

(day-02-part-1 "day02.txt")
(day-02-part-2 "day02.txt")

(reduce min '(4 2 6))
