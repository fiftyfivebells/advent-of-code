(ns advent-of-code.day04
  (:require [clojure_utils.utils :as u]))

(def letter-map {\X \M
                 \M \A
                 \A \S})

(def directions {:north     [-1 0]
                 :northeast [-1 1]
                 :east      [0 1]
                 :southeast [1 1]
                 :south     [1 0]
                 :southwest [1 -1]
                 :west      [0 -1]
                 :northwest [-1 -1]})

(defn get-next-position
  [start direction]
  (let [dxdy (directions direction)]
    (mapv + start dxdy)))

(defn letter-in-direction
  [matrix start direction]
  (let [next-position (get-next-position start direction)
        next-letter (get-in matrix next-position)]
    (when next-letter
      next-letter)))

(defn get-current-letter
  [matrix position]
  (get-in matrix position))

(defn valid-letters-in-direction
  [matrix start direction letter]
  (let [next-letter (letter-map letter)
        next-position (get-next-position start direction)
        at-next-coord (letter-in-direction matrix start direction)]
    (cond
      (= at-next-coord next-letter \S) true
      (= at-next-coord next-letter) (valid-letters-in-direction matrix next-position direction next-letter)
      :else false)))

(defn num-xmas-at-point
  [matrix start]
  (let [letter (get-in matrix start)
        directions (map identity (keys directions))]
    (if (= letter \X)
      (count (filter true? (map #(valid-letters-in-direction matrix start % \X) directions)))
      0)))

(defn check-matrix-for-xmas
  [matrix]
  (for [x (range (count matrix))
        y (range (count (first matrix)))
        :when (= (get-current-letter matrix [x y]) \X)]
    (num-xmas-at-point matrix [x y])))

(defn day-04-part-1
  [input]
  (->> input
       u/read-file
       u/to-matrix
       check-matrix-for-xmas
       (reduce +)))

(defn is-center-of-mas?
  [matrix position [d1 d2]]
  (let [l1 (letter-in-direction matrix position d1)
        l2 (letter-in-direction matrix position d2)]
    (or (and (= l1 \M) (= l2 \S))
        (and (= l1 \S) (= l2 \M)))))


(defn check-diagonals-for-mas
  [matrix position]
  (and (is-center-of-mas? matrix position [:northeast :southwest])
       (is-center-of-mas? matrix position [:northwest :southeast])))

(defn find-coords-for-letter
  [matrix letter]
  (for [x (range (count matrix))
        y (range (count (first matrix)))
        :when (= (get-in matrix [x y]) letter)]
    [x y]))

(defn day-04-part-2
  [input]
  (let [matrix (u/to-matrix (u/read-file input))
        all-as (find-coords-for-letter matrix \A)]
    (loop [[a & as] all-as, count 0]
      (if (nil? a)
        count
        (if (check-diagonals-for-mas matrix a)
          (recur as (inc count))
          (recur as count))))))

(day-04-part-1 "day04.txt")
(day-04-part-2 "day04.txt")
