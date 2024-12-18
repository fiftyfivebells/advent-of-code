(ns advent-of-code.day06
  (:require [clojure_utils.utils :as u]))

(def input (u/to-matrix (u/read-file "day06test.txt")))

(def direction {:north [-1 0]
                :east [0 1]
                :south [1 0]
                :west [0 -1]})

(def next-dir {:north :east
               :east :south
               :south :west
               :west :north})

(defn find-in-matrix
  [char matrix]
  (first (for [[x line] (map-indexed vector matrix)
               [y c] (map-indexed vector line)
               :when (= c char)]
           [x y])))

(defn take-step
  [start dir]
  (let [dxdy (direction dir)]
    (map + start dxdy)))

(defn count-guard-steps
  [start matrix]
  (loop [position start, dir :north, seen #{}]
    (let [next-char (get-in matrix (take-step position dir))
          seen' (conj seen position)]
      (cond
        (nil? next-char) seen'
        (= next-char \#) (recur position (next-dir dir) seen')
        :else (recur (take-step position dir) dir seen')))))

(defn day-6-part-1
  [input]
  (let [matrix (u/to-matrix (u/read-file input))
        start (find-in-matrix \^ matrix)]
    (count (count-guard-steps start matrix))))

(def positions (count-guard-steps [6 4] input))

(defn is-loop?
  [matrix start obstacle]
  (loop [position start, direction :north, seen #{}]
    (let [next-char (get-in matrix (take-step position direction))
          next-pos (take-step position direction)
          seen' (conj seen [position direction])]
      (cond
        (nil? next-char) false
        (contains? seen [position direction]) true
        (or (= next-char \#)
            (= next-pos obstacle)) (recur position (next-dir direction) seen')
        :else (recur next-pos direction seen')))))

(defn day-6-part-2
  [input]
  (let [matrix (u/to-matrix (u/read-file input))
        start (find-in-matrix \^ matrix)
        path (count-guard-steps start matrix)]
    (->> path
         (pmap #(is-loop? matrix start %))
         (remove false?)
         count)))

(day-6-part-1 "day06.txt")
(day-6-part-2 "day06.txt")
