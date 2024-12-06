(ns advent-of-code.day04
  (:require [clojure_utils.utils :as u]))

(def sample (u/to-matrix (u/read-file "day04test.txt")))

(run! println sample)

(get-in sample [0 0])

(defn get-coords-with-match
  [matrix start substring]
  (let [start-x (first start)
        start-y (second start)]
    (for [x (range (dec start-x) (+ 2 start-x))
          y (range (dec start-y) (+ 2 start-y))
          :when (= (get-in matrix [x y]) substring)]
      [x y])))

(def next-letter {\X \M
                  \M \A
                  \A \S})

(defn descend
  [matrix start current-letter]
  (let [coords (get-coords-with-match matrix start (next-letter current-letter))]
    (println coords)
    (cond
      (empty? coords) 0
      ;; (= current-letter \X) (reduce + (map #(descend matrix % \M) coords))
      ;; (= current-letter \M) (reduce + (map #(descend matrix % \A) coords))
      ;; (= current-letter \A) (reduce + (map #(descend matrix % \S) coords))
      (= current-letter \S) 1
      :else
      (reduce + (map #(descend matrix % current-letter) coords)))))

(descend sample [0 0] (get-in sample [0 0]) 0)
(get-coords-with-match sample [0 0] \X)

(for [x (range (count (first sample)))
      y (range (count sample))
      :when (= (get-in sample [x y]) \X)]
  (descend sample [x y] (get-in sample [x y]) 0))

(descend sample [0 4] \X)
(map #(descend sample % \A) '([1 3] [1 5]))
(descend sample [1 5] \A)

(map-indexed vector "XMAS")
