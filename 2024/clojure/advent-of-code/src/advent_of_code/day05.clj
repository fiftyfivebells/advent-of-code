(ns advent-of-code.day05
  (:require [clojure_utils.utils :as u]
            [clojure.string :as str]))

(defn block->usable
  "Takes in a block of text and turns it into lines of numbers"
  [block]
  (->> block
       u/to-lines
       (map u/strings->longs)))

(defn rules->rule-sets
  "Takes the list of rules and makes a map where each key is a number and the
  values are sets of numbers that need to come before the key."
  [acc [k v]]
  (assoc acc k (conj (get acc k #{}) v)))

(defn create-rule-sets
  [rules]
  (reduce rules->rule-sets {} rules))

(defn get-rules-and-pages
  "Turns the puzzle input into a list of rule sets and the pages that need to be updated."
  [blocks]
  (let [rules (block->usable (first blocks))
        pages (block->usable (second blocks))]
    (list (create-rule-sets rules) pages)))

(defn middle-value
  [lst]
  (let [length (count lst)
        index (/ length 2)]
    (nth lst index)))


(defn pages-in-order?
  "Loops through an update and checks to see if its in order. It does this by comparing the current
  page in the list to the pages seen before. If any rules are violated, it returns false. If it
  gets to the end of the list with no violations, it's a valid update."
  [[rule-sets pages]]
  (loop [[page & rest-pages] pages, seen #{}]
    (let [page-before (rule-sets page)]
      (cond
        (nil? page) true
        (some seen page-before) false
        :else (recur rest-pages (conj seen page))))))

(defn filter-pages-in-order
  [[rule-sets pages]]
  (filter #(pages-in-order? [rule-sets %]) pages))

(defn day-5-part-1
  [input]
  (->> input
       u/read-file
       u/to-blocks
       get-rules-and-pages
       filter-pages-in-order
       (map middle-value)
       (reduce +)))

(defn filter-pages-out-of-order
  [[rule-sets pages]]
  (remove #(pages-in-order? [rule-sets %]) pages))

(defn fix-update
  "Goes through an out of order update and rebuilds it in order. It does this by comparing the
  current number with a list being built with previously seen numbers. If the current number needs
  to be before any of the previous numbers, this inserts the number in its rightful place. Otherwise,
  it just conjs the new number onto the accumulator."
  [rules update]
  (letfn [(put-page-in-place
            [pages page]
            (let [page-rules (get rules page #{})
                  position (first (for [n (range (count pages))
                                        :when (page-rules (nth pages n))]
                                    n))]
              (if position
                (into [] (concat (take position pages)
                                 (list page)
                                 (drop position pages)))
                (conj pages page))))]
    (reduce put-page-in-place [] update)))

(defn day-5-part-2
  [input]
  (let [[rules updates] (->> input
                             u/read-file
                             u/to-blocks
                             get-rules-and-pages)
        out-of-order (filter-pages-out-of-order [rules updates])]
    (->> out-of-order
         (map #(fix-update rules %))
         (map middle-value)
         (reduce +))))

(day-5-part-1 "day05.txt")
(day-5-part-2 "day05.txt")
