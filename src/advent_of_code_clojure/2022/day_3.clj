(ns advent-of-code-clojure.2022.day-3
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.set :as cset]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)))

;; Part 1

(defn priority [^Character c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(def shared-items
  (->> input
       (map #(map set (split-at (/ (count %) 2) %)))
       (map (fn [[a b]] (first (cset/intersection a b))))))

(comment
  (->> (map priority shared-items)
       (reduce +))
  ; Answer = 8139
  )

;; Part 2

(def group-badges
  (->> input
       (map set)
       (partition 3)
       (map (fn [[a b c]] (first (cset/intersection a b c))))))

(comment
  (->> (map priority group-badges)
       (reduce +))
  ; Answer = 2668
  )

