(ns advent-of-code-clojure.2021.day-11
  (:require [advent-of-code-clojure.inputs :as inputs]))

(defn parse-input [s]
  (-> s
      (inputs/lines)))

(def input
  (parse-input
   (inputs/get-input-for-day 2021 11)))

(def test-input
  (parse-input
   "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

input