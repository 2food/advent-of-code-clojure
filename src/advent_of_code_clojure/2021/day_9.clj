(ns advent-of-code-clojure.2021.day-9
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [advent-of-code-clojure.utils :as utils]))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (mapv #(->> (string/split % #"")
                   (mapv read-string)))))

(def input
  (-> (inputs/get-input-for-this-day)
      (parse-input)))

(def test-input
  (parse-input "2199943210
3987894921
9856789892
8767896789
9899965678"))


; Part 1

(defn is-low-point? [m x y]
  (let [this (utils/get-coord m x y)]
    (->> (utils/get-neighbours-4 m x y)
         (map (fn [[x y]] (utils/get-coord m x y)))
         (every? #(< this %)))))

(defn find-all-low-points [m]
  (filter (fn [[x y]] (is-low-point? m x y))
          (utils/all-coords m)))

(defn risk-level [m x y]
  (inc (utils/get-coord m x y)))

(defn risk-of-all-low-points [m]
  (let [low-points (find-all-low-points m)]
    (->> low-points
         (map (fn [[x y]] (risk-level m x y)))
         (reduce +))))

(comment (risk-of-all-low-points input))
; Answer = 456


; Part 2

(defn find-basin [m low-point]
  (loop [basin (set [low-point])]
    (let [new-basin (->> basin
                         (reduce (fn [acc [x y]]
                                   (into acc (utils/get-neighbours-4 m x y)))
                                 basin)
                         (filter (fn [[x y]] (< (utils/get-coord m x y) 9)))
                         (set))]
      (if (= basin new-basin)
        basin
        (recur new-basin)))))

(defn mult-top-3-basins [inp]
  (->> (find-all-low-points inp)
       (map #(find-basin inp %))
       (map count)
       (sort)
       (reverse)
       (take 3)
       (reduce *)))

(comment
  (mult-top-3-basins input))
; Answer = 1047744