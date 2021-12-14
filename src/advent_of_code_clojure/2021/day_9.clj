(ns advent-of-code-clojure.2021.day-9
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (mapv #(->> (string/split % #"")
                   (mapv read-string)))))

(def input
  (-> (inputs/get-input-for-day 2021 9)
      (parse-input)))

(def test-input
  (parse-input "2199943210
3987894921
9856789892
8767896789
9899965678"))


; Part 1

(defn get-coord [m x y]
  (nth (nth m y) x))

(defn printr [x]
  (println x)
  x)

(defn get-all-neighbours [m x y]
  (let [maxx (dec (count (first m)))
        maxy (dec (count m))]
    (cond-> []
      (> y 0) (conj [x (dec y)])
      (< y maxy) (conj [x (inc y)])
      (> x 0) (conj [(dec x) y])
      (< x maxx) (conj [(inc x) y]))))

(defn is-low-point? [m x y]
  (let [this (get-coord m x y)]
    (->> (get-all-neighbours m x y)
         (map (fn [[x y]] (get-coord m x y)))
         (every? #(< this %)))))

(comment
  (is-low-point? test-input 6 4))

(defn all-coords [m]
  (let [xrange (range 0 (count (first m)))
        yrange (range 0 (count m))]
    (for [x xrange y yrange]
      [x y])))

(defn find-all-low-points [m]
  (filter (fn [[x y]] (is-low-point? m x y))
          (all-coords m)))

(comment
  (find-all-low-points test-input))

(defn risk-level [m x y]
  (inc (get-coord m x y)))

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
                                   (into acc (get-all-neighbours m x y)))
                                 basin)
                         (filter (fn [[x y]] (< (get-coord m x y) 9)))
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