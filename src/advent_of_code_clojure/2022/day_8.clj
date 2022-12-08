(ns advent-of-code-clojure.2022.day-8
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv #(mapv (comp parse-long str) %))))

(def test-input
  (->> "30373\n25512\n65332\n33549\n35390"
       (inputs/lines)
       (mapv #(mapv (comp parse-long str) %))))

;; Part 1

(defn sizes [forest]
  (let [sizey (count forest)
        sizex (count (first forest))]
    [sizey sizex]))

(defn north-of [forest [y x]]
  (for [j (range (dec y) -1 -1)]
    (get-in forest [j x])))

(defn east-of [forest [y x]]
  (let [[_ sizex] (sizes forest)]
    (for [i (range (inc x) sizex)]
      (get-in forest [y i]))))

(defn south-of [forest [y x]]
  (let [[sizey _] (sizes forest)]
    (for [j (range (inc y) sizey)]
      (get-in forest [j x]))))

(defn west-of [forest [y x]]
  (for [i (range (dec x) -1 -1)]
    (get-in forest [y i])))

(defn visible-line? [tree trees]
  (or (empty? trees)
      (every? #(< % tree) trees)))

(defn visible? [forest tree-pos]
  (let [tree (get-in forest tree-pos)]
    (or (visible-line? tree (north-of forest tree-pos))
        (visible-line? tree (east-of forest tree-pos))
        (visible-line? tree (south-of forest tree-pos))
        (visible-line? tree (west-of forest tree-pos)))))

(defn tree-positions [forest]
  (let [[sizey sizex] (sizes forest)]
    (for [y (range 0 sizey)
          x (range 0 sizex)]
      [y x])))

(comment
  (let [forest input]
    (->> (tree-positions forest)
         (filter (partial visible? forest))
         (count)))

  ; Answer = 1843
  )

;; Part 2

(defn score-line [tree line]
  (let [res (count (take-while #(< % tree) line))]
    (if (= res (count line))
      res
      (inc res))))

(defn scenic-score [forest tree-pos]
  (let [tree (get-in forest tree-pos)]
    (* (score-line tree (north-of forest tree-pos))
       (score-line tree (east-of forest tree-pos))
       (score-line tree (south-of forest tree-pos))
       (score-line tree (west-of forest tree-pos)))))

(comment
  (let [forest input]
    (->> (tree-positions forest)
         (apply max-key #(scenic-score forest %))
         (scenic-score forest)))

  ; Answer = 180000
  ) 

