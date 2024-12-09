(ns advent-of-code-clojure.2024.day-8
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.set :as set]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv vec)))

;; Part 1

(defn antennas [m]
  (apply merge-with set/union
         (keep (fn [[x y]]
                 (let [val (utils/get-coord m x y)]
                   (when (not= \. val)
                     {val #{[x y]}})))
               (utils/all-coords m))))

(defn antinodes [[ax ay] [bx by]]
  (let [diff-x (- bx ax)
        diff-y (- by ay)]
    [[(- ax diff-x) (- ay diff-y)]
     [(+ bx diff-x) (+ by diff-y)]]))

(defn combinations [op x & others]
  (if-not (and x others)
    #{}
    (into (set (map #(op x %) others))
          (apply combinations op others))))

(comment
  (antennas input)

  (apply combinations antinodes [[3 3] [5 5] [5 3] [7 9]])

  (->> (antennas input)
       (mapcat (fn [[k vals]] (apply concat (apply combinations antinodes vals))))
       (set)
       (remove #(utils/out-of-bounds? input %))
       (count))

  ; Answer = 269
  )

;; Part 2

(defn antinodes2 [[ax ay] [bx by]]
  (let [diff-x (- bx ax)
        diff-y (- by ay)]
    (vec (concat (map (fn [amp] [(- ax (* amp diff-x)) (- ay (* amp diff-y))]) (range 50))
                 (map (fn [amp] [(+ bx (* amp diff-x)) (+ by (* amp diff-y))]) (range 50))))))


(comment

  (antinodes2 [3 3] [5 5])

  (->> (antennas input)
       (mapcat (fn [[k vals]] (apply concat (apply combinations antinodes2 vals))))
       (set)
       (remove #(utils/out-of-bounds? input %))
       (count))

  )
