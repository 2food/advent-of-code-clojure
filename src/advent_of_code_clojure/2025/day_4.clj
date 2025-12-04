(ns advent-of-code-clojure.2025.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as ut]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> (inputs/lines s)
       (mapv vec)))

(def example-input
  (->> ["..@@.@@@@."
        "@@@.@.@.@@"
        "@@@@@.@.@@"
        "@.@@@@..@."
        "@@.@@@@.@@"
        ".@@@@@@@.@"
        ".@.@.@.@@@"
        "@.@@@.@@@@"
        ".@@@@@@@@."
        "@.@.@@@.@."]
       (str/join "\n")
       (parse-input)))

(def input
  (->> (inputs/get-input-for-this-day)
       (parse-input)))

;; Part 1

(defn can-lift? [m x y]
  (let [neighbours (ut/get-neighbours-8 m x y)
        rolls      (filter #(= \@ %) (map (fn [[a b]] (ut/get-coord m a b)) neighbours))]
    (< (count rolls) 4)))

(defn removable-coords [m]
  (->> (ut/all-coords m)
       (filter (fn [[x y]] (= \@ (ut/get-coord m x y))))
       (filter (fn [[x y]] (can-lift? m x y)))))

(defn mark-xs [m coords]
  (->> coords
       (reduce (fn [m [x y]] (assoc-in m [y x] \x)) m)))

(comment

  (let [m example-input]
    (mapv str/join (mark-xs m (removable-coords m))))

  (let [m input]
    (->> (removable-coords m)
         (count)))

  ; Answer: 1349

  )

;; Part 2


(comment

  (def example-res (loop [m example-input]
                     (let [new-m (mark-xs m (removable-coords m))]
                       (if (= m new-m)
                         m
                         (recur new-m)))))
  (count (filter #(= % \x) (flatten example-res)))


  (def res (loop [m input]
             (let [new-m (mark-xs m (removable-coords m))]
               (if (= m new-m)
                 m
                 (recur new-m)))))
  (count (filter #(= % \x) (flatten res)))

  ; Answer: 8277

  )

