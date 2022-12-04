(ns advent-of-code-clojure.2022.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv (fn [line] (->> (string/split line #",")
                             (mapv (fn [ranges] (mapv parse-long (string/split ranges #"-")))))))))

;; Part 1 

(defn inside? [[a b] [x y]]
  (and (>= a x) (<= b y)))

(defn fully-overlaps? [[a b]]
  (or (inside? a b) (inside? b a)))

(comment
  (count (filter fully-overlaps? input))
  ;; Answer = 556
  ) 

;; Part 2

(defn somewhat-overlaps? [[[a b] [x y]]]
  (or (<= x a y) (<= x b y)
      (<= a x b) (<= a y b)))

(comment
  (count (filter somewhat-overlaps? input))
  ;; Answer = 876
  ) 

