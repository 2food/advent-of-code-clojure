(ns advent-of-code-clojure.2025.day-6
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.string :as str]))

(def example-input
  (inputs/lines "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "))

(def input
  (inputs/lines (inputs/get-input-for-this-day)))

;; Part 1


(defn parse-word [s]
  (cond
    (= "*" s) :*
    (= "+" s) :+
    :else (parse-long s)))

(defn parse-input [s]
  (->> s
       (mapv inputs/words)
       (mapv #(vec (keep parse-word %)))))

(defn calc [& args]
  (let [nums (drop-last args)
        op (last args)]
    (case op
      :* (apply * nums)
      :+ (apply + nums))))

(comment

  (->> (parse-input example-input)
       (apply map calc)
       (reduce +))

  (->> (parse-input input)
       (apply map calc)
       (reduce +))

  ; Answer = 4583860641327

  )

;; Part 2

(defn number-groups [input]
  (->> input
       (drop-last)
       (utils/transpose)
       (map (comp parse-long str/trim str/join))
       (partition-by some?)
       (remove (comp nil? first))
       (mapv vec)))

(defn ops [input]
  (->> (last input)
       (inputs/words)
       (keep parse-word)))


(comment
  (let [inp example-input]
    (->> (map (fn [nums op]
                (case op
                  :* (apply * nums)
                  :+ (apply + nums)))
              (number-groups inp)
              (ops inp))
         (reduce +)))

  (let [inp input]
    (->> (map (fn [nums op]
                (case op
                  :* (apply * nums)
                  :+ (apply + nums)))
              (number-groups inp)
              (ops inp))
         (reduce +)))

  ; Answer = 11602774058280

  )

