(ns advent-of-code-clojure.2024.day-3
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (inputs/get-input-for-this-day))

;; Part 1

(defn mult-and-sum [muls]
  (->> muls
       (map (fn [[n m]] (* n m)))
       (reduce +)))

(comment
  (->> (re-seq #"mul\((\d+),(\d+)\)" input)
       (map (fn [[_ n m]] [(parse-long n) (parse-long m)]))
       (mult-and-sum))
  )

; Answer = 183788984

;; Part 2


(comment
  (->> (re-seq #"do\(\)|don't\(\)|mul\((\d+),(\d+)\)" input)
       (partition-by #(or (= (first %) "do()")
                          (= (first %) "don't()")))
       (reduce (fn [{:keys [do muls] :as acc} list]
                 (case (ffirst list)
                   nil acc
                   "do()" (assoc acc :do true)
                   "don't()" (assoc acc :do false)
                   (if do
                     (assoc acc :muls (concat muls list))
                     acc)))
               {:do   true
                :muls []})
       :muls
       (map (fn [[_ n m]] [(parse-long n) (parse-long m)]))
       (mult-and-sum))
  )

;; Answer = 62098619
