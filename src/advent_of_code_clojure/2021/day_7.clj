(ns advent-of-code-clojure.2021.day-7
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> (string/split s #"[,\n]")
       (mapv read-string)))

(def input
  (-> (inputs/get-input-for-day 2021 7)
      (parse-input)))


; Part 1

(defn sum-of-diffs [inp target]
  (->> inp
       (mapv #(Math/abs (- target %)))
       (reduce +)))


(defn find-nearest-point [inp]
  (loop [n 0
         m 1000000]
    (let [new (sum-of-diffs inp n)]
      (if (= n (count inp))
        m
        (if (< new m)
          (recur (inc n) new)
          (recur (inc n) m))))))

(comment
  (find-nearest-point input))
; Answer = 353800


; Part 2

(defn arithmetic-sum [start end nterms]
  (* nterms (/ (+ start end) 2)))


(defn cost [inp target]
  (->> inp
       (map #(Math/abs (- target %)))
       (map #(arithmetic-sum 1 % %))
       (reduce +)))


(defn find-nearest-point-2 [inp]
  (loop [n 0
         m ##Inf]
    (let [new (cost inp n)]
      (if (= n (count inp))
        m
        (if (< new m)
          (recur (inc n) new)
          (recur (inc n) m))))))

(comment
  (find-nearest-point-2 input))
; Answer = 98119739