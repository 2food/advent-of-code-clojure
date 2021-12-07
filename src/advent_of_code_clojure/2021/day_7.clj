(ns advent-of-code-clojure.2021.day-7
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> (string/split s #"[,\n]")
       (mapv read-string)))

(defn input [] 
  (-> (inputs/get-input-for-day 2021 7)
      (parse-input)))

(defn sum-of-diffs [inp target]
  (reduce + (mapv #(Math/abs (- target %)) inp)))


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
  (find-nearest-point (input)))
; Answer = 353800
