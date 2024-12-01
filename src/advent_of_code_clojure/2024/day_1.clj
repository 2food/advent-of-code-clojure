(ns advent-of-code-clojure.2024.day-1
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (flatten)
       (keep parse-long)
       (partition 2)
       (mapv vec)))

;; Part 1

(def left-list (sort (map first input)))
(def right-list (sort (map second input)))

(comment
  (->> (map - left-list right-list)
       (map abs)
       (reduce +))
  )

; Answer = 1873376


;; Part 2

(def right-freq (frequencies right-list))

(comment
  (reduce + (map #(* % (get right-freq % 0)) left-list))
  )

;; Answer = 18997088
