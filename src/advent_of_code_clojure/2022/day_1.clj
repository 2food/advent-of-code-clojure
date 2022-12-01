(ns advent-of-code-clojure.2022.day-1
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (reduce (fn [acc x]
                 (if (empty? x)
                   (conj acc [])
                   (update acc (dec (count acc)) into [(parse-long x)])))
               [[]])))

;; Part 1

(defn sum [x] (reduce + x))

(comment
  (->> input
       (map sum)
       (apply max))
  ; Answer = 71924
  )

;; Part 2

(comment
  (->> input
       (map sum)
       (sort)
       (take-last 3)
       (sum))
  ; Answer = 210406
  )

