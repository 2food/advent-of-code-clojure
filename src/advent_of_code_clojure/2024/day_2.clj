(ns advent-of-code-clojure.2024.day-2
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/read-matrix)))

;; Part 1

(defn incline [level]
  (map - (rest level) (butlast level)))

(defn level-safe? [level]
  (let [i (incline level) ]
    (and (every? #(<= 1 (abs %) 3) i)
         (or (every? neg? i)
             (every? pos? i)))))

(comment
  input
  (map incline input)
  (count (filter level-safe? input))
  )

; Answer = 236

;; Part 2

(defn alternatives [level]
  (for [n (range (count level))]
    (vec (concat (subvec level 0 n)
                 (subvec level (inc n))))))

(defn safe-w-problem-dampener? [level]
  (some level-safe? (alternatives level)))

(comment
  input
  (safe-w-problem-dampener? [92 94 97 98 97])
  (count (filter (some-fn level-safe? safe-w-problem-dampener?)
                 input))
  
  )

; Answer = 308
