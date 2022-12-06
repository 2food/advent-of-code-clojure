(ns advent-of-code-clojure.2022.day-6
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (drop-last)))

;; Part 1

(defn find-start [input package-len]
  (->> (partition package-len 1 input)
       (map-indexed (fn [ind e] [ind e]))
       (some (fn [[ind e]] (and (= package-len (count (set e))) (+ package-len ind))))))


(comment
  (find-start input 4)
  ;; Answer = 1850
  )

;; Part 2

(comment
  (find-start input 14)
  ;; Answer = 2823
  ) 

