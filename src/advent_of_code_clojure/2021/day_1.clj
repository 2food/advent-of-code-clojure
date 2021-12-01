(ns advent-of-code-clojure.2021.day-1
  (:require [advent-of-code-clojure.inputs :as inputs]))

(defn input [] (inputs/read-longs (inputs/get-input-for-day 2021 1)))

(defn greater-than-prev? [nums]
  (let [rest-nums (rest nums)]
    (map < nums rest-nums)))

(comment
  (= (greater-than-prev? [1 2 2 4 1]) [true false true false]))

(defn n-greater-than-prev [nums]
  (count (filter identity (greater-than-prev? nums))))

(comment
  (n-greater-than-prev (input)))


