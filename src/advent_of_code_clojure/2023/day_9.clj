(ns advent-of-code-clojure.2023.day-9
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (mapv #(mapv parse-long %))))

(def example
  (->> "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
       (inputs/lines-and-words)
       (mapv #(mapv parse-long %))))

;; Part 1

(defn diffs [nums]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 nums)))

(defn all-diffs [nums]
  (loop [nums        nums
         known-diffs []]
    (if (or (every? zero? nums) (empty? nums))
      (conj known-diffs nums)
      (let [new-nums (diffs nums)]
        (recur new-nums (conj known-diffs nums))))))

(defn extrapolate [nums]
  (->> (all-diffs nums)
       (map last)
       (reverse)
       (reduce + 0)))


(comment

  (->> (map extrapolate example)
       (reduce +))

  (->> (map extrapolate input)
       (reduce +))

  ; Answer = 1479011877

  )

;; Part 2


(comment

  (->> (map reverse example)
       (map extrapolate)
       (reduce +))

  (->> (map reverse input)
       (map extrapolate)
       (reduce +))

  ; Answer = 973

  )

