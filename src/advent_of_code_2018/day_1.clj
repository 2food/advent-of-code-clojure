(ns advent-of-code-2018.day-1
  (:require [advent-of-code-2018.inputs :as inputs]))


(defn input [] (inputs/read-longs (inputs/get-input-for-day 1)))

; Part 1


(defn freq-sum
  [ints]
  (apply + ints))

(defn answer-1 [] (freq-sum (input)))


; Part 2


(defn detect-first-dup
  [ints]
  (loop [seen-freqs #{0}
         cur-freq  0
         cur-ints  ints]
    (if (empty? cur-ints)
      (recur seen-freqs cur-freq ints)
      (let [new-freq (+ cur-freq (first cur-ints))]
        (if (contains? seen-freqs new-freq)
          new-freq
          (recur (conj seen-freqs new-freq)
                 new-freq
                 (rest cur-ints)))))))

(defn answer-2 [] (detect-first-dup (input)))
