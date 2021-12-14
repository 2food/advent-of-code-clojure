(ns advent-of-code-clojure.2021.day-3
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(def input
  (-> (inputs/get-input-for-day 2021 3)
      (inputs/lines)))

; Part 1

(defn group-bits-by-position [diagnostic-report]
  (apply (partial map vector) diagnostic-report))

(defn most-common-bits [grouped-bits]
  (let [bit-freqs (map frequencies grouped-bits)]
    (for [bit-freq bit-freqs]
      (let [ones (if (bit-freq \1) (bit-freq \1) 0)
            zeros (if (bit-freq \0) (bit-freq \0) 0)]
        (if (>= ones zeros)
          1
          0)))))

(defn invert [b]
  (if (= b 1) 0 1))

(defn get-rates-1 [diagnostic-report]
  (let [grouped-bits (group-bits-by-position diagnostic-report)
        most-common (most-common-bits grouped-bits)
        gamma (-> most-common
                  (string/join)
                  (Long/parseUnsignedLong 2))
        epsilon (-> (map invert most-common)
                    (string/join)
                    (Long/parseUnsignedLong 2))]
    [gamma epsilon]))

(comment
  (get-rates-1 input)
  (apply * (get-rates-1 input)))
; Answer = 3958484


; Part 2

(defn filter-matching [diagnostic-report invert?]
  (loop [index 0
         bits diagnostic-report]
    (let [key (most-common-bits (group-bits-by-position bits))
          key (cond->> key
                invert? (map invert)
                true (string/join))
          new-bits (filterv #(= (nth % index) (nth key index)) bits)]
      (if (> (count new-bits) 1)
        (recur (inc index) new-bits)
        (first new-bits)))))


(defn get-rates-2 [diagnostic-report]
  (let [oxy-gen-rating (->  (filter-matching diagnostic-report false)
                            (Long/parseUnsignedLong 2))
        co2-scrub-rating (-> (filter-matching diagnostic-report true)
                             (Long/parseUnsignedLong 2))]
    [oxy-gen-rating co2-scrub-rating]))

; Test
(comment
  (let [sample  ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"]]
    (= (get-rates-2 sample) [23 10])))

(comment
  (get-rates-2 input)
  (apply * (get-rates-2 input)))
; Answer = 1613181