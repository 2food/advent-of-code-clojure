(ns advent-of-code-clojure.2021.day-3
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn input []
  (-> (inputs/get-input-for-day 2021 3)
      (inputs/lines)))

; Part 1

(defn group-bits-by-position [diagnostic-report]
  (apply (partial map vector) diagnostic-report))

(defn most-common-bits [grouped-bits]
  (let [bit-freqs (map frequencies grouped-bits)]
    (for [bit-freq bit-freqs]
      (if (> (bit-freq \1) (bit-freq \0))
        [1 0]
        [0 1]))))

(defn get-rates [diagnostic-report]
  (let [grouped-bits (group-bits-by-position diagnostic-report)
        most-common (most-common-bits grouped-bits)
        gamma (-> (map first most-common)
                  (string/join)
                  (Long/parseUnsignedLong 2))
        epsilon (-> (map second most-common)
                    (string/join)
                    (Long/parseUnsignedLong 2))]
    [gamma epsilon]))

(comment
 (apply * (get-rates (input))))
; Answer = 3958484