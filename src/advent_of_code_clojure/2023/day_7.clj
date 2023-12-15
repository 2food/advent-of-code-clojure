(ns advent-of-code-clojure.2023.day-7
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (update-vals
   (->> (inputs/get-input-for-this-day)
        (inputs/lines-and-words)
        (into {}))
   parse-long))

(def example
  (update-vals
   (->> "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
        (inputs/lines-and-words)
        (into {}))
   parse-long))

;; Part 1

(def cards [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])

(def hand-types [:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind :two-pair :one-pair :high-card])

(defn hand-type [hand]
  (let [freqs (vals (frequencies hand))]
    (cond
      (some #{5} freqs) :five-of-a-kind
      (some #{4} freqs) :four-of-a-kind
      (and (some #{3} freqs) (some #{2} freqs)) :full-house
      (some #{3} freqs) :three-of-a-kind
      (= 2 (count (filter #{2} freqs))) :two-pair
      (some #{2} freqs) :one-pair
      (= [1 1 1 1 1] freqs) :high-card
      :else (throw (ex-info "Unknown hand type" {:hand hand :freqs freqs})))))

(defn compare-hands [a b]
  (compare [(.indexOf hand-types (hand-type a)) (mapv #(.indexOf cards %) a)]
           [(.indexOf hand-types (hand-type b)) (mapv #(.indexOf cards %) b)]))

(comment

  (->> example
       (sort-by first compare-hands)
       (reverse)
       (map-indexed (fn [idx [hand bid]] (* (inc idx) bid)))
       (reduce +))

  (->> input
       (sort-by first compare-hands)
       (reverse)
       (map-indexed (fn [idx [hand bid]] (* (inc idx) bid)))
       (reduce +))

  ; Answer = 248179786

  )


;; Part 2

(def cards2 [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J])

(defn hand-type2 [hand]
  (let [freqs      (frequencies hand)
        freq-vals  (vals freqs)
        js         (get freqs \J 0)
        non-js     (dissoc freqs \J)
        non-j-vals (vals non-js)]
    (cond
      (or (some #{5} freq-vals)
          (= 5 (+ (apply max non-j-vals) js))) :five-of-a-kind

      (or (some #{4} freq-vals)
          (= 4 (+ (apply max non-j-vals) js))) :four-of-a-kind

      (or (and (some #{3} freq-vals) (some #{2} freq-vals))
          (and (some #{3} non-j-vals) (some #{1} non-j-vals) (= 1 js))
          (and (= 2 (count (filter #{2} freq-vals))) (= 1 js))
          (and (some #{2} non-j-vals) (some #{1} non-j-vals) (= 2 js))) :full-house

      (or (some #{3} freq-vals)
          (= 3 (+ (apply max non-j-vals) js))) :three-of-a-kind

      (or (= 2 (count (filter #{2} freq-vals)))
          (and (some #{2} non-j-vals) (some #{1} non-j-vals) (<= 1 js))) :two-pair

      (or (some #{2} freq-vals)
          (and (some #{1} non-j-vals) (<= 1 js))) :one-pair

      (= [1 1 1 1 1] freq-vals) :high-card

      :else (throw (ex-info "Unknown hand type" {:hand hand :freqs freq-vals})))))

(defn compare-hands2 [a b]
  (compare [(.indexOf hand-types (hand-type2 a)) (mapv #(.indexOf cards2 %) a)]
           [(.indexOf hand-types (hand-type2 b)) (mapv #(.indexOf cards2 %) b)]))

(comment

  (->> example
       (sort-by first compare-hands2)
       (reverse)
       (map-indexed (fn [idx [hand bid]] (* (inc idx) bid)))
       (reduce +))

  (->> input
       (sort-by first compare-hands2)
       (reverse)
       (map-indexed (fn [idx [hand bid]] (* (inc idx) bid)))
       (reduce +))

  ; Answer = 247885995

  )

