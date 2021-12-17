(ns advent-of-code-clojure.2021.day-14
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))


(defn parse-input [s]
  (let [[template insertions] (string/split s #"\n\n")
        insertions (->> insertions
                        (inputs/lines-and-words)
                        (flatten)
                        (filter #(not= "->" %))
                        (apply hash-map))]
    [template insertions]))

(def input
  (parse-input
   (inputs/get-input-for-day 2021 14)))

(def test-input
  (parse-input
   "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))


; Part 1

(defn string->pairs [s]
  (->> (drop-last s)
       (map-indexed (fn [ind char] (str char (nth s (inc ind)))))
       (frequencies)))

(defn new-pairs
  ([insertions pair]
   (new-pairs insertions pair 1))
  ([insertions pair occurances]
   {(str (first pair) (insertions pair)) occurances
    (str (insertions pair) (last pair)) occurances}))

(defn step [pairs insertions]
  (->> pairs
       (map (fn [[pair occurs]] (new-pairs insertions pair occurs)))
       (apply (partial merge-with +))))

(defn apply-steps-ntimes [pairs insertions n]
  (reduce (fn [acc _] (step acc insertions))
          pairs
          (range n)))

(defn high-minus-low [template pair-freqs]
  (let [char-freqs (->> (map (fn [[k v]] {(first k) v}) pair-freqs)
                        (apply (partial merge-with +)))
        quantities (-> char-freqs
                       (update (last template) inc)
                       (vals))]
    (- (apply max quantities) (apply min quantities))))

(defn simulate [inp nsteps]
  (let [[template insertions] inp
        pairs (string->pairs template)]
    (->> (apply-steps-ntimes pairs insertions nsteps)
         (high-minus-low template))))

(comment
  (simulate test-input 10)
  (simulate input 10))
; Answer = 2360


; Part 2

(comment
  (simulate test-input 40)
  (simulate input 40))
; Answer = 2967977072188