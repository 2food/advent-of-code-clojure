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

(defn replace-pair [pair insertions]
  (if (insertions pair)
    (str (first pair) (insertions pair) (last pair))
    pair))


(defn step [template insertions]
  (let [filled-out (->> (drop-last 1 template)
                        (map-indexed (fn [ind char]
                                       (let [pair (str char (nth template (inc ind)))]
                                         (replace-pair pair insertions)))))]
    (str (first filled-out) (string/join (map (comp string/join rest) (rest filled-out))))))

(comment
  (apply step test-input))

(defn apply-steps-ntimes [template insertions n]
  (reduce (fn [acc _] (step acc insertions))
          template
          (range n)))

(defn high-minus-low [s]
  (let [quantities (vals (frequencies s))]
    (- (apply max quantities) (apply min quantities))))

(comment
  (let [[template insertions] test-input]
    (-> (apply-steps-ntimes template insertions 10)
        (high-minus-low)))

  (let [[template insertions] input]
    (-> (apply-steps-ntimes template insertions 10)
        (high-minus-low))))
; Answer = 2360


; Part 2

(defn template->pairs [template]
  (->> (drop-last template)
       (map-indexed (fn [ind char] (str char (nth template (inc ind)))))
       (frequencies)))

(defn new-pairs
  ([insertions pair]
   (new-pairs insertions pair 1))
  ([insertions pair occurances]
   {(str (first pair) (insertions pair)) occurances
    (str (insertions pair) (last pair)) occurances}))

(defn step-2 [pairs insertions]
  (->> pairs
       (map (fn [[pair occurs]] (new-pairs insertions pair occurs)))
       (apply (partial merge-with +))))



(defn apply-steps-ntimes-2 [pairs insertions n]
  (reduce (fn [acc _] (step-2 acc insertions))
          pairs
          (range n)))

(comment
  (let [[template insertions] test-input
        pairs (template->pairs template)]
    (= (apply-steps-ntimes-2 pairs insertions 10) 
       (template->pairs (apply-steps-ntimes template insertions 10)))))


(defn high-minus-low-2 [template pair-freqs]
  (let [char-freqs (->> (map (fn [[k v]] {(first k) v}) pair-freqs)
                        (apply (partial merge-with +))
                        )
        quantities (-> char-freqs 
                       (update (last template) inc)
                       (vals))]
    (- (apply max quantities) (apply min quantities))))

(comment
  (let [[template insertions] test-input
        pairs (template->pairs template)]
    (->> (apply-steps-ntimes-2 pairs insertions 40)
         (high-minus-low-2 template)))
  
  (let [[template insertions] input
        pairs (template->pairs template)]
    (->> (apply-steps-ntimes-2 pairs insertions 40)
         (high-minus-low-2 template))))
; Answer = 2967977072188