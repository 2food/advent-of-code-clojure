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