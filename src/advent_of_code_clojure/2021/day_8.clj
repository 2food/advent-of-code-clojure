(ns advent-of-code-clojure.2021.day-8
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-input [s]
  (let [lines (inputs/lines s)]
    (mapv (fn [line]
            (let [[unique output] (string/split line #"\|")]
              [(mapv (comp string/join sort) (inputs/words unique))
               (mapv (comp string/join sort) (rest (inputs/words output)))]))
          lines)))

(def input
  (-> (inputs/get-input-for-this-day)
      (parse-input)))

(def test-input
  (parse-input
   "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))


; Part 1

(defn determine-char [unique]
  (reduce (fn [acc u]
            (merge acc
                   (condp = (count u)
                     2 {u 1}
                     3 {u 7}
                     4 {u 4}
                     7 {u 8}
                     {u nil})))
          {}
          unique))

(defn determine-chars [inp]
  (mapv (fn [[u o]] [(determine-char u) o])
        inp))

(def empty-count-map
  {0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9 0})

(defn count-chars [inp]
  (let [pre-determined (determine-chars inp)]
    (mapv
     (fn [[det o]]
       (reduce (fn [acc digit]
                 (if (det digit)
                   (update acc (det digit) inc)
                   acc))
               empty-count-map
               o))
     pre-determined)))

(defn sum-of-counts [inp]
  (apply (partial merge-with +) (count-chars inp)))

(comment
  test-input
  (determine-chars test-input)
  (map #(select-keys % [1 4 7 8]) (count-chars test-input))
  (reduce + (vals (select-keys (sum-of-counts test-input) [1 4 7 8]))))

(comment
  (map #(select-keys % [1 4 7 8]) (count-chars input))
  (reduce + (vals (select-keys (sum-of-counts input) [1 4 7 8]))))
; Answer = 456


; Part 2

(defn has-in-common-with [det n u target]
  (= target (count (set/intersection (set (det n)) (set u)))))

(defn is-0? [det u]
  (and (has-in-common-with det 8 u 6)
       (has-in-common-with det 1 u 2)
       (has-in-common-with det 4 u 3)
       (has-in-common-with det 7 u 3)))

(defn is-2? [det u]
   (and (has-in-common-with det 7 u 2)
        (has-in-common-with det 4 u 2)))

(defn is-3? [det u]
  (and (has-in-common-with det 7 u 3)
       (has-in-common-with det 4 u 3)))

(defn is-5? [det u]
  (and (has-in-common-with det 1 u 1)
       (has-in-common-with det 4 u 3)
       (has-in-common-with det 7 u 2)
       (has-in-common-with det 8 u 5)))

(defn is-6? [det u]
  (and (has-in-common-with det 1 u 1)
       (has-in-common-with det 4 u 3)
       (has-in-common-with det 7 u 2)
       (has-in-common-with det 8 u 6)))

(defn is-9? [det u]
  (and (has-in-common-with det 1 u 2)
       (has-in-common-with det 4 u 4)
       (has-in-common-with det 7 u 3)
       (has-in-common-with det 8 u 6)))

(defn analyze-char [det unique]
  (let [invert-det (set/map-invert det)]
    (reduce (fn [acc u]
              (merge-with #(first (filter some? [%1 %2]))
                          acc
                          (cond
                            (is-0? invert-det u) {u 0}
                            (is-2? invert-det u) {u 2}
                            (is-3? invert-det u) {u 3}
                            (is-5? invert-det u) {u 5}
                            (is-6? invert-det u) {u 6}
                            (is-9? invert-det u) {u 9}
                            :else {u nil})))
            det
            unique)))

(defn analyze-chars [inp]
  (let [pre-determined (determine-chars inp)]
    (mapv (fn [[u o] [det _]] [(analyze-char det u) o])
          inp pre-determined)))

(defn remove-leading-zeros [ns]
  (if (= 0 (first ns))
    (remove-leading-zeros (rest ns))
    ns))

(defn find-output [inp]
  (let [analyzed (analyze-chars inp)]
    (mapv
     (fn [[det o]]
       (->> (map det o)
            (remove-leading-zeros)
            (string/join)
            (read-string)))
     analyzed)))

(comment
  (analyze-chars test-input)
  (find-output test-input)
  (reduce + (find-output test-input)))

(comment 
  (find-output input)
  (reduce + (find-output input)))
; Answer = 1091609