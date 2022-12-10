(ns advent-of-code-clojure.2022.day-10
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.core.match :refer [match]]
            [clojure.string :as string]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (map #(if (= 2 (count %))
               (update % 1 parse-long)
               %))))

(def test-input
  (->> "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n"
       (inputs/lines-and-words)
       (map #(if (= 2 (count %))
               (update % 1 parse-long)
               %))))

(def small-input
  (->> "noop\naddx 3\naddx -5"
       (inputs/lines-and-words)
       (map #(if (= 2 (count %))
               (update % 1 parse-long)
               %))))

;; Part 1 

(defn cycle-fns [instruction]
  (match instruction
    ["noop"] [identity]
    ["addx" n] [identity #(+ % n)]))

(defn instructions->cycle-fns [instructions]
  (vec (mapcat cycle-fns instructions)))

(def initial-x 1)

(defn cycle-vec [instructions]
  (->> (instructions->cycle-fns instructions)
       (reduce (fn [xs cycle-fn] (conj xs (cycle-fn (last xs)))) [initial-x initial-x])
       (vec)))

(defn calc-vals [instructions]
  (let [cycle-vec (cycle-vec instructions)]
    (->> (range 20 221 40)
         (map #(* (nth cycle-vec %) %))
         (reduce +))))

(comment
  (cycle-vec small-input)

  (calc-vals test-input)
  (calc-vals input)
  ; Answer = 12560
  )

;; Part 2

(defn draw-screen [instructions]
  (->> (drop-last (rest (cycle-vec instructions)))
       (map (fn [write-pos sprite-pos]
              (if (<= (abs (- sprite-pos write-pos)) 1)
                \#
                \.))
            (cycle (range 0 40)))
       (partition-all 40)
       (map string/join)))

(comment
  (draw-screen test-input)
  (draw-screen input)
  ; Answer = PLPAFBCL
  ) 

