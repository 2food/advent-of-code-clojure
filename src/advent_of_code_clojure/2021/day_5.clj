(ns advent-of-code-clojure.2021.day-5
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn read-point [s]
  (->> (string/split s #",")
       (mapv read-string)))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (map #(string/split % #" -> "))
       (mapv (partial mapv read-point))))

(def input
  (->> (inputs/get-input-for-day 2021 5)
       (parse-input)))

(def test-input
  (-> "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"
      (parse-input)))

; Part 1

(defn horizontal? [line]
  (let [[[x1 _] [x2 _]] line]
    (= x1 x2)))

(defn vertical? [line]
  (let [[[_ y1] [_ y2]] line]
    (= y1 y2)))

(defn h-or-v? [line]
  (filter (some-fn horizontal? vertical?) line))

(defn bidir-range-line [a b]
  (if (> b a)
    (range a (inc b))
    (reverse (range b (inc a)))))

(defn populate-area [line]
  (let [[[x1 y1] [x2 y2]] line
        xs (bidir-range-line x1 x2)
        ys (bidir-range-line y1 y2)]
    (for [x xs y ys]
      [x y])))

(defn count-positions [positions]
  (frequencies (reduce into positions)))

(defn duplicate-positions [positions]
  (reduce-kv (fn [acc k v] (if (> v 1)
                             (conj acc k)
                             acc))
             []
             (count-positions positions)))

(defn answer1 [inp]
  (->> (h-or-v? inp)
       (mapv populate-area)
       (duplicate-positions)
       (count)))

(comment  (answer1 test-input)
          (answer1 input))
; Answer = 6687


; Part 2

(defn diagonal? [line]
  (let [[[x1 y1] [x2 y2]] line]
    (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))))

(defn h-or-v-or-d? [line]
  (filter (some-fn horizontal? vertical? diagonal?) line))

(defn populate-line [line]
  (if (diagonal? line)
    (let [[[x1 y1] [x2 y2]] line
          xs (bidir-range-line x1 x2)
          ys (bidir-range-line y1 y2)]
      (mapv vector xs ys))
    (populate-area line)))

(defn answer2 [inp]
  (->> (h-or-v-or-d? inp)
       (mapv populate-line)
       (duplicate-positions)
       (count)))

(comment (answer2 input))
; Answer = 19851