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

(defn input []
  (->> (inputs/get-input-for-day 2021 5)
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

(defn populate-area [line]
  (let [[[x1 y1] [x2 y2]] line
        xs (if (> x2 x1) 
             (range x1 (inc x2))
             (range x2 (inc x1)))
        ys (if (> y2 y1) 
             (range y1 (inc y2))
             (range y2 (inc y1)))]
    (for [x xs y ys]
      [x y])))

(defn count-positions [lines]
  (frequencies (reduce into (mapv populate-area lines))))

(defn duplicate-positions [lines]
  (reduce-kv (fn [acc k v] (if (> v 1)
                             (conj acc k)
                             acc))
             []
             (count-positions lines)))

(defn answer1 [inp]
  (count (duplicate-positions (h-or-v? inp))))

(def example-input
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

(comment  (answer1 example-input)
          (answer1 (input)))
; Answer = 6687