(ns advent-of-code-clojure.2023.day-3
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv vec)))

(def example
  (->> "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..\n"
       (inputs/lines)
       (mapv vec)))

;; Part 1

(def num-chars (set "123456789"))
(def not-symbol (set/union #{\.} num-chars))

(defn symbol-indices [input]
  (->> (utils/all-coords input)
       (filter (fn [[x y]]
                 (not (not-symbol (utils/get-coord input x y)))))
       (set)))


(defn num-index [input]
  (->> input
       (map-indexed (fn [y line]
                      (->> (map vector line (range))
                           (partition-by (comp some? num-chars first))
                           (map (partial filter #(num-chars (first %))))
                           (remove empty?)
                           (mapv (fn [x] [(parse-long (str/join (map first x)))
                                          (set (map (comp #(vector % y) second) x))])))))
       (apply concat)))


(defn adjacent-to-symbol? [sym-inds [num coords]]
  (->> coords
       (mapcat (fn [[x y]] (utils/get-neighbours-8 input x y)))
       (set)
       (some sym-inds)))


(comment

  (let [sym-inds (symbol-indices example)]
    (->> (num-index example)
         (filter (partial adjacent-to-symbol? sym-inds))
         (map first)
         (reduce +)))

  (let [sym-inds (symbol-indices input)]
    (->> (num-index input)
         (filter (partial adjacent-to-symbol? sym-inds))
         (map first)
         (reduce +)))

  ; Too low = 453032

  )

;; Part 2


(comment

  )

