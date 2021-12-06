(ns advent-of-code-clojure.2021.day-6
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> (string/split s #"[,\n]")
       (mapv read-string)))

(defn input []
  (parse-input (inputs/get-input-for-day 2021 6)))

(defn simulate-day [lanternfish]
  (reduce (fn [acc fish]
            (let [[lf new] acc]
              (if (= 0 fish)
                [(conj lf 6) (conj new 8)]
                [(conj lf (dec fish)) new])))
          [[] []]
          lanternfish))

(defn simulate [lanternfish end-day]
  (loop [day 1
         fish lanternfish]
    (let [[old-fish new-fish] (simulate-day fish)
          fish (into old-fish new-fish)]
      (if (= day end-day)
        fish
        (recur (inc day) fish)))))

(count (simulate (input) 80))
; Answer = 350917
