(ns advent-of-code-clojure.2021.day-6
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> (string/split s #"[,\n]")
       (mapv read-string)))

(def input
  (parse-input (inputs/get-input-for-this-day)))


; Part 1

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

(comment
  (count (simulate input 80)))
; Answer = 350917


; Part 2

(defn to-index [inp]
  (into (->> (frequencies inp)
             (into [[0 0]])
             (sort)
             (mapv second))
        [0 0 0]))

(defn simulate-day-2 [lanternfish]
  (let [new-fish (first lanternfish)]
    (-> (rest lanternfish)
        (vec)
        (update 6 (partial + new-fish))
        (conj new-fish))))

(defn simulate-2 [lanternfish end-day]
  (loop [day 1
         fish lanternfish]
    (let [fish (simulate-day-2 fish)]
      (if (= day end-day)
        fish
        (recur (inc day) fish)))))

(comment
  (reduce + (simulate-2 (to-index input) 256)))
; Answer = 1592918715629