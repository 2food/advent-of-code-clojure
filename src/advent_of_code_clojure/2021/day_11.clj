(ns advent-of-code-clojure.2021.day-11
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [advent-of-code-clojure.utils :as utils]))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (mapv #(mapv read-string
                    (string/split % #"")))))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))


; Part 1


(defn some-to-flash [m]
  (let [flatm (flatten m)]
    (when-let [n (some #(and (> % 9) %) flatm)]
      (let [ind (.indexOf flatm n)
            sizex (count (first m))]
        [(mod ind sizex) (quot ind sizex)]))))

(comment (->> test-input
              (mapv #(mapv inc %))
              (mapv #(mapv inc %))
              (some-to-flash)))

(defn flash! [m [x y]]
  (let [neighbours (utils/get-neighbours-8 m x y)
        inc-if-not-flashed #(if (not= 0 %) (inc %) %)]
    (-> (reduce (fn [m [x y]]
                  (update-in m [y x] inc-if-not-flashed))
                m
                neighbours)
        (assoc-in [y x] 0))))

(defn execute-flashes [m]
  (loop [m m
         nflashes 0]
    (if-let [xy (some-to-flash m)]
      (recur (flash! m xy) (inc nflashes))
      [m nflashes])))

(defn step [m]
  (->> m
       (mapv #(mapv inc %))
       (execute-flashes)))

(defn simulate [m steps]
  (loop [m m 
         nflashes 0
         c 0]
    (if (= c steps)
      [m nflashes]
      (let [[newm nflashed] (step m)] 
        (recur newm (+ nflashes nflashed) (inc c))))))

(comment
  (pprint/pprint (simulate test-input 100))
  (pprint/pprint (simulate input 100)))
; Answer = 1588


; Part 2

(defn find-first-sync [m]
  (loop [m m
         c 1]
    (let [[newm nflashed] (step m)]
      (if (= nflashed (count (flatten m)))
        c
        (recur newm (inc c))))))

(comment 
  (find-first-sync test-input)
  (find-first-sync input))
; Ansewr = 517