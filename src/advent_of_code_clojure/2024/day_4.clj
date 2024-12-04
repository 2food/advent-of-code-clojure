(ns advent-of-code-clojure.2024.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.string :as str]))

(def input
  (->> (inputs/get-input-for-day 2024 4)
       (inputs/lines)))


;; Part 1

(def horizontals input)
(def verticals (mapv str/join (utils/transpose input)))
(def diagonals (->> input
                    (map-indexed (fn [idx v] (concat (repeat idx nil)
                                                     v
                                                     (repeat (- (dec (count input)) idx) nil))))
                    (utils/transpose)
                    (mapv str/join)))
(def other-diagonals (->> input
                          (map-indexed (fn [idx v] (concat (repeat (- (dec (count input)) idx) nil)
                                                           v
                                                           (repeat idx nil))))
                          (utils/transpose)
                          (mapv str/join)))

(defn count-xmas [ss]
  (reduce + (map #(count (re-seq #"XMAS" %)) ss)))

(defn backwards [ss]
  (mapv (comp str/join reverse) ss))

(comment
  (+ (count-xmas horizontals)
     (count-xmas (backwards horizontals))
     (count-xmas verticals)
     (count-xmas (backwards verticals))
     (count-xmas diagonals)
     (count-xmas (backwards diagonals))
     (count-xmas other-diagonals)
     (count-xmas (backwards other-diagonals)))

  ; Answer = 2401

  )

;; Part 2


(comment
  (->> (utils/all-coords input)
       (keep (fn [[x y]]
               (when (and (= \A (utils/get-coord input x y))
                          (or (and (= \M (utils/get-coord input (dec x) (dec y)))
                                   (= \S (utils/get-coord input (inc x) (inc y))))
                              (and (= \S (utils/get-coord input (dec x) (dec y)))
                                   (= \M (utils/get-coord input (inc x) (inc y)))))
                          (or (and (= \M (utils/get-coord input (inc x) (dec y)))
                                   (= \S (utils/get-coord input (dec x) (inc y))))
                              (and (= \S (utils/get-coord input (inc x) (dec y)))
                                   (= \M (utils/get-coord input (dec x) (inc y))))))

                 [x y])))
       (count))

  ; Answer = 1822
  )
