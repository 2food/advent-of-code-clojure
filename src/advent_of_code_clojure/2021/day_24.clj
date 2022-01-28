(ns advent-of-code-clojure.2021.day-24
  (:require  [clojure.string :as string]))

; Analysis of input yields these values:
(def down-targets [nil nil nil nil -8 nil -11 nil -6 -9 nil -5 -4 -9])
(def up-values [7 8 16 8 3 12 1 8 8 14 4 14 15 6])


; Part 1

(def in-range? (set (range 1 10)))

(defn rec-find [ind z]
  (if (= ind 14)
    (when (= z 0) [])
    (if-let [target (nth down-targets ind)]
      (let [pot-input (+ (mod z 26) target)]
        (when (in-range? pot-input)
          (when-let [valid-rest (rec-find (inc ind) (quot z 26))]
            (cons pot-input valid-rest))))
      (loop [pot-input 9]
        (when (not= 0 pot-input)
          (if-let [valid-rest (rec-find (inc ind) (+ (* z 26) pot-input (nth up-values ind)))]
            (cons pot-input valid-rest)
            (recur (dec pot-input))))))))

(defn monad-str [m]
  (read-string (string/join (map str m))))

(comment 
  (monad-str (rec-find 0 0)))
; Answer = 95299897999897


; Part 2

(defn opposite-rec-find [ind z]
  (if (= ind 14)
    (when (= z 0) [])
    (if-let [target (nth down-targets ind)]
      (let [pot-input (+ (mod z 26) target)]
        (when (in-range? pot-input)
          (when-let [valid-rest (opposite-rec-find (inc ind) (quot z 26))]
            (cons pot-input valid-rest))))
      (loop [pot-input 1]
        (when (not= 10 pot-input)
          (if-let [valid-rest (opposite-rec-find (inc ind) (+ (* z 26) pot-input (nth up-values ind)))]
            (cons pot-input valid-rest)
            (recur (inc pot-input))))))))

(comment
  (monad-str (opposite-rec-find 0 0)))
; Answer = 31111121382151
