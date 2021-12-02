(ns advent-of-code-clojure.2021.day-2
  (:require [advent-of-code-clojure.inputs :as inputs]))

(defn input []
  (->>
   (inputs/get-input-for-day 2021 2)
   (inputs/lines-and-words)
   (map (fn [[a b]] [a (Long/parseLong b)]))))

; Part 1

(defn adjust-pos [position instruction]
  (let [[x y] position
        [direction value] instruction]
    (case direction
      "forward" [(+ x value) y]
      "down" [x (+ y value)]
      "up" [x (- y value)])))

(defn compute-position [instructions]
  (reduce adjust-pos [0 0] instructions))

(comment 
  (let [pos (compute-position (input))]
    (* (first pos) (second pos))))
; Answer = 1746616


; Part 2

(defn adjust-pos-2 [position instruction]
  (let [[x y aim] position
        [direction value] instruction]
    (case direction
      "forward" [(+ x value) (+ y (* aim value)) aim]
      "down" [x y (+ aim value)]
      "up" [x y (- aim value)])))

(defn compute-position-2 [instructions]
  (reduce adjust-pos-2 [0 0 0] instructions))

(comment
  (let [pos (compute-position-2 (input))]
    (* (first pos) (second pos))))
; Answer = 1741971043