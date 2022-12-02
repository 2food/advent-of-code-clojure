(ns advent-of-code-clojure.2022.day-2
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (map set)))


;; Part 1

(def rock 1)
(def paper 2)
(def scissor 3)
(def loose 0)
(def draw 3)
(def win 6)
(def rules
  {#{"A" "X"} (+ rock draw)
   #{"A" "Y"} (+ paper win)
   #{"A" "Z"} (+ scissor loose)
   #{"B" "X"} (+ rock loose)
   #{"B" "Y"} (+ paper draw)
   #{"B" "Z"} (+ scissor win)
   #{"C" "X"} (+ rock win)
   #{"C" "Y"} (+ paper loose)
   #{"C" "Z"} (+ scissor draw)})

(comment
  (->> (map rules input)
       (reduce +))
  ; Answer = 12794
  )

;; Part 2

(def new-rules
  {#{"A" "X"} (+ scissor loose)
   #{"A" "Y"} (+ rock draw)
   #{"A" "Z"} (+ paper win)
   #{"B" "X"} (+ rock loose)
   #{"B" "Y"} (+ paper draw)
   #{"B" "Z"} (+ scissor win)
   #{"C" "X"} (+ paper loose)
   #{"C" "Y"} (+ scissor draw)
   #{"C" "Z"} (+ rock win)})

(comment
  (->> (map new-rules input)
       (reduce +))
  ; Answer = 14979
  )
