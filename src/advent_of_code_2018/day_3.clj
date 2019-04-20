(ns advent-of-code-2018.day-3
  (:require [advent-of-code-2018.inputs :as inputs]
            [clojure.string :as string]))


(defn input [] (inputs/lines (inputs/get-input-for-day 3)))


; Part 1


(defn read-claim
  [s]
  (let [[id-s _ coord-s area-s] (string/split s #" ")
        [left-s top-s] (string/split (apply str (drop-last coord-s)) #",")
        [width-s height-s] (string/split area-s #"x")]
    {:id (read-string (string/join (drop 1 id-s)))
     :left (read-string left-s)
     :top  (read-string top-s)
     :width (read-string width-s)
     :height (read-string height-s)}))
