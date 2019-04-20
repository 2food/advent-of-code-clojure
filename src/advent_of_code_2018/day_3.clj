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

(defn calc-coords
  [{:keys [left top width height] :as claim}]
  (let [coords (for [x (range left (+ left width))
                     y (range top (+ top height))]
                 [x y])]
    (assoc claim :coords coords)))

(defn count-covers
  [claims]
  (reduce (fn [acc {cs :coords}] (merge-with + acc (zipmap cs (repeat 1))))
          {}
          claims))

(defn santa-cloth-claims [] (count-covers (map (comp calc-coords read-claim) (input))))

(defn answer-1 [] (count (filter (fn [[k v]] (when (> v 1) k)) (santa-cloth-claims))))