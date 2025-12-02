(ns advent-of-code-clojure.2025.day-1
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.math :as math]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (map #(str/replace % #"L" "-"))
       (map #(str/replace % #"R" ""))
       (map parse-long)))

(def input
  (parse-input (inputs/get-input-for-this-day)))

;; Part 1


(comment

  (def positions (reduce (fn [positions change]
                           (conj positions (mod (+ (last positions) change)
                                                100)))
                         [50]
                         input))

  (count (filter #(= 0 %) positions))

  )

; Answer: 995


;; Part 2

(defn calculate [input]
  (reduce (fn [state change]
            (let [{:keys [position]} (last state)
                  new-pos           (mod (+ position change)
                                         100)
                  whole-rotations   (math/floor-div (abs change)
                                                    100)
                  remainding-change (if (neg? change)
                                      (+ change (* whole-rotations 100))
                                      (- change (* whole-rotations 100)))]
              (conj state {:old-pos  position
                           :position new-pos
                           :change   change
                           :zeros    (+ (abs whole-rotations)
                                        (if (or (<= 100 (+ position remainding-change))
                                                (and (not= 0 position)
                                                     (<= (+ position remainding-change) 0)))
                                          1
                                          0))})))
          [{:position 50 :zeros 0}]
          input))

(comment

  (->> (calculate input)
       (map :zeros)
       (reduce +))

  )

;; Answer: 5847