(ns advent-of-code-clojure.2022.day-9
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.core.match :refer [match]]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (map #(update % 1 parse-long))))

(def test-input
  (->> "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
       (inputs/lines-and-words)
       (map #(update % 1 parse-long))))

;; Part 1

(def initial-state {:head           {:pos [0 0] :next :tail}
                    :tail           {:pos [0 0]}
                    :tail-positions #{[0 0]}})

(defn step-up [[x y]] [x (inc y)])
(defn step-right [[x y]] [(inc x) y])
(defn step-down [[x y]] [x (dec y)])
(defn step-left [[x y]] [(dec x) y])

(defn pos-diff [a b] (mapv - a b))
(defn inc-or-dec [n] (if (pos-int? n) inc dec))

(defn move-tail [state head-key]
  (let [{head-pos :pos tail-key :next} (get state head-key)
        {tail-pos :pos next-key :next} (get state tail-key)
        [dx dy] (pos-diff head-pos tail-pos)
        state (cond
                (and (<= (abs dx) 1) (<= (abs dy) 1)) state
                (and (> (abs dx) 1) (= (abs dy) 0)) (update-in state [tail-key :pos 0] (inc-or-dec dx))
                (and (= (abs dx) 0) (> (abs dy) 1)) (update-in state [tail-key :pos 1] (inc-or-dec dy))
                :else (-> state
                          (update-in [tail-key :pos 0] (inc-or-dec dx))
                          (update-in [tail-key :pos 1] (inc-or-dec dy))))]
    (if next-key
      (move-tail state tail-key)
      (let [new-tail-pos (get-in state [tail-key :pos])]
        (update state :tail-positions conj new-tail-pos)))))

(defn do-steps [state steps]
  (reduce (fn [state step-fn]
            (-> (update-in state [:head :pos] step-fn)
                (move-tail :head)))
          state
          steps))

(defn move [state command]
  (let [steps (match command
                ["U" n] (repeat n step-up)
                ["R" n] (repeat n step-right)
                ["D" n] (repeat n step-down)
                ["L" n] (repeat n step-left))]
    (do-steps state steps)))

(comment
  (->> (reduce move initial-state test-input)
       :tail-positions
       (count))

  (->> (reduce move initial-state input)
       :tail-positions
       (count))
  ; Answer = 5710
  )

;; Part 2

(def longer-state {:head           {:pos [0 0] :next 1}
                   1               {:pos [0 0] :next 2}
                   2               {:pos [0 0] :next 3}
                   3               {:pos [0 0] :next 4}
                   4               {:pos [0 0] :next 5}
                   5               {:pos [0 0] :next 6}
                   6               {:pos [0 0] :next 7}
                   7               {:pos [0 0] :next 8}
                   8               {:pos [0 0] :next 9}
                   9               {:pos [0 0]}
                   :tail-positions #{[0 0]}})

(comment
  (->> (reduce move longer-state input)
       :tail-positions
       (count))
  ; Answer = 2259
  ) 

