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

(def initial-state {:head           {:pos [0 0] :followed-by :tail}
                    :tail           {:pos [0 0]}
                    :tail-positions #{[0 0]}})

(defn step-up [[x y]] [x (inc y)])
(defn step-right [[x y]] [(inc x) y])
(defn step-down [[x y]] [x (dec y)])
(defn step-left [[x y]] [(dec x) y])

(defn pos-diff [a b] (mapv - a b))
(defn inc-or-dec [n] (if (pos-int? n) inc dec))

(defn move-tail [state head-key]
  (let [{head-pos :pos tail-key :followed-by} (head-key state)
        {tail-pos :pos next-key :followed-by} (tail-key state)
        [dx dy] (pos-diff head-pos tail-pos)
        state (cond
                (and (<= (abs dx) 1) (<= (abs dy) 1)) state
                (and (> (abs dx) 1) (= (abs dy) 0)) (as-> state $
                                                          (update-in $ [tail-key :pos 0] (inc-or-dec dx)))
                (and (= (abs dx) 0) (> (abs dy) 1)) (as-> state $
                                                          (update-in $ [tail-key :pos 1] (inc-or-dec dy)))
                :else (as-> state $
                            (update-in $ [tail-key :pos 0] (inc-or-dec dx))
                            (update-in $ [tail-key :pos 1] (inc-or-dec dy))))]
    (if next-key
      (move-tail state tail-key)
      (update state :tail-positions conj (get-in state [tail-key :pos])))))

(defn do-steps [state steps]
  (reduce (fn [state step-fn]
            (-> (update-in state [:head :pos] step-fn)
                (move-tail :head)))
          state
          steps))

(defn move [state command]
  (match command
    ["U" n] (do-steps state (repeat n step-up))
    ["R" n] (do-steps state (repeat n step-right))
    ["D" n] (do-steps state (repeat n step-down))
    ["L" n] (do-steps state (repeat n step-left))))

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

(def longer-state {:head           {:pos [0 0] :followed-by :1}
                   :1              {:pos [0 0] :followed-by :2}
                   :2              {:pos [0 0] :followed-by :3}
                   :3              {:pos [0 0] :followed-by :4}
                   :4              {:pos [0 0] :followed-by :5}
                   :5              {:pos [0 0] :followed-by :6}
                   :6              {:pos [0 0] :followed-by :7}
                   :7              {:pos [0 0] :followed-by :8}
                   :8              {:pos [0 0] :followed-by :9}
                   :9              {:pos [0 0]}
                   :tail-positions #{[0 0]}})

(comment
  (->> (reduce move longer-state input)
       :tail-positions
       (count))
  ; Answer = 2259
  ) 

