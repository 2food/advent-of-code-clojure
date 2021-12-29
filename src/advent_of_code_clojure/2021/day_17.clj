(ns advent-of-code-clojure.2021.day-17
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> s
       (string/trim)
       (re-matches #".*x=(.+)\.\.(.+), y=(.+)\.\.(.+)")
       (rest)
       (mapv read-string)))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "target area: x=20..30, y=-10..-5"))


; Part 1

(defn slow-down [[dx dy]]
  (let [new-dx (cond
                 (> dx 0) (dec dx)
                 (< dx 0) (inc dx)
                 :else dx)
        new-dy (dec dy)]
    [new-dx new-dy]))

(defn step [point speed]
  (let [new-point (mapv + point speed)
        new-speed (slow-down speed)]
    [new-point new-speed]))

(defn simulate-shot [start-speed stop-fn]
  (loop [point [0 0]
         speed start-speed]
    (if (stop-fn point)
      [point speed]
      (let [[next-point next-speed] (step point speed)]
        (recur next-point next-speed)))))

(defn within-target? [[xmin xmax ymin ymax] [x y]]
  (let [xrange (set (range xmin (inc xmax)))
        yrange (set (range ymin (inc ymax)))]
    (and (xrange x) (yrange y))))

(defn missed-target? [[_ xmax ymin _] [x y]]
  (or (> x xmax) (< y ymin)))

(defn valid-shot? [speed target]
  (let [hit? (partial within-target? target)
        miss? (partial missed-target? target)
        [end-point _] (simulate-shot speed (some-fn hit? miss?))]
    (boolean (hit? end-point))))

(comment
  (valid-shot? [7 2] test-input)
  (valid-shot? [6 3] test-input)
  (valid-shot? [9 0] test-input)
  (not (valid-shot? [17 -4] test-input)))

(defn all-valid-dx [dy target]
  (let [dxrange (range (inc (second target)))]
    (filter #(valid-shot? [% dy] target) dxrange)))

(defn has-a-valid-dx? [dy target]
  (not-empty (all-valid-dx dy target)))

(defn all-valid-dy [[_ xmax ymin _ :as target]]
  (let [dyrange (range ymin xmax)]
    (filter #(has-a-valid-dx? % target) dyrange)))

(defn find-highest-valid-shot [target]
  (let [valid-dy (last (all-valid-dy target))
        valid-dx (first (all-valid-dx valid-dy target))]
       [valid-dx valid-dy]))

(defn calc-curve-top [[_ dy]]
  (reduce + (range (inc dy))))

(comment
  (calc-curve-top (find-highest-valid-shot test-input))
  (calc-curve-top (find-highest-valid-shot input)))
; Answer = 4005


; Part 2

(defn all-valid-speeds [target]
  (for [dy (all-valid-dy target)
        dx (all-valid-dx dy target)]
    [dx dy]))

(comment
  (count (all-valid-speeds test-input))
  (count (all-valid-speeds input)))
; Answer = 2953