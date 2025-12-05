(ns advent-of-code-clojure.2025.day-5
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(defn parse-input [s]
  (let [lines (inputs/lines s)]
    {:ranges (mapv (fn [s] (mapv parse-long (str/split s #"-"))) (take-while (complement str/blank?) lines))
     :ids    (mapv parse-long (rest (drop-while (complement str/blank?) lines)))}))

(def example-input
  (->> ["3-5"
        "10-14"
        "16-20"
        "12-18"
        ""
        "1"
        "5"
        "8"
        "11"
        "17"
        "32"]
       (str/join "\n")
       (parse-input)))

(def input
  (->> (inputs/get-input-for-this-day)
       (parse-input)))

;; Part 1

(defn in-range? [[a b] x]
  (<= a x b))

(defn in-any-range? [ranges x]
  (some #(in-range? % x) ranges))

(comment

  (let [{:keys [ranges ids]} example-input]
    (count (filter (partial in-any-range? ranges) ids)))

  (let [{:keys [ranges ids]} input]
    (count (filter (partial in-any-range? ranges) ids)))

  ; Answer: 798

  )

;; Part 2

(defn overlaps? [[x1 y1] [x2 y2]]
  (or (<= x1 x2 y1) (<= x1 x2 y1)
      (<= x2 x1 y2) (<= x2 y1 y2)))

(defn union [[x1 y1] [x2 y2]]
  (if (overlaps? [x1 y1] [x2 y2])
    [(min x1 x2) (max y1 y2)]
    [[x1 y1] [x2 y2]]))

(defn unionize-all-ranges [ranges]
  (loop [ranges ranges]
    (if-let [overlapping (some (fn [r] (some #(and (not= r %)
                                                   (overlaps? r %)
                                                   [r %])
                                             ranges))
                               ranges)]
      (let [[r1 r2] overlapping
            new-r (union r1 r2)]
        (recur (conj (set (remove #{r1 r2} ranges)) new-r)))
      ranges)))

(comment

  (let [{:keys [ranges]} example-input]
    (->> ranges
         (unionize-all-ranges)
         (map (fn [[x y]] (inc (- y x))))
         (reduce +)))

  (let [{:keys [ranges]} input]
    (->> ranges
         (unionize-all-ranges)
         (map (fn [[x y]] (inc (- y x))))
         (reduce +)))

  ; Answer: 366181852921027

  )

