(ns advent-of-code-clojure.2021.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.set :as cset]))

(defn read-drawn [all-input]
  (->> (string/split (first all-input) #",")
       (mapv #(Long/parseLong %))))

(defn read-board [b]
  (->> b
       (map inputs/words)
       (map #(filter not-empty %))
       (mapv (partial mapv #(Long/parseLong %)))))

(defn read-boards [all-input]
  (->> (rest all-input)
       (partition-by empty?)
       (filter (partial not= '("")))
       (mapv read-board)))

(def input
  (let [all-input (-> (inputs/get-input-for-day 2021 4)
                      (inputs/lines))
        drawn-nums (read-drawn all-input)
        boards (read-boards all-input)]
    {:drawn-nums drawn-nums
     :boards boards}))

(comment
  (pp/pprint (:boards input)))

; Part 1

(def example-board
  [[67 57  2 21 19]
   [11 79 74 45 95]
   [42 90 68 47 62]
   [80 61  1  0 39]
   [43 76 40 27 66]])

(defn transpose [m]
  (apply mapv vector m))

(defn any-winning-rows? [drawn board]
  (->> board
       (some #(every? drawn %))
       (boolean)))

(defn any-winning-columns? [drawn board]
  (->> board
       (transpose)
       (some #(every? drawn %))
       (boolean)))

(defn winner? [drawn board]
  (let [drawn (set drawn)]
    (when (or (any-winning-rows? drawn board)
              (any-winning-columns? drawn board))
      board)))

(comment (assert (winner? [67 57 2 21 19] example-board))
         (assert (winner? [67 11 42 80 43] example-board)))

(defn find-winner [boards drawn]
  (some #(winner? drawn %) boards))

(defn score [board drawn]
  (let [unmarked (cset/difference (set (flatten board))
                                  (set drawn))]
    (* (last drawn) (reduce + unmarked))))

(defn find-first-winner [boards all-drawn]
  (loop [n 1]
    (let [drawn (take n all-drawn)
          winner (find-winner boards drawn)]
      (if winner
        (do (println (str "Found at draw #" n " when drawing " (last drawn)))
            (println (str "The score is: " (score winner drawn)))
            winner)
        (recur (inc n))))))

(comment
  (let [{:keys [drawn-nums boards]} input]
    (find-first-winner boards drawn-nums)))
; Answer = 58835

; Part 2

(defn find-last-winner [boards all-drawn]
  (loop [n 1
         boards boards]
    (let [drawn (take n all-drawn)
          winner (find-winner boards drawn)]
      (if (and winner (= 1 (count boards)))
        (do (println (str "Found at draw #" n " when drawing " (last drawn)))
            (println (str "The score is: " (score winner drawn)))
            winner)
        (if winner
          (recur n (remove #(= winner %) boards))
          (recur (inc n) boards))))))

(comment
  (let [{:keys [drawn-nums boards]} input]
    (find-last-winner boards drawn-nums)))
; Answer = 6256