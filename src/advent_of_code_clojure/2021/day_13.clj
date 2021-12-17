(ns advent-of-code-clojure.2021.day-13
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (let [[dots folds] (string/split s #"\n\n")
        dots (->> dots
                  (inputs/lines)
                  (mapv #(mapv read-string (string/split % #","))))
        folds (->> folds
                   (inputs/lines)
                   (mapv #(if (re-find #"x" %)
                            ["x" (read-string (last (string/split % #"=")))]
                            ["y" (read-string (last (string/split % #"=")))])))]
    [(set dots) folds]))

(def input
  (parse-input
   (inputs/get-input-for-day 2021 13)))

(def test-input
  (parse-input
   "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))


; Part 1

(defn fold [dots line flipfn]
  (set (map (partial flipfn line) dots)))

(defn flipy [yline [x y]]
  (if (> y yline)
    [x (- yline (- y yline))]
    [x y]))

(defn foldy [dots yline]
  (fold dots yline flipy))

(defn flipx [xline [x y]]
  (if (> x xline)
    [(- xline (- x xline)) y]
    [x y]))

(defn foldx [dots xline]
  (fold dots xline flipx))

(comment (count (foldy (first test-input) 7))
         (count (foldx (first input) 655)))
; Answer = 818

(defn fold-all [[dots folds]]
  (reduce (fn [dots [dir line]]
            (let [foldfn ({"x" foldx "y" foldy} dir)]
              (foldfn dots line)))
          dots
          folds))

(defn draw-dots [dots]
  (let [sizex (inc (apply max (map first dots)))
        sizey (inc (apply max (map second dots)))
        emptymap  (vec (for [_ (range sizey)]
                         (vec (for [_ (range sizex)] "."))))]
    (->> (reduce (fn [m [x y]]
                   (assoc-in m [y x] "#"))
                 emptymap
                 dots)
         (map (partial string/join " "))
         (string/join "\n"))))

(comment
  (println (draw-dots (fold-all test-input)))
  (println (draw-dots (fold-all input))))
; Answer = LRGPRECB