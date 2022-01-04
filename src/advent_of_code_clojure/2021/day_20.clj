(ns advent-of-code-clojure.2021.day-20
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [advent-of-code-clojure.utils :as utils]))


(defn read-bit [char-or-string]
  (if (or (= "#" char-or-string) (= \# char-or-string))
    true
    false))

(defn show-bit [bit]
  (if bit
    "#"
    "."))

(defn show-image
  ([image]
   (show-image nil image)
   image)
  ([outfile image]
   (let [out-string (->> image
                         (mapv #(string/join (mapv show-bit %)))
                         (string/join "\n"))]
     (if outfile
       (spit outfile out-string)
       (println out-string)))
   image))

(defn parse-input [s]
  (let [[algorithm image] (string/split (string/trim s) #"\n\n")
        algorithm (mapv read-bit algorithm)
        image (->> image
                   (string/split-lines)
                   (mapv #(mapv read-bit %)))]
    {:algorithm algorithm
     :image image}))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"))

; Part 1

(defn get-region [m x y]
  (let [xrange (range (max 0 (dec x))
                      (min (count (first m)) (+ 2 x)))
        yrange (range (max 0 (dec y))
                      (min (count m) (+ 2 y)))
        all-neighbours (for [y yrange
                             x xrange]
                         (nth (nth m y) x))]
    (assert (= 9 (count all-neighbours)))
    all-neighbours))

(comment
  (let [{:keys [algorithm image]} test-input]
    (get-region image 1 1)))

(defn pad-image [m]
  (let [nrows (+ 2 (count (first m)))
        empty-row (vec (repeat nrows false))
        padded (mapv #(into [false] (conj % false)) m)]
    (into [empty-row]
          (conj padded
                empty-row))))

(comment
  (pad-image [[true false]
              [false true]]))

(defn get-number [region]
  (-> (mapv #(if % "1" "0") region)
      (string/join)
      #_(utils/printr)
      (Long/parseLong 2)
      #_(utils/printr)))

(defn determine-bit [algorithm image x y]
  (->> (get-region image x y)
       (get-number)
       (nth algorithm)))

(let [{:keys [algorithm image]} test-input]
  (determine-bit algorithm image 3 1))


(defn step [algo image]
  (let [write-image (pad-image image)
        read-image (pad-image write-image)]
    (vec (map-indexed (fn [y row]
                        (vec (map-indexed (fn [x i]
                                            (determine-bit algo read-image (inc x) (inc y)))
                                          row)))
                      write-image))))

(defn drop-first-and-last [v]
  (->> v
       (drop 1)
       (drop-last 1)))

(defn unpad-image [image]
  (->> image
       (drop-first-and-last)
       (mapv drop-first-and-last)))

(defn count-lighted [image]
  (->> image
       (flatten)
       (filter true?)
       (count)))

(defn simulate [inp nsteps]
  (let [{:keys [algorithm image]} inp
        pad-width (* 2 nsteps)
        image (utils/reapply pad-image pad-width image)
        simulated-image (utils/reapply (partial step algorithm) nsteps image)]
    (utils/reapply unpad-image pad-width simulated-image)))

(comment
  (count-lighted (simulate test-input 2)))

(comment
  (count-lighted (simulate input 2)))
; Answer = 5203


; Part 2

(comment
  (count-lighted (simulate test-input 50)))

(comment
  (count-lighted (simulate input 50)))
; Answer = 18806