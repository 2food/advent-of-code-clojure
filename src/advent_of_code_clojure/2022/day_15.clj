(ns advent-of-code-clojure.2022.day-15
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-line [line]
  (let [[sx sy bx by] (map parse-long (re-seq #"\d+" line))]
    {[sx sy] [bx by]}))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (map parse-line)
       (apply merge)))

(def input (parse-input (inputs/get-input-for-this-day)))
(def test-input (parse-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"))

;; Part 1

(defn distance [[ax ay] [bx by]]
  (+ (abs (- ax bx))
     (abs (- ay by))))

(defn distances [sensor-reading]
  (->> sensor-reading
       (map (fn [[k v]] [k (distance k v)]))
       (into {})))

(defn beacon-inpossible? [beacon-dists pos]
  (some (fn [sensor] (<= (distance sensor pos) (beacon-dists sensor)))
        (keys beacon-dists)))

(defn pos-range [beacon-dists coord-fn]
  (let [smallest-largest (->> beacon-dists
                              (map (fn [[k v]] ((juxt - +) (coord-fn k) v))))]
    [(apply min (map first smallest-largest))
     (apply max (map second smallest-largest))]))

(defn xrange [beacon-dists] (pos-range beacon-dists first))
(defn yrange [beacon-dists] (pos-range beacon-dists second))

(defn inspect-line [input y]
  (let [beacons (set (vals input))
        beacon-dists (distances input)
        [min-x max-x] (xrange beacon-dists)]
    (->> (for [x (range min-x (inc max-x))]
           [x y])
         (filter #(and (not (beacons %))
                       (beacon-inpossible? beacon-dists %)))
         (count))))

(defn draw [input]
  (let [beacons (set (vals input))
        sensors (set (keys input))
        beacon-dists (distances input)
        [minx maxx] (xrange beacon-dists)
        [_ maxy] (yrange beacon-dists)]
    (->> (for [y (range 0 (inc maxy))
               x (range minx (inc maxx))]
           [x y])
         (map #(cond
                 (and (sensors %) (beacons %)) \W
                 (sensors %) \S
                 (beacons %) \B
                 (beacon-inpossible? beacon-dists %) \#
                 :else \.))
         (partition (abs (- minx (inc maxx))))
         (mapv string/join))))

(comment
  (distances test-input)
  (beacon-inpossible? (distances test-input) [24 9])
  (possible-x (distances test-input))
  (nth (draw test-input) 10)
  (inspect-line test-input 10)

  (inspect-line input 2000000)
  ; Too low = 4998562



  )

;; Part 2


(comment

  ) 

