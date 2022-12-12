(ns advent-of-code-clojure.2022.day-12
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv vec)))

(def test-input
  (->> "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
       (inputs/lines)
       (mapv vec)))

;; Part 1

(def start [20 0])
(def top [20 107])
(def test-start [0 0])
(def test-top [2 5])

(defn init-unvisited [hm]
  (set (for [y (range (count hm))
             x (range (count (first hm)))]
         [y x])))

(defn init-distances [unvisited start]
  (-> (zipmap unvisited (repeat ##Inf))
      (assoc start 0)))

(comment
  (some #(and (= \E (get-in input %)) %) (init-unvisited input))
  (apply min-key init-distances init-unvisited))

(defn height [char]
  (case char
    \S (int \a)
    \E (int \z)
    (int char)))

(defn neighbours [hm [y x]]
  (let [ns [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]]
    (filter #(and (get-in hm %)
                  (>= (height (get-in hm %))
                      (dec (height (get-in hm [y x])))))
            ns)))

(comment
  (neighbours test-input [0 2]))

(defn dijkstra [heigh-map start]
  (loop [unvisited (init-unvisited heigh-map)
         distances (init-distances unvisited start)]
    (if (empty? unvisited)
      distances
      (let [current           (apply min-key distances unvisited)
            dist-to-neighbour (inc (distances current))
            neighs            (neighbours heigh-map current)
            new-distances     (reduce (fn [dists n]
                                        (if (< dist-to-neighbour (dists n))
                                          (assoc dists n dist-to-neighbour)
                                          dists))
                                      distances neighs)
            new-unvisited     (set (remove #{current} unvisited))]
        (recur new-unvisited new-distances)))))

(defn shortest-dist [hm start end]
  (let [distances (dijkstra hm start)]
    (distances end)))

(comment
  (shortest-dist test-input test-top test-start)
  (shortest-dist input top start)
  ; Answer = 449
  )

;; Part 2

(defn ground-points [hm]
  (filter #(= (height \a) (height (get-in hm %))) (init-unvisited hm)))

(comment
  (let [ends                  (ground-points input)
        distances             (dijkstra input [20 107])
        shortest-ground-point (apply min-key distances ends)]
    (distances shortest-ground-point))
  ; Answer = 443
  ) 

