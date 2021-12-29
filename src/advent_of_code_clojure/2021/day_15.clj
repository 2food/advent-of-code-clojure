(ns advent-of-code-clojure.2021.day-15
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [advent-of-code-clojure.utils :as utils]))


(defn parse-input [s]
  (->> s
       (inputs/lines)
       (mapv (comp (partial mapv read-string)
                   #(string/split % #"")))))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"))


; Part 1

(defn all-infs [inds]
  (apply hash-map (mapcat #(vector % ##Inf) inds)))

(defn sorted-queue [dist queue]
  (sort-by dist queue))

(defn add-neighbours [dist prev queue m u]
  (let [neighs (utils/get-neighbours-4 m (first u) (last u))]
    (reduce (fn [[d p q] n]
              (let [[x y] n
                    alt (+ (d u) (utils/get-coord m x y))]
                (if (< alt (d n))
                    (let [dist (assoc d n alt)]
                      [dist
                       (assoc p n u)
                       (sorted-queue dist (cons n q))])
                    [d p q])))
            [dist prev queue]
            neighs)))

(defn dejkstra [m]
  (let [Q (utils/all-coords m)
        target (last (sort Q))]
    (println (str "Starting with |Q| = " (count Q)))
    (loop [queue [[0 0]]
           visited 0
           dist (-> (all-infs Q)
                    (assoc [0 0] 0))
           prev {}]
      (when (= 0 (mod visited 1000))
        (println (str "|visited| = " visited)))
      (let [u (first queue)]
        (if (= u target)
          (do (println (str "|visited| = " visited))
              [dist prev])
          (let [queue (rest queue)
                visited (inc visited)
                [dist prev queue] (add-neighbours dist prev queue m u)]
            (recur queue visited dist prev)))))))

(defn construct-path [target prev]
  (if-let [next (prev target)]
    (conj (construct-path next prev) target)
    [target]))

(defn debug [inp]
  (let [[dist prev] (dejkstra inp)
        target (last (sort (utils/all-coords inp)))
        path  (set (construct-path target prev))
        printout (->> inp
                      (map-indexed (fn [i row]
                                     (->> row 
                                          (map-indexed (fn [j n] (if (path [j i]) "," n)))
                                          (string/join))))
                      (string/join "\n"))]
    (spit "debug.txt" printout)
    (dist target)))

(defn find-path-risk [inp]
  (let [[dist _] (dejkstra inp)
        target (last (sort (utils/all-coords inp)))]
    (dist target)))

(comment 
  (find-path-risk test-input)
  (find-path-risk input))
; Answer = 423


; Part 2

(defn conj-matrix-hor [a b]
  (mapv into a b))

(defn conj-matrix-vert [a b]
  (into a b))

(defn +-wrap-around [n x]
  (let [res (+ x n)]
    (if (> res 9)
      (inc (mod res 10))
      res)))

(defn add-to-all [m n]
  (mapv (partial mapv (partial +-wrap-around n)) m))

(defn expand-matrix [m]
  (reduce
   conj-matrix-hor
   (for [x (range 0 5)]
     (reduce conj-matrix-vert
             (for [y (range 0 5)]
               (add-to-all m (+ y x)))))))

(def new-input (expand-matrix input))
(def new-test-input (expand-matrix test-input))

(comment
  (find-path-risk new-test-input)
  (find-path-risk new-input))
; Answer = 2778
