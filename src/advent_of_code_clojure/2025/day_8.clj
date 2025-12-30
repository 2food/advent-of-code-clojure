(ns advent-of-code-clojure.2025.day-8
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]))

(def example-input
  (->> "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
       (inputs/lines)
       (mapv #(mapv parse-long (str/split % #",")))))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv #(mapv parse-long (str/split % #",")))))

;; Part 1


(defn init-circuits [input]
  (into {} (map (fn [p] [p #{p}]) input)))

(defn connect-pair [circuits a b]
  (let [new-circuit (set/union (circuits a) (circuits b))]
    (reduce (fn [acc key] (update acc key set/union new-circuit)) circuits new-circuit)))

(defn all-pairs [input]
  (->> input
       (mapcat (fn [p] (->> input
                            (keep #(when (not= p %) (hash-set p %))))))
       (set)
       (map vec)
       (sort-by #(apply utils/distance %))))

(defn run [input steps]
  (let [pairs (all-pairs input)]
    (loop [circuits (init-circuits input)
           pairs    pairs
           counter  0]
      (if (< counter steps)
        (let [pair      (first pairs)
              new-state (apply connect-pair circuits pair)]
          (recur new-state (rest pairs) (inc counter)))
        circuits))))

(comment

  (all-pairs example-input)

  (->> (run example-input 10)
       (vals)
       (set)
       (map count)
       (sort-by -)
       (take 3)
       (reduce *))

  (def res (run input 1000))

  (->> res
       (vals)
       (set)
       (map count)
       (sort-by -)
       (take 3)
       (reduce *))

  ; Answer = 133574
  )

;; Part 2


(defn run2 [input]
  (let [pairs (all-pairs input)]
    (loop [state (init-circuits input)
           pairs pairs]
      (when (= 0 (mod (count pairs) 1000))
        (prn (count pairs)))
      (if-let [pair (first pairs)]
        (let [new-state (apply connect-pair state pair)]
          (if (apply = (vals (:circuits new-state)))
            pair
            (recur new-state (rest pairs))))
        state))))

(comment

  (let [[a b] (run2 example-input)]
    (* (first a) (first b)))

  (let [[a b] (run2 input)]
    (* (first a) (first b)))

  )

