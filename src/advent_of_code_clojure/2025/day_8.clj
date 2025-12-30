(ns advent-of-code-clojure.2025.day-8
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
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


(defn get-circuit [circuits p]
  (or (first (filter #(contains? % p) circuits))
      #{p}))

(defn connect-pair [circuits [a b]]
  (let [a-circuit (get-circuit circuits a)
        b-circuit (get-circuit circuits b)]
    (conj (set/difference circuits #{a-circuit} #{b-circuit})
          (set/union a-circuit b-circuit))))

(defn all-pairs [input]
  (->> input
       (mapcat (fn [p] (keep #(when (not= p %)
                                (hash-set p %))
                             input)))
       (set)
       (map vec)
       (sort-by #(apply utils/distance %))))

(defn run [pairs steps]
  (loop [circuits #{}
         pairs    pairs
         counter  0]
    (if (< counter steps)
      (recur (connect-pair circuits (first pairs))
             (rest pairs)
             (inc counter))
      circuits)))

(comment

  (def example-pairs (all-pairs example-input))
  (->> (run example-pairs 10)
       (map count)
       (sort-by -)
       (take 3)
       (reduce *))


  (def pairs (all-pairs input))
  (->> (run pairs 1000)
       (map count)
       (sort-by -)
       (take 3)
       (reduce *))

  ; Answer = 133574
  )

;; Part 2


(defn run2 [pairs]
  (let [point-count (count (set (apply concat pairs)))]
    (loop [circuits #{}
           pairs    pairs]
      (let [pair         (first pairs)
            new-circuits (connect-pair circuits pair)]
        (if (and (= 1 (count new-circuits))
                 (= point-count (count (first new-circuits))))
          pair
          (recur new-circuits (rest pairs)))))))

(comment


  (def example-pairs (all-pairs example-input))
  (let [[a b] (run2 example-pairs)]
    (* (first a) (first b)))

  (def pairs (all-pairs input))
  (let [[a b] (run2 pairs)]
    (* (first a) (first b)))

  ; Answer = 2435100380

  )

