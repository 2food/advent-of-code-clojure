(ns advent-of-code-clojure.2025.day-9
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.string :as str]))

(def example-input
  (->> "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
       (inputs/lines)
       (mapv #(mapv parse-long (str/split % #",")))))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv #(mapv parse-long (str/split % #",")))))

;; Part 1

(defn area [p1 p2]
  (* (inc (abs (- (first p1) (first p2))))
     (inc (abs (- (second p1) (second p2))))))

(defn all-pairs [input]
  (->> input
       (mapcat (fn [p] (keep #(when (not= p %)
                                (hash-set p %))
                             input)))
       (set)
       (map vec)
       (sort-by #(apply area %))
       (reverse)))

(comment

  (all-pairs example-input)
  (let [[a b] (first (all-pairs example-input))]
    [[a b] (area a b)])


  (all-pairs input)
  (let [[a b] (first (all-pairs input))]
    [[a b] (area a b)])

  ; Answer = 4763509452

  )

;; Part 2

(defn green-lines [inp]
  (conj (mapv vec (partition 2 1 inp))
        [(first inp) (last inp)]))

(defn bbox-map [inp]
  (let [lines (green-lines inp)]
    (reduce (fn [acc [[ax ay] [bx by]]]
              (let [miny (min ay by)
                    maxy (max ay by)
                    minx (min ax bx)
                    maxx (max ax bx)]
                {:min-x (merge-with min (:min-x acc) (into {} (for [y (range miny (inc maxy))]
                                                                [y minx])))
                 :max-x (merge-with max (:max-x acc) (into {} (for [y (range miny (inc maxy))]
                                                                [y maxx])))
                 :min-y (merge-with min (:min-y acc) (into {} (for [x (range minx (inc maxx))]
                                                                [x miny])))
                 :max-y (merge-with max (:max-y acc) (into {} (for [x (range minx (inc maxx))]
                                                                [x maxy])))}))
            {:max-x {}
             :max-y {}
             :min-x {}
             :min-y {}}
            lines)))

(defn within-bbox? [{:keys [min-x max-x min-y max-y]} [x y]]
  (and (min-x y)
       (max-x y)
       (min-y x)
       (max-y x)
       (<= (min-x y) x (max-x y))
       (<= (min-y x) y (max-y x))))

(defn area-within-bbox? [bbox [[ax ay] [bx by]]]
  (let [miny (min ay by)
        maxy (max ay by)
        minx (min ax bx)
        maxx (max ax bx)]
    ; most can be eliminated by only checking the corners,
    ; which can be much faster than checking the whole outline
    (and (within-bbox? bbox [minx miny])
         (within-bbox? bbox [minx maxy])
         (within-bbox? bbox [maxx maxy])
         (within-bbox? bbox [maxx miny])
         (every? #(within-bbox? bbox %)
                 (concat (for [x (range minx (inc maxx))]
                           [x miny])
                         (for [x (range minx (inc maxx))]
                           [x maxy])
                         (for [y (range miny (inc maxy))]
                           [minx y])
                         (for [y (range miny (inc maxy))]
                           [maxx y]))))))


(comment

  (area-within-bbox? (bbox-map example-input)
                     [[2 3] [11 7]])

  (let [inp   example-input
        pairs (all-pairs inp)
        bbox  (bbox-map inp)
        [a b] (first (filter #(area-within-bbox? bbox %) pairs))]
    [[a b] (area a b)])

  (time (let [inp   input
              pairs (all-pairs inp)
              bbox  (bbox-map inp)
              [a b] (first (filter #(area-within-bbox? bbox %) pairs))]
          [[a b] (area a b)]))

  ; Answer = 1516897893

  )

