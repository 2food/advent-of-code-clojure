(ns advent-of-code-clojure.2022.day-13
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(def input
  (->> (string/split (inputs/get-input-for-this-day) #"\n\n")
       (mapv (fn [pairs] (mapv read-string (inputs/lines pairs))))))

(def test-input
  (->> (string/split "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
                     #"\n\n")
       (mapv (fn [pairs] (mapv read-string (inputs/lines pairs))))))

;; Part 1 

(defn traverse [a b]
  (cond
    (and (vector? a) (vector? b)) (conj (mapv traverse a b) (compare (count a) (count b)))
    (and (vector? a) (number? b)) (traverse a [b])
    (and (number? a) (vector? b)) (traverse [a] b)
    :else (compare a b)))

(defn right-order? [a b]
  (->> (flatten (traverse a b))
       (some #(and (not (zero? %)) %))
       (= -1)))

(defn sum-order-indices [input]
  (->> (map-indexed vector input)
       (reduce (fn [s [ind [a b]]]
                 (if (right-order? a b)
                   (conj s (inc ind))
                   s))
               #{})
       (reduce +)))

(comment
  (right-order? [1 1 3 1 1] [1 1 5 1 1])
  (right-order? [[1] [2 3 4]] [[1] 4])
  (right-order? [9] [[8 7 6]])
  (right-order? [[4 4] 4 4] [[4 4] 4 4 4])
  (right-order? [7 7 7 7] [7 7 7])
  (right-order? [] [3])
  (right-order? [[[]]] [[]])
  (right-order? [1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9])

  (map (partial apply traverse) test-input)

  (sum-order-indices test-input)
  (sum-order-indices input)
  ; Answer = 5350
  )

;; Part 2


(comment

  ) 

