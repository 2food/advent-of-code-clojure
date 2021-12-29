(ns advent-of-code-clojure.2021.day-18
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [advent-of-code-clojure.utils :as utils]))

(defn build-tree [v]
  (if (number? v)
    v
    (let [[left right] v]
      {:left (build-tree left)
       :right (build-tree right)})))

(defn parse-input [s]
  (->> s
       (string/trim)
       (inputs/lines)
       (mapv read-string)
       (mapv build-tree)))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))


; Part 1

(defn is-leaf? [snailfish-num] (number? snailfish-num))
(defn is-tree? [snailfish-num] (not (is-leaf? snailfish-num)))

(defn some-n-deep? [n snailfish-num]
  (cond
    (and (= 0 n) (is-tree? snailfish-num)) []
    (is-leaf? snailfish-num) nil
    :else (some (fn [[k v]] (when-let [deep-path (some-n-deep? (dec n) v)]
                              (cons k deep-path)))
                snailfish-num)))

(defn some-4-deep? [snailfish-num]
  (some-n-deep? 4 snailfish-num))

(defn rightmost-child-of [snailfish-num]
  (if (is-leaf? snailfish-num)
    []
    (cons :right (rightmost-child-of (:right snailfish-num)))))

(defn some-left-of [snailfish-num path]
  (if (every? #(= :left %) path)
    false
    (let [path-to-investigate (->> (reverse path)
                                   (drop-while #(= % :left))
                                   (rest)
                                   (cons :left)
                                   (reverse))]
      (into (vec path-to-investigate)
            (rightmost-child-of (get-in snailfish-num path-to-investigate))))))

(defn leftmost-child-of [snailfish-num]
  (if (is-leaf? snailfish-num)
    []
    (cons :left (leftmost-child-of (:left snailfish-num)))))

(defn some-right-of [snailfish-num path]
  (if (every? #(= :right %) path)
    false
    (let [path-to-investigate (->> (reverse path)
                                   (drop-while #(= % :right))
                                   (rest)
                                   (cons :right)
                                   (reverse))]
      (into (vec path-to-investigate)
            (leftmost-child-of (get-in snailfish-num path-to-investigate))))))

(defn explode [snailfish-num]
  (let [path (some-4-deep? snailfish-num)
        exploded-node (get-in snailfish-num path)]
    (cond-> snailfish-num
      true (assoc-in path 0)
      (some-left-of snailfish-num path) (update-in (some-left-of snailfish-num path)
                                                   #(+ % (:left exploded-node)))
      (some-right-of snailfish-num path) (update-in (some-right-of snailfish-num path)
                                                    #(+ % (:right exploded-node))))))

(defn some->-9? [snailfish-num]
  (if (is-leaf? snailfish-num)
    (and (> snailfish-num 9) [])
    (some (fn [[k v]] (when-let [gt-path (some->-9? v)]
                        (cons k gt-path)))
          snailfish-num)))

(defn split [snailfish-num]
  (let [path (some->-9? snailfish-num)
        splitting-num (get-in snailfish-num path)
        left-num (int (Math/floor (/ splitting-num 2)))
        right-num (int (Math/ceil (/ splitting-num 2)))]
    (-> snailfish-num
        (assoc-in path {:left left-num :right right-num}))))

(defn reduce-num [n]
  (loop [n n]
    (cond
      (some-4-deep? n) (recur (explode n))
      (some->-9? n) (recur (split n))
      :else n)))

(defn add [a b]
  (reduce-num {:left a :right b}))

(defn sum [inp]
  (reduce add inp))

(defn magnitude [snailfish-num]
  (if (number? snailfish-num)
    snailfish-num
    (+ (* 3 (magnitude (:left snailfish-num)))
       (* 2 (magnitude (:right snailfish-num))))))

(comment
  (magnitude (sum input)))

; Part 2

(defn all-pairs [l]
  (for [a l
        b l]
    [a b]))

(defn largest-magnitude [inp]
  (->> (all-pairs inp)
       (map (fn [[a b]] (add a b)))
       (map magnitude)
       (apply max)))

(comment
  (largest-magnitude input))
