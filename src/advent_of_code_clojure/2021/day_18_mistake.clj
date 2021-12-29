(ns advent-of-code-clojure.2021.day-18-old
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> s
       (string/trim)
       (inputs/lines)
       (mapv read-string)))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

; Part 1

(defn- some-n-deep? [num n]
  (when (seqable? num)
    (if (= n 0)
      num
      (some #(some-n-deep? % (dec n)) num))))

(defn some-4-deep? [num]
  (some-n-deep? num 4))

(defn escape-vec [v]
  (string/escape (str v) {(first "[") "l" (first "]") "r"}))

(defn unescape-vec [v]
  (string/escape (str v) {\l "[" \r "]"}))


(defn add-to-right-part [s num]
  (let [num-pattern #"\d+"
        matching-num (re-find num-pattern s)]
    (if matching-num
      (string/replace-first s num-pattern (str (+ (read-string matching-num) num)))
      s)))

(defn add-to-left-part [s num]
  (let [num-pattern #"\d+"
        matching-seq (re-seq num-pattern s)]
    (if-let [first-match (first matching-seq)]
      (if (> (count matching-seq) 1)
        (let [[start rest] (string/split s (re-pattern first-match) 2)]
          (str start first-match (add-to-left-part rest num)))
        (string/replace-first s num-pattern (str (+ (read-string first-match) num))))
      s)))

(defn explode [n]
  (let [explodable (some-4-deep? n)
        [left-num right-num] explodable
        [left-part right-part] (string/split (escape-vec n) (re-pattern (escape-vec explodable)) 2)
        left-part (-> (unescape-vec left-part)
                      (add-to-left-part left-num))
        right-part (-> (unescape-vec right-part)
                       (add-to-right-part right-num))]
    (-> (str left-part " 0 " right-part)
        (parse-input)
        (first))))

(defn some->-9? [n]
  (->> n
       (flatten)
       (some #(and (> % 9) %))))

(defn split [n]
  (let [splitable (some->-9? n)
        [left-part right-part] (string/split (str n) (re-pattern (str splitable)) 2)
        left-num (int (Math/floor (/ splitable 2)))
        right-num (int (Math/ceil (/ splitable 2)))]
    (-> (str left-part "[" left-num " " right-num "]" right-part)
        (parse-input)
        (first))))

(defn reduce-num [n]
  (loop [n n]
    (println n)
    (println (some-4-deep? n) (some->-9? n))
    (cond
      (some-4-deep? n) (recur (explode n))
      (some->-9? n) (recur (split n))
      :else n)))

(defn add [a b]
  (reduce-num [a b]))

(defn sum [inp]
  (reduce add inp))

(defn magnitude [n]
  (if (number? n)
    n
    (let [[left right] n]
      (+ (* 3 (magnitude left)) (* 2 (magnitude right))))))

(comment
  (magnitude (sum input)))

; Too low = 1684


; This way of solving it failed due to a fundamental error.
; The error was to rely on string matching. 
; This worked for almost everything, except when were multiple equal pairs the snailfish number. 
; Which were annoingly rare, making this mistake time-consuming to discover.
; Ultimately I had to scrap all of this and solve it with trees instead, 
; which I considered initially, but for some reason discarded.

