(ns advent-of-code-clojure.2025.day-2
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> (str/split (str/trim s) #",")
       (mapv #(mapv parse-long (str/split % #"-")))))

(def input
  (->> (inputs/get-input-for-this-day)
       (parse-input)))

(def example-input
  (->> "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
       (parse-input)))

;; Part 1

(defn first-half [s]
  (str/join (subvec (vec s) 0 (int (/ (count s) 2)))))

(defn valid? [num]
  (let [num-str (str num)]
    (not (and (even? (count num-str))
              (str/ends-with? num-str (first-half num-str))))))

(comment

  (->> example-input
       (mapcat (fn [[start end]] (range start (inc end))))
       (remove valid?)
       (reduce +))

  (->> input
       (mapcat (fn [[start end]] (range start (inc end))))
       (remove valid?)
       (reduce +))

  ; Answer: 55916882972
  )

;; Part 2

(defn possible-repeats [s]
  (map #(str/join (subvec (vec s) 0 %)) (range 1 (inc (int (/ (count s) 2))))))

(defn possible-repeating-matches [s]
  (map #(str/join (repeat (/ (count s) (count %)) %)) (possible-repeats s)))

(comment
  (possible-repeating-matches "asdfasdf"))

(defn valid?-2 [num]
  (let [num-str (str num)]
    (not-any? #(= num-str %) (possible-repeating-matches num-str))))


(comment

  (->> example-input
       (mapcat (fn [[start end]] (range start (inc end))))
       (remove valid?-2)
       (reduce +))

  (->> input
       (mapcat (fn [[start end]] (range start (inc end))))
       (remove valid?-2)
       (reduce +))

  ; Answer: 76169125915
  )

