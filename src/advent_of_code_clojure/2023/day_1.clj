(ns advent-of-code-clojure.2023.day-1
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)))

;; Part 1

(defn line->num [line]
  (let [digits (re-seq #"\d" line)]
    (parse-long (str (first digits) (last digits)))))

(comment

  (->> (map line->num input)
       (reduce +))

  ;; Answer = 55816

  )

;; Part 2

(def example (-> "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
                 (inputs/lines)))

(defn word->num-str [x]
  (case x
    "one" "1"
    "two" "2"
    "three" "3"
    "four" "4"
    "five" "5"
    "six" "6"
    "seven" "7"
    "eight" "8"
    "nine" "9"
    x))

(defn line->num2 [line]
  (let [digits (->> (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))" line)
                    (map last)
                    (map word->num-str))]
    (prn digits)
    (parse-long (str (first digits) (last digits)))))

(comment

  (->> (map line->num2 example)
       (reduce +))

  (->> (map line->num2 input)
       (reduce +))

  ;; Answer = 54980

  )

