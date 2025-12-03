(ns advent-of-code-clojure.2025.day-3
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(defn char->long [c]
  (- (int c) 48))

(def example-input
  (->> "987654321111111\n811111111111119\n234234234234278\n818181911112111"
       (inputs/lines)
       (mapv #(mapv char->long %))))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv #(mapv char->long %))))



;; Part 1

(defn largest-n-joltage [n nums]
  (if (= 0 n)
    []
    (let [largest (apply max (drop-last (dec n) nums))]
      (cons largest
            (->> nums
                 (drop-while #(not= largest %))
                 (rest)
                 (largest-n-joltage (dec n)))))))

(comment

  (->> example-input
       (map (fn [nums]
              {:row     nums
               :joltage (parse-long (str/join (largest-n-joltage 2 nums)))}))
       (map :joltage)
       (reduce +))

  (->> input
       (map (fn [nums]
              {:row     nums
               :joltage (parse-long (str/join (largest-n-joltage 2 nums)))}))
       (map :joltage)
       (reduce +))

  ; Answer: 17324
  )

;; Part 2


(comment

  (->> input
       (map (fn [nums]
              {:row     nums
               :joltage (parse-long (str/join (largest-n-joltage 12 nums)))}))
       (map :joltage)
       (reduce +))

  ; Answer: 171846613143331

  )

