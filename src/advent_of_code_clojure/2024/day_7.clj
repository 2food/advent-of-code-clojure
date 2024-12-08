(ns advent-of-code-clojure.2024.day-7
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.core.reducers :as r]
            [clojure.string :as str]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv (fn [s] (let [[test-val nums] (str/split s #":")]
                       {:test-value    (parse-long test-val)
                        :equation-nums (vec (keep parse-long (str/split nums #" ")))})))))

(def example
  (->> "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
       (inputs/lines)
       (mapv (fn [s] (let [[test-val nums] (str/split s #":")]
                       {:test-value    (parse-long test-val)
                        :equation-nums (vec (keep parse-long (str/split nums #" ")))})))))

;; Part 1

(defn solutions [operators acc [x & others]]
  (if-not x
    #{acc}
    (set (mapcat #(solutions operators % others)
                 (map (fn [op] (op acc x))
                      operators)))))

(def operators [+ *])

(comment
  (->> example
       (map #(assoc % :possible-solutions (solutions operators
                                                     (first (:equation-nums %))
                                                     (rest (:equation-nums %))))))


  (->> input
       (map #(assoc % :possible-solutions (solutions operators
                                                     (first (:equation-nums %))
                                                     (rest (:equation-nums %)))))
       (filter (fn [{:keys [possible-solutions test-value]}] (possible-solutions test-value)))
       (map :test-value)
       (reduce +))

  ; Answer = 12940396350192
  )

;; Part 2

(defn || [a b] (parse-long (str a b)))
(def operators2 [+ * ||])

(comment

  (->> example
       (map #(assoc % :possible-solutions (solutions operators2
                                                     (first (:equation-nums %))
                                                     (rest (:equation-nums %))))))

  (time (->> input
             (r/map #(assoc % :possible-solutions (solutions operators2
                                                           (first (:equation-nums %))
                                                           (rest (:equation-nums %)))))
             (r/filter (fn [{:keys [possible-solutions test-value]}] (possible-solutions test-value)))
             (r/map :test-value)
             (r/fold +)))

  ; Answer = 106016735664498

  )
