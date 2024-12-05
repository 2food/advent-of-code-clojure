(ns advent-of-code-clojure.2024.day-5
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(defn read-input [input]
  (let [[rules _ updates] (->> input
                               (inputs/lines)
                               (partition-by str/blank?))]
    {:rules   (->> rules
                   (map #(mapv parse-long (str/split % #"\|")))
                   (reduce (fn [acc [k v]]
                             (merge-with into acc {k #{v}}))
                           {}))
     :updates (mapv #(mapv parse-long (str/split % #",")) updates)}))

(def input (read-input (inputs/get-input-for-day 2024 5)))
(def example (read-input "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"))

;; Part 1

(defn update-unsorted? [rules update]
  (first (keep-indexed (fn [idx x]
                         (when (some (get rules x #{})
                                     (set (subvec update 0 idx)))
                           x))
                       update)))

(defn middle-number [update]
  (nth update (long (/ (count update) 2))))

(comment

  (->> (:updates example)
       (remove (partial update-unsorted? (:rules example)))
       (map middle-number)
       (reduce +))

  (->> (:updates input)
       (remove (partial update-unsorted? (:rules input)))
       (map middle-number)
       (reduce +))

  ; Answer = 4569

  )

;; Part 2

(defn compare-fn [rules a b]
  (if (get (get rules b)
           a)
    0
    -1))

(comment
  (let [{:keys [rules updates]} example]
    (->> updates
         (filter #(update-unsorted? rules %))
         (map #(sort-by identity (partial compare-fn rules) %))
         (map middle-number)
         (reduce +)))

  (let [{:keys [rules updates]} input]
    (->> updates
         (filter #(update-unsorted? rules %))
         (map #(sort-by identity (partial compare-fn rules) %))
         (map middle-number)
         (reduce +)))

  ; Answer = 64567

  )
