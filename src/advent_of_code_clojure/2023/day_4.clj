(ns advent-of-code-clojure.2023.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.set :as set]
            [clojure.string :as str]))


(defn parse-line [line]
  (let [[a b] (str/split (second (str/split line #":")) #"\|")]
    [(set (map parse-long (remove str/blank? (str/split a #"\s"))))
     (set (map parse-long (remove str/blank? (str/split b #"\s"))))]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (map parse-line)))


(def example
  (->> "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
       (inputs/lines)
       (map parse-line)))

;; Part 1

(defn matches [card] (count (apply set/intersection card)))

(comment

  (->> input
       (map matches)
       (keep #(last (take % (iterate (partial * 2) 1))))
       (reduce +))

  ; Answer = 19855

  )

;; Part 2

(defn copy-matches [inp]
  (reduce (fn [inp i]
            (let [{:keys [matches copies]} (nth inp i)]
              (vec (concat (take (inc i) inp)
                           (->> (drop (inc i) inp)
                                (take matches)
                                (mapv #(update % :copies + copies)))
                           (drop (+ 1 i matches) inp)))))
          inp
          (range (count inp))))

(comment

  (->> example
       (map matches)
       (mapv (fn [match-count] {:matches match-count :copies 1}))
       (copy-matches)
       (map :copies)
       (reduce +))

  (->> input
       (map matches)
       (mapv (fn [match-count] {:matches match-count :copies 1}))
       (copy-matches)
       (map :copies)
       (reduce +))

  ; Answer = 10378710

  )

