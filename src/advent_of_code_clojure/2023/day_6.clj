(ns advent-of-code-clojure.2023.day-6
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (map #(rest (remove str/blank? %)))
       (mapv #(mapv parse-long %))))

(def example
  (->> "Time:      7  15   30
Distance:  9  40  200"
       (inputs/lines-and-words)
       (map #(rest (remove str/blank? %)))
       (mapv #(mapv parse-long %))))

;; Part 1

(defn hold-time->distance [run-time hold-time]
  (max (* hold-time (- run-time hold-time))
       0))

(defn options [time dist]
  (->> (range time)
       (filter #(< dist (hold-time->distance time %)))))

(comment
  (let [[times dists] example]
    (->> (map (comp count options) times dists)
         (reduce *)))

  (let [[times dists] input]
    (->> (map (comp count options) times dists)
         (reduce *)))

  ; Answer = 2612736
  
  )

;; Part 2


(comment

  (let [[times dists] example
        time (parse-long (str/join times))
        dist (parse-long (str/join dists))]
    (count (options time dist)))

  (let [[times dists] input
        time (parse-long (str/join times))
        dist (parse-long (str/join dists))]
    (count (options time dist)))

  ; Answer = 29891250

  )

