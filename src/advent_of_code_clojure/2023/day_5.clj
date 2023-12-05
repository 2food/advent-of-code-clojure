(ns advent-of-code-clojure.2023.day-5
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-mapping [mapping]
  (let [[dest source range] (map parse-long mapping)]
    {:dest dest :source source :map-range range}))

(defn parse-almanac-map [header & mappings]
  {(keyword (first header)) (mapv parse-mapping mappings)})

(defn parse-almanac [lines]
  (let [chunks (->> lines
                    (partition-by (comp str/blank? first))
                    (remove (comp str/blank? ffirst)))
        seeds  (mapv parse-long (rest (first (first chunks))))]
    (into {:seeds seeds}
          (map (partial apply parse-almanac-map) (rest chunks)))))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (parse-almanac)))

(def example
  (->> "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
       (inputs/lines-and-words)
       (parse-almanac)))

;; Part 1

(defn map-number [num mapping]
  (if-let [{:keys [source dest]} (some (fn [{:keys [source map-range] :as m}]
                                         (and (<= source num)
                                              (<= num (+ source map-range))
                                              m))
                                       mapping)]
    (+ dest (- num source))
    num))

(defn map-chain [num mappings]
  (reduce (fn [n m] (map-number n m))
          num
          mappings))

(defn seed-to-location [input seed-num]
  (map-chain seed-num ((juxt :seed-to-soil :soil-to-fertilizer :fertilizer-to-water :water-to-light
                             :light-to-temperature :temperature-to-humidity :humidity-to-location)
                       input)))

(comment

  (let [input example]
    (->> (:seeds input)
         (apply min-key (partial seed-to-location input))
         (seed-to-location input)))

  (->> (:seeds input)
       (apply min-key (partial seed-to-location input))
       (seed-to-location input))

  ; Answer = 282277027

  )

;; Part 2


(defn reverse-mapping [mapping]
  (map #(set/rename-keys % {:source :dest :dest :source}) mapping))

(defn location-to-seed [input loc-num]
  (->> input
       ((juxt :humidity-to-location :temperature-to-humidity :light-to-temperature :water-to-light
              :fertilizer-to-water :soil-to-fertilizer :seed-to-soil))
       (map reverse-mapping)
       (map-chain loc-num)))

(defn in-seed-ranges? [input seed-ranges loc-num]
  (when (some (fn [[start n]]
                (let [seed-num (location-to-seed input loc-num)]
                  (and (<= start seed-num)
                       (<= seed-num (+ start n))
                       seed-num)))
              seed-ranges)
    loc-num))

(comment

  (let [input       example
        seed-ranges (partition 2 (:seeds input))]
    (->> (range 100)
         (map (partial in-seed-ranges? input seed-ranges))
         (filter some?)
         (first)))

  (do (prn (str (java.time.LocalDateTime/now)))
      (time (let [seed-ranges (partition 2 (:seeds input))]
              (->> (range)
                   (pmap (partial in-seed-ranges? input seed-ranges))
                   (filter some?)
                   (first)))))

  ; Answer = 11554135
  ; Took 1.5 hours to compute so not exatcly ideal.

  )

