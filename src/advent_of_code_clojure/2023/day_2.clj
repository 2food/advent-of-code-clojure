(ns advent-of-code-clojure.2023.day-2
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(defn parse-line [line]
  (->> (str/split (second (str/split line #":")) #";")
       (map #(->> (str/split % #",|\s")
                  (remove str/blank?)
                  (reverse)
                  (apply hash-map)))
       (mapv #(-> %
                  (update-vals parse-long)
                  (update-keys keyword)))))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv parse-line)))

(def example
  (->> "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"
       (inputs/lines)
       (mapv parse-line)))

;; Part 1

(def limits {:red 12 :green 13 :blue 14})

(defn valid-draw? [{:keys [red green blue]}]
  (and (or (nil? red) (<= red (:red limits)))
       (or (nil? green) (<= green (:green limits)))
       (or (nil? blue) (<= blue (:blue limits)))))

(defn valid-game? [game]
  (every? valid-draw? game))

(comment

  (->> input
       (keep-indexed (fn [idx game]
                       (when (valid-game? game) idx)))
       (map inc)
       (reduce +))

  ; Answer = 2101
  
  )

;; Part 2


(defn minimum-valid-set [game]
  (reduce (partial merge-with max)
          {:red 0 :green 0 :blue 0}
          game))

(defn power [{:keys [red green blue]}]
  (* red green blue))

(comment

  (->> example
       (map minimum-valid-set)
       (map power)
       (reduce +))

  (->> input
       (map minimum-valid-set)
       (map power)
       (reduce +))

  ; Answer = 58269

  )

