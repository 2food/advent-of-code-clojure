(ns advent-of-code-2018.inputs
  (:require [clojure.string :as string]))


(defn get-input-for-day
  [n]
  (slurp (str "resources/inputs/day" n ".txt")))

(defn longs
  [input]
  (->> input
       string/split-lines
       (map #(Long/parseLong %))))

(defn lines
  [input]
  (string/split-lines input))