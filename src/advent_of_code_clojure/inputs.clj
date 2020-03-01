(ns advent-of-code-clojure.inputs
  (:require [clojure.string :as string]))


(defn get-input-for-day
  [year n]
  (slurp (str "resources/inputs/" year "/day" n ".txt")))

(defn read-longs
  [input]
  (->> input
       string/split-lines
       (map #(Long/parseLong %))))

(defn lines
  [input]
  (string/split-lines input))
