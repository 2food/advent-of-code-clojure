(ns advent-of-code-clojure.inputs
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn filename [year day]
  (str "resources/inputs/" year "/day" day ".txt"))

(def aoc-session "53616c7465645f5fd8f48f0e5870b405d7e575ec435ab7a647ff978d46dac62e773b0b21ec31b492fcbbf9cb101f1670")

(defn- download [year day]
  (let [url (str "https://adventofcode.com/" year "/day/" day "/input")
        req {:cookies {"session" {:value aoc-session}}}]
    (->> (client/get url req)
         :body
         (spit (filename year day)))))

(defn get-input-for-day
  [year day]
  (let [file (filename year day)]
    (when (not (.exists (io/file file)))
      (download year day))
    (slurp file)))

(comment
  (get-input-for-day 2021 1))

(defn read-longs
  [input]
  (->> input
       string/split-lines
       (map #(Long/parseLong %))))

(defn lines
  [input]
  (string/split-lines input))

(defn words [input]
  (string/split input #" "))

(defn lines-and-words [input]
  (->> input
       (lines)
       (map words)))