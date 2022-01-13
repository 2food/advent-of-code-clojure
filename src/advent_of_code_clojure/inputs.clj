(ns advent-of-code-clojure.inputs
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn filename [year day]
  (str "resources/inputs/" year "/day" day ".txt"))

(def aoc-session 
  "53616c7465645f5fdf24638511d44ee8e78307c50af488d6234256c3a826dc9b5a480c05480e96c508dea4f3bc013c59")

(defn- download [year day]
  (let [url (str "https://adventofcode.com/" year "/day/" day "/input")
        req {:cookies {"session" {:value aoc-session}}}]
    (->> (client/get url req)
         :body
         (spit (filename year day)))))

(defn get-input-for-day [year day]
  (let [file (filename year day)]
    (when (not (.exists (io/file file)))
      (download year day))
    (slurp file)))

(defn get-input-for-this-day
  "Gets the right input data by namespace."
  []
  (let [[_ year day] (re-matches #"advent-of-code-clojure\.(....)\.day-(.+)"
                                 (str *ns*))]
    (get-input-for-day year day)))

(defn read-longs [input]
  (->> input
       string/split-lines
       (map #(Long/parseLong %))))

(defn lines [input]
  (string/split-lines input))

(defn words [input]
  (string/split input #" "))

(defn lines-and-words [input]
  (->> input
       (lines)
       (map words)))