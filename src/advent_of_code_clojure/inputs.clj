(ns advent-of-code-clojure.inputs
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn filename [year day]
  (str "resources/inputs/" year "/day" day ".txt"))

(def aoc-session-cookie
  "53616c7465645f5f3c0778dd4af78536483bce133292d0b17b9af3487bb9d0c4631f75e63d32f75342d3e54cb4e125d02d4fc520b013ba76dee173c8086c202f")

(defn- download [year day]
  (let [url (str "https://adventofcode.com/" year "/day/" day "/input")
        req {:cookies {"session" {:value aoc-session-cookie}}}]
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

(defn lines [input]
  (string/split-lines input))

(defn words [input]
  (string/split input #" "))

(defn lines-and-words [input]
  (->> input
       (lines)
       (map words)))

(defn read-longs [input]
  (->> input
       (lines)
       (map parse-long)))

(defn read-matrix [input]
  (->> input
       (lines-and-words)
       (mapv #(mapv parse-long %))))
