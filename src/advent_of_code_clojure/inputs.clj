(ns advent-of-code-clojure.inputs
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn filename [year day]
  (str "resources/inputs/" year "/day" day ".txt"))

(def aoc-session-cookie
  "53616c7465645f5f9b2a3c5fc88d9b0d0578d4488b3c99dc7f0f13c0604fb20bdc18275907b3d3109e24204fc64f1027d674398452cafb5883fe5ea6b43ae4f2")

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