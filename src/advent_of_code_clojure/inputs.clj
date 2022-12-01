(ns advent-of-code-clojure.inputs
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn filename [year day]
  (str "resources/inputs/" year "/day" day ".txt"))

(def aoc-session 
  "53616c7465645f5f91e0ab2e65b14f268c2146e494db0bbc9daf393ac4caba251a506c21bae8c4954b5d9fb521ec7919214948584a3509bacfd742342b6dd747")

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