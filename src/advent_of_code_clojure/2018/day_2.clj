(ns advent-of-code-clojure.2018.day-2
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))


(defn input [] (inputs/lines (inputs/get-input-for-day 2018 2)))


; Part 1


(defn count-occurances
  [s]
  (reduce (fn [acc c] (update acc c (fnil inc 0)))
          {}
          s))

(defn has-exactly-n?
  [n count-map]
  (or (some #(= % n) (vals count-map)) false))

(defn checksum
  [box-ids]
  (let [count-map   (map count-occurances box-ids)
        has-2-count (count (filter (partial has-exactly-n? 2) count-map))
        has-3-count (count (filter (partial has-exactly-n? 3) count-map))]
    (* has-2-count has-3-count)))

(defn answer-1 [] (checksum (input)))


; Part 2


(defn differ-by-n?
  [n stringa stringb]
  (loop [n       n
         stringa stringa
         stringb stringb]
    (cond
      (< n 0) false
      (or (empty? stringa) (empty? stringb)) true
      (= (first stringa) (first stringb)) (recur n
                                                 (string/join (rest stringa))
                                                 (string/join (rest stringb)))
      :else (recur (- n 1)
                  (string/join (rest stringa))
                  (string/join (rest stringb))))))

(defn find-differ-by-n
  [n strings]
  (or (some #(when (differ-by-n? n (first strings) %) %)
            (rest strings))
      (find-differ-by-n n (rest strings))))

(defn answer-2 [] (find-differ-by-n 1 (input)))