(ns advent-of-code-clojure.2021.day-12
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clojure.set :as cset]
            [clojure.pprint :as pprint]))

(defn parse-input [s]
  (->> s
       (inputs/lines)
       (reduce (fn [acc l]
                 (let [[a b] (string/split l #"-")]
                   (-> acc
                       (update a (fnil #(conj % b) #{}))
                       (update b (fnil #(conj % a) #{})))))
               {})))

(def input
  (parse-input
   (inputs/get-input-for-day 2021 12)))

(def test-input
  (parse-input
   "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))


; Part 1

(defn grow-path [cave-map path]
  (when-let [next-nodes
             (and (not= (last path) "end")
                  (filter (fn [node]
                            (or (= node (string/upper-case node))
                                (not ((set path) node))))
                          (cave-map (last path))))]
    (mapv #(conj path %) next-nodes)))

(defn find-paths [cave-map]
  (loop [paths #{["start"]}]
    (let [new-paths (->> paths
                         (mapcat (partial grow-path cave-map))
                         (filter some?)
                         (into paths))]
      (if (= new-paths paths)
        paths
        (recur new-paths)))))

(defn valid-paths [paths]
  (filter #(= (last %) "end") paths))

(comment
  (count (valid-paths (find-paths test-input)))
  (count (valid-paths (find-paths input))))
; Answer = 3738


; Part 2
