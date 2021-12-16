(ns advent-of-code-clojure.2021.day-12
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

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

(defn is-upper-case? [node]
  (= node (string/upper-case node)))

(defn not-taken-already? [path node]
  (not ((set path) node)))

(defn not-start? [node] (not (= "start" node)))

(defn not-end? [node] (not (= "end" node)))

(defn grow-path
  ([cave-map path]
   (letfn [(valid-node? [node] (or (is-upper-case? node)
                                   (not-taken-already? path node)))]
     (grow-path cave-map path valid-node?)))
  ([cave-map path valid-fn]
   (when (not-end? (last path))
     (when-let [next-nodes (filter valid-fn (cave-map (last path)))]
       (mapv #(conj path %) next-nodes)))))

(defn valid-paths [paths]
  (filter #(= (last %) "end") paths))

(defn find-paths
  ([cave-map]
   (find-paths cave-map grow-path))
  ([cave-map grow-fn]
   (loop [paths #{["start"]}]
     (let [new-paths (->> paths
                          (mapcat (partial grow-fn cave-map))
                          (filter some?)
                          (into paths))]
       (if (= new-paths paths)
         (valid-paths paths)
         (recur new-paths))))))

(comment
  (count (find-paths test-input))
  (count (find-paths input)))
; Answer = 3738


; Part 2

(defn grow-path-2 [cave-map path]
  (letfn [(no-previous-doubles? [path] (->> path
                                            (filter (comp not is-upper-case?))
                                            (frequencies)
                                            (vals)
                                            (some #(= 2 %))
                                            (not)))
          (valid-node? [node] (or (is-upper-case? node)
                                  (and (not-start? node)
                                       (or (no-previous-doubles? path)
                                           (not-taken-already? path node)))))]
    (grow-path cave-map path valid-node?)))

(defn find-paths-2 [cave-map]
  (find-paths cave-map grow-path-2))

(comment
  (count (find-paths-2 test-input))
  (count (find-paths-2 input)))
; Answer = 120506