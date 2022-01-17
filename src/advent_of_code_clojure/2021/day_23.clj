(ns advent-of-code-clojure.2021.day-23
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clojure.core.reducers :as r]
            [advent-of-code-clojure.utils :as utils]
            [clojure.set :as cset]))

(def amphipod-map
  {:h1  [:h2]
   :h2  [:h1 :h3]
   :h3  [:h2 :h4 :a1]
   :h4 [:h3 :h5]
   :h5 [:h4 :h6 :b1]
   :h6 [:h5 :h7]
   :h7 [:h6 :h8 :c1]
   :h8 [:h7 :h9]
   :h9 [:h8 :h10 :d1]
   :h10 [:h9 :h11]
   :h11 [:h10]
   :a1 [:a2 :h3]
   :a2 [:a1]
   :b1 [:b2 :h5]
   :b2 [:b1]
   :c1 [:c2 :h7]
   :c2 [:c1]
   :d1 [:d2 :h9]
   :d2 [:d1]})


(defn parse-input [s]
  (let [[r1 r2] (->> s
                     (string/trim)
                     (inputs/lines)
                     (drop 2)
                     (drop-last))
        [_ a1 b1 c1 d1] (re-find #"([A-D])#([A-D])#([A-D])#([A-D])" r1)
        [_ a2 b2 c2 d2] (re-find #"([A-D])#([A-D])#([A-D])#([A-D])" r2)]
    {:a1 a1 :a2 a2 :b1 b1 :b2 b2
     :c1 c1 :c2 c2 :d1 d1 :d2 d2
     :energy 0
     :moves []}))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"))


; Part 1

(def hallway #{:h1 :h2 :h3 :h4 :h5 :h6 :h7 :h8 :h9 :h10 :h11})
(def hallway-endpoints (set (filter #(not (#{:h3 :h5 :h7 :h9} %)) hallway)))

(defn pod->target-rooms [state pod]
  (when pod
    (let [[r1 r2] (mapv #(keyword (str (string/lower-case pod) %)) [1 2])]
      (if (= (r2 state) pod)
        #{r1 r2}
        #{r2}))))

(defn winning-position? [state spot]
  (when (spot state)
    (let [pod (spot state)]
      ((pod->target-rooms state pod) spot))))

(defn can-move [state spot]
  (when (not (winning-position? state spot))
    (->> (spot amphipod-map)
         (filterv #(not (some? (% state))))
         (not-empty))))

(defn legal-endpoint [state spot]
  (let [amphipod (spot state)]
    (if (hallway spot)
      (into #{} (pod->target-rooms state amphipod))
      (into hallway-endpoints
            (pod->target-rooms state amphipod)))))

(defn possible-path-tree [state prev spot]
  (let [next-maps (->> (can-move state spot)
                       #_(utils/printr)
                       (filter #(not= % prev))
                       (map #(possible-path-tree state spot %)))]
    (if (seqable? next-maps)
      {spot (apply merge next-maps)}
      spot)))

(comment
  (possible-path-tree {:b1 "A" :h6 "B"} :none :b1))

(defn path-tree->paths [paths tree]
  (if tree
    (mapcat (fn [[k v]]
              (path-tree->paths (mapv #(conj % k) paths) v))
            tree)
    paths))

(defn sub-paths [path]
  (map-indexed (fn [ind _] (take (inc ind) path)) path))

(defn legal-path [state path]
  (let [[start end] [(first path) (last path)]
        endpoints (legal-endpoint state start)]
    (and (>= (count path) 2)
         (endpoints end)
         #_(not (winning-position? state start)))))

(defn possible-paths [state spot]
  (let [path-tree (possible-path-tree state :none spot)]
    (->> (path-tree->paths [[]] path-tree)
         (mapcat sub-paths)
         (filter (partial legal-path state)))))

(def energy-per-step
  {"A" 1
   "B" 10
   "C" 100
   "D" 1000})

(defn energy [pod path]
  (* (energy-per-step pod) (dec (count path))))

(defn move [state path]
  (let [[from to] [(first path) (last path)]
        pod (from state)]
    (-> state
        (assoc to (from state))
        (update :energy + (energy pod path))
        (update :moves conj path)
        (dissoc from))))


(defn spot-keys [state]
  (->> (keys state)
       (remove #{:energy :moves})))

(defn num-correct [state]
  (count (filter #(winning-position? state %) (spot-keys state))))

(defn all-moves [state]
  (let [new-states (->> (spot-keys state)
                        (filter #(and (% state) (can-move state %)))
                        (mapcat #(possible-paths state %))
                        (map #(move state %)))]
    (if-let [winning-move (some #(and (winning-position? % (last (last (:moves %)))) %)
                                new-states)]
      [winning-move]
      new-states)))

(defn won? [state]
  (every? #(= (string/upper-case (first (name %))) (% state)) 
          (filter some? (spot-keys state))))

(defn lowest-energy [states]
  (apply (partial min-key :energy) states))

(defn simulate [state]
  (loop [states [state]]
    (let [winning-states (filter won? states)
          search-width 30000
          ongoing-states (->> states
                              (filter (comp not won?))
                              (sort-by num-correct)
                              (reverse))
          searched-states (->> ongoing-states
                               (take search-width)
                               (pmap all-moves)
                               (apply concat))
          unsearched-states (->> ongoing-states
                                 (drop search-width))
          new-states (into searched-states unsearched-states)]
      (println (count new-states))
      (if (or (and (not-empty winning-states) (= winning-states states))
              (empty? states))
        winning-states
        (if (empty? winning-states)
          (recur new-states)
          (let [best-won (lowest-energy winning-states)]
            (println best-won)
            (recur (into winning-states
                         (remove #(> (:energy %) (:energy best-won))
                                 new-states)))))))))

(def simple-test-input
  {:a1 "A" :a2 nil :h1 "A" :b1 "B" :b2 "B"
   :c1 "C" :c2 "D" :d1 "D" :d2 "C"
   :energy 0
   :moves []})

(comment
  (time (lowest-energy (simulate test-input)))
  (time (lowest-energy (simulate input))))
; Answer = 15516 (424 seconds)


; Part 2

(def unfolded-amphipod-map
  (-> amphipod-map
      (assoc :a2 [:a1 :a3] :a3 [:a2 :a4] :a4 [:a3])
      (assoc :b2 [:b1 :b3] :b3 [:b2 :b4] :b4 [:b3])
      (assoc :c2 [:c1 :c3] :c3 [:c2 :c4] :c4 [:c3])
      (assoc :d2 [:d1 :d3] :d3 [:d2 :d4] :d4 [:d3])))

(defn unfold [inp]
  (-> inp
      (assoc :a4 (:a2 inp) :b4 (:b2 inp)
             :c4 (:c2 inp) :d4 (:d2 inp))
      (assoc :a2 "D" :a3 "D")
      (assoc :b2 "C" :b3 "B")
      (assoc :c2 "B" :c3 "A")
      (assoc :d2 "A" :d3 "C")))

(def unfolded-test-input 
  (unfold test-input))

(def unfolded-input
  (unfold input))

(def unfolded-simple-test-input
  {:a1 "A" :a2 "A" :a3 "A" :a4 "B" 
   :b1 "B" :b2 "B" :b3 "B" :b4 "C"
   :c1 "C" :c2 "C" :c3 "C" :c4 "A"
   :d1 "D" :d2 "D" :d3 "D" :d4 "D"
   :energy 0
   :moves []})

(defn pod->target-rooms [state pod]
  (when pod
    (let [[r1 r2 r3 r4] (mapv #(keyword (str (string/lower-case pod) %)) [1 2 3 4])]
      (cond-> #{r4}
        (= (r4 state) pod) (conj r3)
        (and (= (r4 state) pod) (= (r3 state) pod)) (conj r2)
        (and (= (r4 state) pod) (= (r3 state) pod) (= (r2 state) pod)) (conj r1)))))

(defn can-move [state spot]
  (when (not (winning-position? state spot))
    (->> (spot unfolded-amphipod-map)
         (filterv #(not (some? (% state))))
         (not-empty))))

(comment 
  (let [winners (time (simulate unfolded-simple-test-input))]
    (lowest-energy winners))
  (let [winners (time (simulate unfolded-test-input))]
    (lowest-energy winners))
  (let [winners (time (simulate unfolded-input))]
    (lowest-energy winners)))
; Answer = 45272