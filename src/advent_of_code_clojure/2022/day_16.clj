(ns advent-of-code-clojure.2022.day-16
  (:require [advent-of-code-clojure.inputs :as inputs]))

(defn parse-line [[_ valve _ _ rate _ _ _ _ & neighbours]]
  {valve
   {:rate       (parse-long (re-find #"\d+" rate))
    :neighbours (set (map #(re-find #"\w+" %) neighbours))}})

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)
       (map parse-line)
       (apply merge)))

(def test-input
  (->> "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
       (inputs/lines-and-words)
       (map parse-line)
       (apply merge)))

;; Part 1

(defn init-distances [input]
  (->> (map (fn [[valve {:keys [neighbours]}]]
              {valve (->> (map hash-map neighbours (repeat 1))
                          (apply merge))})
            input)
       (apply merge)))

(defn dijkstra [input start]
  (let [init-dists (init-distances input)]
    (loop [unvisited (set (keys input))
           distances (-> (get init-dists start)
                         (assoc start 0))]
      (if (empty? unvisited)
        distances
        (let [dist-fn           #(get distances % ##Inf)
              current           (apply min-key dist-fn unvisited)
              dist-to-neighbour (inc (dist-fn current))
              neighs            (keys (get init-dists current))
              new-distances     (reduce (fn [dists n]
                                          (if (< dist-to-neighbour (dist-fn n))
                                            (assoc dists n dist-to-neighbour)
                                            dists))
                                        distances neighs)
              new-unvisited     (set (remove #{current} unvisited))]
          (recur new-unvisited new-distances))))))

(defn all-distances [input]
  (->> input
       (map (fn [[valve _]] [valve (dijkstra input valve)]))
       (into {})))

(defn pressure-value [input distances time-left current-valve target-valve]
  (let [time-left-after (- time-left (+ 1 (get-in distances [current-valve target-valve])))]
    {:valve         target-valve
     :pressure      (* (get-in input [target-valve :rate])
                       time-left-after)
     :time-left     time-left-after}))

(defn sum-pressures [{:keys [pressure then]}]
  (if then
    (+ pressure (sum-pressures then))
    pressure))

(defn best-choice [input distances time-left current-valve]
  (let [choices (->> (keys input)
                     (map (fn [valve] (pressure-value input distances time-left current-valve valve)))
                     (filter #(and (< 0 (:time-left %)) (< 0 (:pressure %))))
                     (map (fn [{:keys [valve time-left] :as this}]
                            (assoc this
                              :then (best-choice (dissoc input valve) distances time-left valve)))))]
    (when (not-empty choices)
      (apply max-key sum-pressures choices))))

(defn choice-order [{:keys [valve then]}]
  (if then
    (cons valve (choice-order then))
    [valve]))

(comment
  (dijkstra test-input "AA")
  (choice-order (best-choice test-input (all-distances test-input) 30 "AA"))
  (time (let [choice (best-choice input (all-distances input) 30 "AA")]
          [(sum-pressures choice)
           (choice-order choice)]))

  ; Answer = 2265
  )

;; Part 2


(comment

  ) 

