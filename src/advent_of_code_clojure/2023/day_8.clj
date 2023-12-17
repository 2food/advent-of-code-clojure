(ns advent-of-code-clojure.2023.day-8
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as str]))

(defn parse-node [node-string]
  (let [[_ node left right] (re-find #"(...) = \((...), (...)\)" node-string)]
    {node [left right]}))

(defn parse-input [input]
  (let [[instructions _ & nodes] (inputs/lines input)]
    {:instructions instructions
     :nodes        (->> nodes
                        (map parse-node)
                        (apply merge))}))

(def input
  (->> (inputs/get-input-for-this-day)
       (parse-input)))

(def example1
  (->> "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"
       (parse-input)))

(def example2
  (->> "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
       (parse-input)))

;; Part 1

(defn count-steps [{:keys [instructions nodes]}]
  (loop [current      "AAA"
         steps        0
         instructions (cycle instructions)]
    (if (= "ZZZ" current)
      steps
      (case (first instructions)
        \L (recur (first (nodes current)) (inc steps) (rest instructions))
        \R (recur (second (nodes current)) (inc steps) (rest instructions))))))

(comment

  (count-steps example1)
  (count-steps example2)
  (count-steps input)

  )

;; Part 2

(def example3 (->> "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"
                   (parse-input)))

(defn perform-step [nodes instruction current]
  (case instruction
    \L (first (nodes current))
    \R (second (nodes current))))

(defn find-path [{:keys [nodes instructions]} starting-node]
  (let [inst-size (count instructions)]
    (iterate (fn [{:keys [node steps]}]
               {:node  (perform-step nodes (nth instructions (mod steps inst-size)) node)
                :steps (inc steps)})
             {:node starting-node :steps 0})))

(defn find-cycles [input]
  (let [starting (filter #(str/ends-with? % "A") (keys (:nodes input)))]
    (->> (map #(find-path input %) starting)
         (map (fn [path] (filter (comp #(str/ends-with? % "Z") :node) path)))
         (map (fn [path] {:step (- (:steps (second path))
                                   (:steps (first path)))})))))

(comment


  (apply * (map :step (find-cycles example3)))

  (loop [steps (map #(assoc % :count 1) (find-cycles example3))]
    (let [multiplied-steps (map #(* (:step %) (:count %)) steps)]
      (if (apply = multiplied-steps)
        (first multiplied-steps)
        (recur (map #(if (< (* (:step %) (:count %)) (apply max multiplied-steps))
                      (update % :count inc)
                      %) steps)))))

  (time
   (loop [steps (map #(assoc % :count 1) (find-cycles input))]
     (let [multiplied-steps (map #(* (:step %) (:count %)) steps)]
       (if (apply = multiplied-steps)
         (first multiplied-steps)
         (recur (map #(if (< (* (:step %) (:count %)) (apply max multiplied-steps))
                        (update % :count inc)
                        %) steps))))))

  ; Answer = 21165830176709
  ; Time = 1.014 hours

  )

