(ns advent-of-code-clojure.2025.day-7
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.string :as str]))

(defn parse-input [s]
  (->> (inputs/lines s)
       (mapv vec)))

(def example-input
  (->> ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."
       (parse-input)))

(def input
  (->> (inputs/get-input-for-this-day)
       (parse-input)))

(defn printable [m]
  (str/join "\n" (map str/join m)))

;; Part 1

(defn all-coords [m]
  (let [xlen (count (first m))
        ylen (count m)]
    (for [y (range 0 ylen)
          x (range 0 xlen)]
      [x y])))

(defn step [m [x y]]
  (let [c (utils/get-coord m x y)]
    (if (and (= \. c)
             (or (#{\S \|} (utils/get-coord m x (dec y)))
                 (and (= \^ (utils/get-coord m (dec x) y))
                      (= \| (utils/get-coord m (dec x) (dec y))))
                 (and (= \^ (utils/get-coord m (inc x) y))
                      (= \| (utils/get-coord m (inc x) (dec y))))))
      (assoc-in m [y x] \|)
      m)))

(defn pour [m]
  (reduce step m (all-coords m)))

(comment
  (print (printable (pour example-input)))

  (let [poured (pour example-input)]
    (->> (all-coords poured)
         (filter (fn [[x y]]
                   (and (= \^ (utils/get-coord poured x y))
                        (= \| (utils/get-coord poured x (dec y))))))
         (count)))

  (print (printable (pour input)))

  (let [poured (pour input)]
    (->> (all-coords poured)
         (filter (fn [[x y]]
                   (and (= \^ (utils/get-coord poured x y))
                        (= \| (utils/get-coord poured x (dec y))))))
         (count)))

  )

;; Part 2

(defn step2 [m [[x y] num]]
  (if (= \^ (utils/get-coord m x (inc y)))
    {[(dec x) (inc y)] num
     [(inc x) (inc y)] num}
    {[x (inc y)] num}))

(defn find-paths [m]
  (let [start-loc [(first (filter #(= \S (nth (first m) %)) (range 0 (count (first m)))))
                   0]]
    (loop [path-head-freqs {start-loc 1}]
      (if (= (count m) (second (ffirst path-head-freqs)))
        path-head-freqs
        (recur (->> (map (partial step2 m) path-head-freqs)
                    (apply merge-with +)))))))

(comment

  (step2 example-input [[7 0] 1])
  (->> (find-paths example-input)
       (vals)
       (reduce +))

  (->> (find-paths input)
       (vals)
       (reduce +))


  )

