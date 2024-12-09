(ns advent-of-code-clojure.2024.day-6
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.core.reducers :as r]
            [clojure.string :as str]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (mapv vec)))

(def example
  (->> "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
       (inputs/lines)
       (mapv vec)))

;; Part 1

(defn start-coords [m]
  (some (fn [[x y]] (and (not (#{\. \#} (utils/get-coord m x y)))
                         [x y]))
        (utils/all-coords m)))

(defn next-pos [m [x y]]
  (case (utils/get-coord m x y)
    \^ (utils/north [x y])
    \< (utils/west [x y])
    \> (utils/east [x y])
    \v (utils/south [x y])))

(def turn-right
  {\^ \>
   \> \v
   \v \<
   \< \^})

(defn next-state [m [x y]]
  (let [[next-x next-y] (next-pos m [x y])
        guard (utils/get-coord m x y)]
    (cond
      (= \# (utils/get-coord m next-x next-y)) [(assoc-in m [y x] (turn-right guard))
                                                [x y]]
      (utils/out-of-bounds? m [next-x next-y]) [(assoc-in m [y x] \X)
                                                nil]
      :else [(-> m
                 (assoc-in [y x] \X)
                 (assoc-in [next-y next-x] guard))
             [next-x next-y]])))

(defn str-map [m]
  (str/join "\n" (map str/join m)))

(defn do-game [m]
  (loop [m   m
         pos (start-coords m)]
    #_(println pos)
    #_(println (str-map m) "\n")
    #_(Thread/sleep 1000)
    (let [[next-m next-pos] (next-state m pos)]
      (if next-pos
        (recur next-m next-pos)
        next-m))))

(comment
  (print (str-map example))


  (next-state example (start-coords example))
  (println (str-map (do-game input)))


  (->> (do-game input)
       (flatten)
       (filter #(= \X %))
       (count))

  )

; Answer = 4982

;; Part 2

(defn do-game-2 [m]
  (loop [m   m
         path #{}
         pos (start-coords m)]
    (let [[next-m next-pos] (next-state m pos)
          situation [pos (utils/get-coord m (first pos) (second pos))]]
      (if next-pos
        (if (get path situation)
          :looping
          (recur next-m (conj path situation) next-pos))
        next-m))))

(defn original-path [m]
  (let [finished-game (do-game m)]
    (filter (fn [[x y]] (= \X (utils/get-coord finished-game x y))) (utils/all-coords m))))

(defn find-loop-causing-placements [m]
  (->> (original-path m)
       (remove #(= (start-coords m) %))
       (filter #(= :looping (do-game-2 (assoc-in m (reverse %) \#))))
       (count)))

(comment

  (do-game input)
  (original-path example)

  (find-loop-causing-placements example)

  (time (find-loop-causing-placements input))

  ; Answer = 1663

  )
