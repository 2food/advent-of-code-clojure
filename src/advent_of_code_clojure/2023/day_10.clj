(ns advent-of-code-clojure.2023.day-10
  (:require [advent-of-code-clojure.inputs :as inputs]
            [advent-of-code-clojure.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)))

(def example1
  (->> "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
       (inputs/lines)))

(def example2
  (->> "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ"
       (inputs/lines)))

;; Part 1

(defn starting-coords [pipe-map]
  (utils/something (fn [[x y]] (= \S (utils/get-coord pipe-map x y))) (utils/all-coords pipe-map)))

(defn connected-pipes [pipe-map [x y]]
  (let [get-coord       (fn [[x y]] (utils/get-coord pipe-map x y))
        conn-north?     (#{\| \7 \F} (get-coord (utils/north [x y])))
        conn-east?      (#{\- \7 \J} (get-coord (utils/east [x y])))
        conn-south?     (#{\| \J \L} (get-coord (utils/south [x y])))
        conn-west?      (#{\- \L \F} (get-coord (utils/west [x y])))
        connected-pipes (fn [cardinals]
                          (->> cardinals
                               (keep (fn [dir]
                                       (case dir
                                         :north (when conn-north? (utils/north [x y]))
                                         :east (when conn-east? (utils/east [x y]))
                                         :south (when conn-south? (utils/south [x y]))
                                         :west (when conn-west? (utils/west [x y])))))
                               (set)))]
    (case (get-coord [x y])
      \S (connected-pipes [:north :east :south :west])
      \- (connected-pipes [:east :west])
      \| (connected-pipes [:north :south])
      \L (connected-pipes [:north :east])
      \F (connected-pipes [:east :south])
      \7 (connected-pipes [:south :west])
      \J (connected-pipes [:north :west]))))

(defn find-all-connected [pipe-map start]
  (loop [visited   []
         unvisited [start]]
    (if (empty? unvisited)
      visited
      (let [current   (first unvisited)
            new-pipes (connected-pipes pipe-map current)
            unknown   (set/difference (set new-pipes) (set visited) (set unvisited))]
        (recur (conj visited current) (concat unknown (rest unvisited)))))))

(comment
  (find-all-connected example1 (starting-coords example1))
  (/ (count (find-all-connected example1 (starting-coords example1))) 2)

  (/ (count (find-all-connected example2 (starting-coords example2))) 2)

  (time (/ (count (find-all-connected input (starting-coords input))) 2))

  ; Answer = 7107
  ; Time = 8.8 seconds

  )

;; Part 2

(def example3
  (->> "...........\n.S-------7.\n.|F-----7|.\n.||OOOOO||.\n.||OOOOO||.\n.|L-7OF-J|.\n.|II|O|II|.\n.L--JOL--J.\n.....O....."
       (inputs/lines)))

(def example4
  (->> "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"
       (inputs/lines)))

(defn turn [c side]
  (cond
    (#{\- \| \S} c) side
    (#{\L \7} c) (let [dirs {:north :east :south :west}]
                   (or (dirs side) ((set/map-invert dirs) side)))
    (#{\F \J} c) (let [dirs {:north :west :south :east}]
                   (or (dirs side) ((set/map-invert dirs) side)))))

(defn touching [coord side]
  (case side
    :north (utils/north coord)
    :east (utils/east coord)
    :south (utils/south coord)
    :west (utils/west coord)))

(defn run-path [pipe-map whole-path starting-side]
  (loop [path whole-path
         touched #{}
         side starting-side]
    (if (empty? path)
      touched
      (let [[head & tail] path]
        (recur tail
               (conj touched (touching head side))
               (turn (apply utils/get-coord pipe-map head) side))))))

(defn fill [pipe-map path touched]
  (let [path-set (set path)]
    (loop [visited   #{}
           unvisited (set touched)]
      (if (empty? unvisited)
        visited
        (let [current (first unvisited)
              new-coords (->> (apply utils/get-neighbours-4 pipe-map current)
                              (remove path-set)
                              (remove visited)
                              (set))]
          (recur (conj visited current) (-> unvisited
                                            (disj current)
                                            (into new-coords))))))))

(comment
  (turn \7 :east)

  (let [input example3
        path (find-all-connected input (starting-coords input))]
    (->> (run-path input path :south)
         (remove (set path))
         (fill input path)
         (count)))


  (let [input example4
        path (find-all-connected input (starting-coords input))]
    (->> (run-path input path :south)
         (remove (set path))
         (fill input path)
         (count)))

  (time
   (let [path (find-all-connected input (starting-coords input))]
     (->> (run-path input path :south)
          (remove (set path))
          (fill input path)
          (count))))

  ; Too low = 275

  )

