(ns advent-of-code-clojure.2022.day-14
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.set :as cset]
            [clojure.string :as string]))

(defn parse-line [line]
  (->> (string/split line #" -> ")
       (map #(read-string (str "[" % "]")))))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines)
       (map parse-line)))

(def test-input
  (->> "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
       (inputs/lines)
       (map parse-line)))

;; Part 1

(defn make-continuous [rocks]
  (reduce (fn [acc [rx ry]]
            (let [[px py] (last acc)]
              (conj (into acc
                          (for [x (range (min rx px) (inc (max rx px)))
                                y (range (min ry py) (inc (max ry py)))]
                            [x y]))
                    [rx ry])))
          [(first rocks)]
          (rest rocks)))



(defn rock-map [input] (set (mapcat make-continuous input)))
(defn map-bottom [rock-map] (apply max (map second rock-map)))

(defn fall-down [[x y]] [x (inc y)])
(defn fall-left [[x y]] [(dec x) (inc y)])
(defn fall-right [[x y]] [(inc x) (inc y)])

(defn step [occupied sand-loc]
  (when sand-loc
    (some #(and (not (occupied %)) %)
          ((juxt fall-down fall-left fall-right) sand-loc))))

(defn sand-path [occupied sand-loc]
  (iterate (partial step occupied) sand-loc))

(defn resting? [occupied sand-loc] (and sand-loc (nil? (step occupied sand-loc))))

(defn resting-place [out-of-bounds? occupied starting-loc]
  (let [path (->> (sand-path occupied starting-loc)
                  (take-while #(and % (not (out-of-bounds? %)))))
        end  (last path)]
    (when (resting? occupied end)
      end)))

(def sand-start [500 0])

(defn drop-grain [out-of-bounds? occupied]
  (when-let [new-grain (resting-place out-of-bounds? occupied sand-start)]
    (conj occupied new-grain)))

(defn draw!
  ([rm with-sand] (draw! rm with-sand #{}))
  ([rm with-sand path]
   (let [min-width  (- (apply min (map first rm)) 1)
         max-width  (+ (apply max (map first rm)) 2)
         max-height (+ (apply max (map second rm)) 2)]
     (println
      (string/join "\n"
                   (->> (for [y (range 0 max-height)
                              x (range min-width max-width)]
                          (cond
                            (and (with-sand [x y]) (not (rm [x y]))) \o
                            (rm [x y]) \#
                            (path [x y]) \~
                            :else \.))
                        (partition (- max-width min-width))
                        (map string/join)))))))

(defn simulate [input]
  (let [rm             (rock-map input)
        bottom         (map-bottom rm)
        out-of-bounds? (fn [[_ y]] (> y bottom))
        final-state    (last (take-while some? (iterate (partial drop-grain out-of-bounds?) rm)))
        final-path     (->> (sand-path final-state sand-start)
                            (take-while #(and % (not (out-of-bounds? %))))
                            (set))]
    (draw! rm final-state final-path)
    (count (cset/difference final-state rm))))

(comment
  (simulate test-input)
  (simulate input)

  ; Answer = 592
  )

;; Part 2

(defn floor [rm]
  (let [floor-y (+ 2 (map-bottom rm))
        [x-from x-to] ((juxt - +) (first sand-start) (int (* 1.2 floor-y)))]
    (set (for [x (range x-from (inc x-to))]
           [x floor-y]))))

(defn simulate-2 [input]
  (let [rm             (rock-map input)
        rm-with-floor  (into rm (floor rm))
        bottom         (map-bottom rm-with-floor)
        out-of-bounds? (fn [[_ y]] (> y bottom))
        final-state    (last (take-while #(not (% sand-start))
                                         (iterate (partial drop-grain out-of-bounds?) rm-with-floor)))]
    (draw! rm-with-floor final-state)
    (inc (count (cset/difference final-state rm-with-floor)))))


(comment
  (time (simulate-2 test-input))
  (time (simulate-2 input))
  ; Answer = 30367
  )

