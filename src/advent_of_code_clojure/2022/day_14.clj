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



(def rock-map (memoize (fn [input] (set (mapcat make-continuous input)))))
(def map-bottom (memoize (fn [rock-map] (apply max (map second rock-map)))))

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
(defn out-of-bounds? [occupied [_ y]] (> y (map-bottom occupied)))

(defn resting-place [occupied starting-loc]
  (let [path (->> (sand-path occupied starting-loc)
                  (take-while #(and % (not (out-of-bounds? occupied %)))))
        end  (last path)]
    (when (resting? occupied end)
      end)))

(def sand-start [500 0])

(defn drop-grain [occupied]
  (when-let [new-grain (resting-place occupied sand-start)]
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
  (let [rm (rock-map input)
        final-state (last (take-while some? (iterate drop-grain rm)))
        final-path (->> (sand-path final-state sand-start)
                        (take-while #(and % (not (out-of-bounds? final-state %))))
                        (set))]
    (draw! rm final-state final-path)
    (count (cset/difference final-state rm))))

(comment
  (simulate test-input)
  (simulate input)

  ; Answer = 592
  )

;; Part 2


(comment

  )

