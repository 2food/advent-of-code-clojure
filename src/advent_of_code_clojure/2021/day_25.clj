(ns advent-of-code-clojure.2021.day-25
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (let [lines (->> s
                   (string/trim)
                   (inputs/lines))
        y (count lines)
        x (count (first lines))
        sc-map (->> lines
                    (map-indexed (fn [y row]
                                   (map-indexed (fn [x e] [y x e]) row)))
                    (apply concat)
                    (reduce (fn [acc [y x e]]
                              (cond-> acc
                                (= e \v) (assoc [y x] :v)
                                (= e \>) (assoc [y x] :>)))
                            {}))]
    (merge sc-map {:size {:x x :y y}})))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"))


; Part 1

(defn filter-hash-map [filterfn m]
  (apply hash-map (apply concat (filter filterfn m))))

(def east-scs (partial filter-hash-map (fn [[_ v]] (= v :>))))
(def south-scs (partial filter-hash-map (fn [[_ v]] (= v :v))))

(defn east-of [size [y x]]
  (if (= (inc x) (:x size)) [y 0] [y (inc x)]))

(defn south-of [size [y x]]
  (if (= (inc y) (:y size)) [0 x] [(inc y) x]))

(defn can-move [{:keys [size] :as sc-map} pos dir-of]
  (let [new-pos (dir-of size pos)]
    (when-not (sc-map new-pos)
      new-pos)))

(defn can-move-east [sc-map pos] (can-move sc-map pos east-of))
(defn can-move-south [sc-map pos] (can-move sc-map pos south-of))

(defn dir-step [sc-map dir-scs can-move-dir]
  (let [dir-facing (dir-scs sc-map)]
    (reduce (fn [acc [pos e]]
              (if-let [new-pos (can-move-dir sc-map pos)]
                (-> acc (dissoc pos) (assoc new-pos e))
                acc))
            sc-map
            dir-facing)))

(defn east-step [sc-map] (dir-step sc-map east-scs can-move-east))
(defn south-step [sc-map] (dir-step sc-map south-scs can-move-south))

(defn step [sc-map]
  (-> sc-map
      (east-step)
      (south-step)))

(defn printable [{{ysize :y xsize :x} :size :as sc-map}]
  (string/join
   "\n"
   (for [y (range ysize)]
     (string/join
      (for [x (range xsize)]
        (case (sc-map [y x])
          :v "v"
          :> ">"
          "."))))))

(comment
  (println (printable test-input))
  (println (printable (step test-input))))

(defn find-stop [init-sc-map]
  (loop [n 1
         sc-map init-sc-map]
    (let [new-sc-map (step sc-map)]
      (if (= new-sc-map sc-map)
        n
        (recur (inc n) new-sc-map)))))

(comment 
  (find-stop test-input)
  (find-stop input))
; Answer = 601
