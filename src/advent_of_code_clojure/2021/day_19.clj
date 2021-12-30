(ns advent-of-code-clojure.2021.day-19
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clojure.set :as cset]
            [advent-of-code-clojure.utils :as utils]))

(defn parse-beacon [s]
  (->> (string/split s #",")
       (mapv read-string)))

(defn parse-scanner [s]
  (let [s (inputs/lines s)
        scanner-id (->> (first s) (re-find #"\d+"))
        beacons (->> (rest s) (mapv parse-beacon))]
    {:id scanner-id :beacons beacons}))

(defn parse-input [s]
  (->> (string/split (string/trim s) #"\n\n")
       (mapv parse-scanner)))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

; Part 1

(defn reapply [fun ntimes start]
  (reduce (fn [res _] (fun res)) start (range ntimes)))

(defn flip [flipz point]
  (mapv (fn [p f] (if (>= f 0) p (- p))) point flipz))

(defn rotate [point [rx ry rz :as rot]]
  (letfn [(rotx [[x y z]] [x (- z) y])
          (roty [[x y z]] [(- z) y (- x)])
          (rotz [[x y z]] [y (- x) z])]
    (->> point
         (reapply rotx (Math/abs rx))
         (reapply roty (Math/abs ry))
         (reapply rotz (Math/abs rz))
         (flip rot))))

(defn rotate-list [plist rot]
  (mapv #(rotate % rot) plist))

(def all-possible-rotations
  (-> (for [x (range -4 4)
            y (range -4 4)
            z (range -4 4)]
        [x y z])
      (set)))

(defn all-pairs
  ([l]
   (all-pairs l l))
  ([la lb]
   (for [a la b lb]
     [a b])))

(defn some-matching-rotation [blista blistb]
  (some (fn [rot]
          (and (= (set blista) (set (rotate-list blistb rot)))
               rot))
        all-possible-rotations))

(defn translate [point translation]
  (mapv + point translation))

(defn find-translation [pointa pointb]
  (mapv - pointb pointa))

(defn find-all-translations [plista plistb]
  (->> (all-pairs plista plistb)
       (map (fn [[a b]] {[a b] (find-translation a b)}))
       (apply merge)))

(defn best-translation [plista plistb]
  (->> (find-all-translations plista plistb)
       (vals)
       (frequencies)
       (reduce-kv (fn [[bestk bestv] k v]
                    (if (> v bestv)
                      [k v]
                      [bestk bestv]))
                  [0 0])))

(defn best-transform-per-rotation [plista plistb]
  (->> all-possible-rotations
       (map (fn [rot] [rot (rotate-list plistb rot)]))
       (map (fn [[rot rotated-plistb]]
              (let [[trans num] (best-translation rotated-plistb plista)]
                {:num-matches num
                 :transform [rot trans]})))))

(defn most-likely-transform [scannera scannerb]
  (let [{beaconsa :beacons} scannera
        {beaconsb :beacons} scannerb
        t-maps (best-transform-per-rotation beaconsa beaconsb)
        most-matches (apply (partial max-key :num-matches) t-maps)]
    (merge scannerb most-matches)))

(comment
  (best-translation (:beacons (first input)) (:beacons (first input))))

(defn valid-to-add [universe scanners]
  (->> scanners
       (map (partial most-likely-transform {:beacons universe}))
       (filter #(>= (:num-matches %) 12))))

(comment
  (valid-to-add (:beacons (first input)) (rest input)))

(defn transform-point [point rotation translation]
  (-> point
      (rotate rotation)
      (translate translation)))

(defn transform-best [{:keys [transform beacons]}]
  (let [[rot trans] transform]
    (map #(transform-point % rot trans) beacons)))

(defn merge-universe [inp]
  (loop [universe (set (:beacons (first inp)))
         scanners (rest inp)
         found-scanners []]
    (println "Sice of universe = " (count universe))
    (println "Scanner remaining = " (count scanners))
    (if (not-empty scanners)
      (let [valids (valid-to-add universe scanners)
            new-universe (cset/union universe
                                     (set (mapcat transform-best valids)))
            new-scanners (remove #((set (map :id valids)) (:id %)) scanners)
            new-found-scanners (into found-scanners valids)]
        #_(println (map #(select-keys % [:id :num-matches]) valids))
        (recur new-universe new-scanners new-found-scanners))
      [universe found-scanners])))

(comment 
  (let [[universe found-scanners] (merge-universe input)]
    (count universe)))
; Answer = 320


; Part 2

(defn man-dist [p1 p2]
  (reduce + (map - p1 p2)))

