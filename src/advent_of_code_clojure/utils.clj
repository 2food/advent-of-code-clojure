(ns advent-of-code-clojure.utils)

(defn reapply [fun ntimes start]
  (reduce (fn [res _] (fun res)) start (range ntimes)))

; Print
(defn printr [x]
  (println x)
  x)


; Matrix stuff 

(defn all-coords [m]
  (let [xrange (range 0 (count (first m)))
        yrange (range 0 (count m))]
    (for [x xrange y yrange]
      [x y])))

(defn get-coord [m x y]
  (nth (nth m y) x))


(defn get-neighbours-4 [m x y]
  (let [maxx (dec (count (first m)))
        maxy (dec (count m))
        all-neighbours (->> [[x (max 0 (dec y))] [(max 0 (dec x)) y]
                             [x (min maxy (inc y))] [(min maxx (inc x)) y]]
                            (remove #(= [x y] %)))]
    (set all-neighbours)))

(defn get-neighbours-8 [m x y]
  (let [xrange (range (max 0 (dec x)) 
                      (min (count (first m)) (+ 2 x)))
        yrange (range (max 0 (dec y)) 
                      (min (count m) (+ 2 y)))
        all-neighbours (->> (for [x xrange
                                  y yrange]
                              [x y])
                            (remove #(= [x y] %)))]
    (set all-neighbours)))

(defn transpose [m]
  (apply mapv vector m))