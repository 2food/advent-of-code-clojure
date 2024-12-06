(ns advent-of-code-clojure.utils)

(defn reapply [fun ntimes start]
  (reduce (fn [res _] (fun res)) start (range ntimes)))

; Print
(defn printr [x]
  (println x)
  x)

(defmacro capture-env
  ([]
   `(capture-env ~@(keys &env)))
  ([& symbols]
   (cons 'do
         (map (fn [local]
                `(def ~local ~local))
              symbols))))

; Matrix stuff 

(defn all-coords [m]
  (let [xrange (range 0 (count (first m)))
        yrange (range 0 (count m))]
    (for [x xrange y yrange]
      [x y])))

(defn get-coord [m x y]
  (nth (nth m y nil) x nil))

(defn north [[x y]] [x (dec y)])
(defn north-east [[x y]] [(inc x) (dec y)])
(defn east [[x y]] [(inc x) y])
(defn south-east [[x y]] [(inc x) (inc y)])
(defn south [[x y]] [x (inc y)])
(defn south-west [[x y]] [(dec x) (inc y)])
(defn west [[x y]] [(dec x) y])
(defn north-west [[x y]] [(dec x) (dec y)])

(def cardinal-4 [north east south west])
(def cardinal-8 [north north-east east south-east south south-west west north-west])

(defn get-neighbours* [cardinal-directions m x y]
  (->> [x y]
       ((apply juxt cardinal-directions))
       (remove #(= [x y] %))
       (filter (fn [[x y]] (get-coord m x y)))
       (set)))

(defn get-neighbours-4 [m x y] (get-neighbours* cardinal-4 m x y))
(defn get-neighbours-8 [m x y] (get-neighbours* cardinal-8 m x y))

(defn transpose [m]
  (apply mapv vector m))

(defn something [pred coll] (some #(and (pred %) %) coll))

(defn out-of-bounds? [m [x y]]
  (or (not (< -1 x (count (first m))))
      (not (< -1 y (count m)))))
