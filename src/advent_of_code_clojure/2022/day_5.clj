(ns advent-of-code-clojure.2022.day-5
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn transpose [& xs]
  (apply map list xs))

(defn read-stacks [s]
  (->> (string/replace s #"\[|\]" " ")
       (inputs/lines)
       (apply transpose)
       (map (fn [line] (filter #(not= % \space) line)))
       (filter not-empty)
       (map (comp reverse drop-last))
       (mapv #(into '() %))))

(defn read-commands [s]
  (->> s
       (inputs/lines-and-words)
       (mapv #(-> (apply hash-map %)
                  (update-vals parse-long)
                  (update-keys keyword)))))

(def input
  (let [[stack commands] (string/split (inputs/get-input-for-this-day) #"\n\n")]
    [(read-stacks stack)
     (read-commands commands)]))

(def test-input
  (let [ti "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
        [stack commands] (string/split ti #"\n\n")]
    [(read-stacks stack)
     (read-commands commands)]))

;; Part 1

(defn perform-move [stacks {:keys [from to]}]
  (let [from-idx   (dec from)
        to-idx     (dec to)
        from-stack (nth stacks from-idx)
        to-stack   (nth stacks to-idx)]
    (assoc stacks
      from-idx (pop from-stack)
      to-idx (conj to-stack (peek from-stack)))))

(defn perform-command [stacks {:keys [move] :as command}]
  (reduce (fn [stacks command-fn] (command-fn stacks))
          stacks
          (repeat move #(perform-move % command))))

(defn simulate [[stacks commands] perform-fn]
  (->> (reduce perform-fn stacks commands)
       (map #(do (prn %) (first %)))
       (string/join)))

(comment
  (simulate test-input perform-command)

  (simulate input perform-command)
  ;; Answer = NTWZZWHFV
  )

;; Part 2

(defn perform-command-2 [stacks {:keys [move from to] :as _command}]
  (let [from-idx   (dec from)
        to-idx     (dec to)
        from-stack (nth stacks from-idx)
        to-stack   (nth stacks to-idx)]
    (assoc stacks
      from-idx (drop move from-stack)
      to-idx (concat (take move from-stack) to-stack))))

(comment
  (simulate test-input perform-command-2)

  (simulate input perform-command-2)
  ;; Answer = BRZGFVBTJ

  ) 

