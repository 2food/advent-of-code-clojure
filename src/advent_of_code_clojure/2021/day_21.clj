(ns advent-of-code-clojure.2021.day-21
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [advent-of-code-clojure.utils :as utils]))

(defn get-starting-num [s]
  (read-string (str (last (re-find #"position: (\d+)" s)))))

(defn parse-input [s]
  (let [[p1 p2] (map get-starting-num (string/split-lines (string/trim s)))]
    {:p1 {:pos p1 :score 0}
     :p2 {:pos p2 :score 0}}))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-input
  (parse-input
   "Player 1 starting position: 4
Player 2 starting position: 8"))


; Part 1

(defn get-deterministic-die []
  (let [die (atom {:times-rolled 0
                   :face 0
                   :roll-fn (fn [d]
                              (if (= 100 (:face @d))
                                (swap! d #(assoc % :face 1))
                                (swap! d #(update % :face inc)))
                              (swap! d #(update % :times-rolled inc))
                              (:face @d))})]
    die))

(defn roll [die]
  (let [roll-fn (:roll-fn @die)]
    (roll-fn die)))

(defn times-rolled [die]
  (:times-rolled @die))

#_(def die (get-deterministic-die))

(defn sum-three-rolls [die]
  (reduce + (for [_ (range 3)] (roll die))))

(defn circular-board []
  (flatten (repeat (range 1 11))))

(defn add-pos [{:keys [pos] :as player} rolls]
  (let [pos (last (take (+ pos rolls) (circular-board)))]
    (assoc player :pos pos)))

(defn update-pos-and-score [player rolls]
  (let [player (add-pos player rolls)]
    (update player :score #(+ % (:pos player)))))

(defn turn [players die]
  (let [{:keys [p1 p2]} players
        p1-rolls (sum-three-rolls die)
        new-p1  (update-pos-and-score p1 p1-rolls)]
    (if (>= (:score new-p1) 1000)
      {:p1 new-p1
       :p2 p2}
      (let [p2-rolls (sum-three-rolls die)
            new-p2 (update-pos-and-score p2 p2-rolls)]
        {:p1 new-p1
         :p2 new-p2}))))

(defn any-winner? [{:keys [p1 p2]}]
  (or (and (>= (:score p1) 1000) p1)
      (and (>= (:score p2) 1000) p2)))

(defn perform-game [players die]
  (loop [players players]
    (if (any-winner? players)
      players
      (recur (turn players die)))))

(comment
  (let [die (get-deterministic-die)
        player-state (perform-game test-input die)]
    (println player-state)
    (println (times-rolled die))))

(comment
  (let [die (get-deterministic-die)
        player-state (perform-game input die)]
    (println player-state)
    (println (times-rolled die)))
  (* 766 924))
; Answer = 707784


; Part 2

