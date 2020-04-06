(ns advent-of-code-clojure.2018.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [clojure.math.numeric-tower :as math]))

(defn markings [] (sort (inputs/lines (inputs/get-input-for-day 2018 4))))

(comment
  (map println (markings)))

(defn group-shifts
  [markings]
  (loop [shifts []
         remaining-markings markings]
    (if (empty? remaining-markings)
      shifts
      (let [marking (first remaining-markings)]
        (if (re-find #"Guard" marking)
          (recur (conj shifts [marking])
                 (rest remaining-markings))
          (recur (update shifts
                         (dec (count shifts))
                         #(conj % marking))
                 (rest remaining-markings)))))))

(comment
  (group-shifts (markings)))

(def example-shift-strs ["[1518-11-13 00:00] Guard #1877 begins shift" "[1518-11-13 00:10] falls asleep" "[1518-11-13 00:43] wakes up"])
(def example-event-str (first example-shift-strs))

(defn str->timestamp
  [s]
  (f/parse (f/formatter "[YYYY-MM-DD HH:mm]") s))

(defn str->action
  [s]
  (cond
    (re-find #"begins" s) {:guard  (Long/parseLong (last (re-matches #".*#(\d+) .*" s)))
                           :action :starts}
    (re-find #"wakes" s) {:action :wakes}
    (re-find #"falls asleep" s) {:action :falls-asleep}))

(defn event-str->event
  [event-string]
  (let [[date time & rest] (string/split event-string #" ")
        timestamp (str->timestamp (str date " " time))
        action (str->action (string/join " " rest))]
    (merge {:timestamp timestamp}
           action)))

(comment
  (event-str->event example-event-str))

(defn shifts-str-groups->shifts
  [shift-str-groups]
  (->> shift-str-groups
       (map #(map event-str->event %))
       (group-by (comp :guard first))))

(comment
  (first (get (-> (markings)
                  group-shifts
                  shifts-str-groups->shifts)
              3251)))

(defn merge-min-vecs [min-vecs]
  (mapv #(reduce + %) (partition (count min-vecs) (apply interleave min-vecs))))

(defn min-vec' [r prev-min [f & rest]]
  (if (some? f)
    (let [now (t/minute (:timestamp f))]
      (into (vec (repeat (- now prev-min) r))
            (min-vec' (math/abs (- r 1)) now rest)))
    []))

(defn pad-to-60 [v]
  (into (vec v) (repeat (- 60 (count v)) 0)))

(defn min-vec [v]
  (pad-to-60 (min-vec' 0 0 (rest v))))

(defn shifts->min-vectors [shiftmap]
  (reduce-kv (fn [acc k v]
               (into acc {k (merge-min-vecs (map min-vec v))}))
             {}
             shiftmap))

(defn guard-sleep-times []
  (-> (markings)
      group-shifts
      shifts-str-groups->shifts
      shifts->min-vectors))

(defn most-asleep []
  (reduce-kv (fn [[oldk oldv] k v]
               (let [newv (reduce + v)]
                 (if (< oldv newv)
                   [k newv]
                   [oldk oldv])))
             [0 0]
             (guard-sleep-times)))

(def correct-1 151754)
(defn answer-1 []
  (let [[guard _] (most-asleep)
        sleep-mins ((guard-sleep-times) guard)]
    (* guard (.indexOf sleep-mins (apply max sleep-mins)))))

(defn highest-consistent-sleep []
  (reduce-kv (fn [[oldk oldv] newk newv]
               (let [old (apply max oldv)
                     new (apply max newv)]
                 (if (< old new)
                   [newk newv]
                   [oldk oldv])))
             [0 [0]]
             (guard-sleep-times)))

; wrong 12435, too low
(def correct-2 19896)
(defn answer-2 []
  (let [[guard sleep-mins] (highest-consistent-sleep)]
    (* guard (.indexOf sleep-mins (apply max sleep-mins)))))