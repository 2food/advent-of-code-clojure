(ns advent-of-code-clojure.2018.day-4
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]
            [clj-time.format :as f]))

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
  (map println (group-shifts (markings))))

(def example-shift-strs ["[1518-11-13 00:00] Guard #1877 begins shift" "[1518-11-13 00:10] falls asleep" "[1518-11-13 00:43] wakes up"])
(def example-event-str (first example-shift-strs))

(defn str->timestamp
  [s]
  (f/parse (f/formatter "[YYYY-MM-DD HH:mm]") s))

(defn str->action
  [s]
  (cond
    (re-find #"begins" s) {:guard (Long/parseLong (last (re-matches #".*#(\d+) .*" s)))
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

