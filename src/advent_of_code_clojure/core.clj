(ns advent-of-code-clojure.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:import (java.time Year)))

(defn new-year!
  ([] (new-year! (.getValue (Year/now))))
  ([year]
   (io/make-parents (str "resources/inputs/" year "/whatever"))
   (doseq [d (range 1 13)]
     (let [f (str "src/advent_of_code_clojure/" year "/day_" d ".clj")]
       (io/make-parents f)
       (spit f
             (str "(ns advent-of-code-clojure." year ".day-" d "
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       ))

;; Part 1


(comment \n  \n  )

;; Part 2


(comment \n  \n  )

"))))))

(comment
  (new-year!)
  )

