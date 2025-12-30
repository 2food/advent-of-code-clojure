(ns advent-of-code-clojure.utils-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-clojure.utils :refer [distance]]))

(deftest distance-test
  (is (== 5 (distance [1 1] [4 5]))))
