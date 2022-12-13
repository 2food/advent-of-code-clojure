(ns advent-of-code-clojure.2022.day-11
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.core.match :refer [match]]
            [clojure.string :as string]))

(defn parse-monkey [s]
  (let [[_ item-line op-line test-line then-line else-line] (inputs/lines-and-words s)
        items  (read-string (str "[" (string/join " " (drop 4 item-line)) "]"))
        op     (mapv read-string (take-last 2 op-line))
        test-n (read-string (last test-line))
        then-n (read-string (last then-line))
        else-n (read-string (last else-line))]
    {:items            items
     :operation        op
     :inspection-count 0
     :test             test-n
     :then             then-n
     :else             else-n}))

(def input
  (->> (string/split (inputs/get-input-for-this-day) #"\n\n")
       (mapv parse-monkey)))

(def test-input
  (->> (string/split "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1" #"\n\n")
       (mapv parse-monkey)))

;; Part 1

(def primes [2 3 5 7 11 13 17 19])
(def cycle-length (apply * primes))

(defn do-op [operation n]
  (match operation
    ['+ 'old] (+ n n)
    ['* 'old] (* n n)
    ['+ m] (+ n m)
    ['* m] (* n m)))

(def ^:dynamic worry-divisor 3)

(defn take-turn [state this-monkey-n]
  (let [{:keys [items operation test then else]} (get state this-monkey-n)]
    (-> (reduce (fn [st i]
                  (let [new-val      (mod (long (/ (do-op operation i) worry-divisor))
                                          cycle-length)
                        new-monkey-n (if (= (mod new-val test) 0)
                                       then else)]
                    #_(prn "item" new-val "from" this-monkey-n "to" new-monkey-n)
                    (-> st
                        (update-in [this-monkey-n :inspection-count] inc)
                        (update-in [new-monkey-n :items] conj new-val))))
                state
                items)
        (assoc-in [this-monkey-n :items] []))))

(defn do-round [state]
  (let [monkey-nums (range (count state))]
    (reduce take-turn state monkey-nums)))

(defn monkey-business [state rounds]
  (->> (reduce (fn [st _] (do-round st)) state (range rounds))
       (map :inspection-count)
       (sort)
       (take-last 2)
       (apply *)))

(comment
  (reduce (fn [st _] (do-round st)) test-input (range 1))
  (monkey-business test-input 1)
  (monkey-business input 20)
  ; Answer = 66802
  )

;; Part 2

(comment
  (binding [worry-divisor 1]
    (monkey-business input 10000))
  ; Answer = 21800916620
  ) 

