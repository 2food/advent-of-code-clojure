(ns advent-of-code-clojure.2021.day-16
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(def hex-to-dec
  {\A 10
   \B 11
   \C 12
   \D 13
   \E 14
   \F 15})

(defn pad-to-4 [s]
  (str (string/join (repeat (- 4 (count s)) "0")) s))

(defn to-bin [c]
  (-> (if (number? (read-string (str c)))
        (read-string (str c))
        (hex-to-dec c))
      (Long/toBinaryString)
      (pad-to-4)))

(defn parse-input [s]
  (->> s
       (string/trim)
       (mapcat to-bin)
       (string/join)))

(def input
  (parse-input
   (inputs/get-input-for-this-day)))

(def test-literal (parse-input "D2FE28"))
(def test-operator-1 (parse-input "8A004A801A8002F478"))
(def test-operator-2 (parse-input "620080001611562C8802118E34"))
(def test-operator-3 (parse-input "C0015000016115A2E0802F182340"))
(def test-operator-4 (parse-input "A0016C880162017C3686B18A3D4780"))


(comment test-literal)

; Part 1

(defn split-string-at [n s]
  (mapv string/join (split-at n s)))

(defn read-bin [s]
  (when (not-empty s)
    (read-string (str "2r" s))))


(defn de-group-val [s]
  (let [[group s] (split-string-at 5 s)]
    (if (= (first group) \0)
      [(string/join (rest group)) s]
      (let [[groups s] (de-group-val s)]
        [(str (string/join (rest group)) groups) s]))))

(defn parse-lit-val [s]
  (let [[val rest] (de-group-val s)]
    {:val (read-bin val)
     :rest rest}))

(defn parse [s]
  (let [[version s] (split-string-at 3 s)
        version (read-bin version)
        [type s] (split-string-at 3 s)
        type (read-bin type)
        subloop (fn [s finished?]
                  (loop [packets []
                         rests s]
                    (if (finished? packets rests)
                      {:val packets :rest rests}
                      (let [packet (parse rests)
                            new-rest (if (seq? (:val packet))
                                       (:rest (last (:val packet)))
                                       (:rest packet))]
                        (recur (conj packets packet) new-rest)))))]
    (if (= type 4)
      (merge {:version version
              :type type}
             (parse-lit-val s))
      (merge {:version version
              :type type}
             (let [[lbit s] (split-string-at 1 s)]
               (if (= lbit "1")
                 (let [[nsubs s] (split-string-at 11 s)
                       nsubs (read-bin nsubs)]
                   (subloop s (fn [packets _] 
                                (= (count packets) nsubs))))
                 (let [[bitlength s] (split-string-at 15 s)
                       bitlength (read-bin bitlength)]
                   (subloop s (fn [_ rests] 
                                (<= bitlength (- (count s) (count rests))))))))))))

(defn version-sum [parse-tree]
  (if (seqable? (:val parse-tree))
    (apply (partial + (:version parse-tree))
           (map version-sum (:val parse-tree)))
    (:version parse-tree)))

(comment
  (version-sum (parse test-literal))
  (version-sum (parse test-operator-1))
  (version-sum (parse test-operator-2))
  (version-sum (parse test-operator-3))
  (version-sum (parse test-operator-4))
  (version-sum (parse input)))
; Answer = 953


; Part 2

(def bool-to-int
  {true 1
   false 0})

(defn compute [parse-tree]
  (case (:type parse-tree)
    0 (->> (:val parse-tree)
           (map compute)
           (reduce +))
    1 (->> (:val parse-tree)
           (map compute)
           (reduce *))
    2 (->> (:val parse-tree)
           (map compute)
           (apply min))
    3 (->> (:val parse-tree)
           (map compute)
           (apply max))
    4 (:val parse-tree)
    5 (->> (:val parse-tree)
           (map compute)
           (apply >)
           (bool-to-int))
    6 (->> (:val parse-tree)
           (map compute)
           (apply <)
           (bool-to-int))
    7 (->> (:val parse-tree)
           (map compute)
           (apply =)
           (bool-to-int))))

(comment
  (compute (parse (parse-input "C200B40A82")))
  (compute (parse (parse-input "04005AC33890")))
  (compute (parse (parse-input "880086C3E88112")))
  (compute (parse (parse-input "CE00C43D881120")))
  (compute (parse (parse-input "D8005AC2A8F0")))
  (compute (parse (parse-input "F600BC2D8F")))
  (compute (parse (parse-input "9C005AC2F8F0")))
  (compute (parse (parse-input "9C0141080250320F1802104A08")))
  (compute (parse input)))
; Answer = 246225449979