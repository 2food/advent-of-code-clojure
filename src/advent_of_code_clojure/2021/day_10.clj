(ns advent-of-code-clojure.2021.day-10
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.string :as string]))

(defn parse-input [s]
  (->> s
       (inputs/lines)))

(defn input []
  (-> (inputs/get-input-for-day 2021 10)
      (parse-input)))

(input)

; Part 1

(def syntax-scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn score [illegal-chars]
  (->> (frequencies illegal-chars)
       (map (fn [[k v]] (* (syntax-scores k) v)))
       (reduce +)))

(def opposite
  {\( \)
   \[ \]
   \{ \}
   \< \>})

(def openning #{\( \[ \{ \<})

(def closing #{\) \] \} \>})

(defn printr [x]
  (println x)
  x)

(defn find-corrupted [line]
  (loop [stack []
         line line]
    (let [x (first line)]
      (cond
        (empty? line) nil
        (or (empty? stack) (openning x)) (recur (cons x stack) (rest line))
        (= x (opposite (first stack))) (recur (rest stack) (rest line))
        :else x))))

(def test-input
  (parse-input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(defn find-all-corrupted [lines]
  (->> (map find-corrupted lines)
       (filterv some?)))

(comment
  (score (find-all-corrupted test-input))
  (score (find-all-corrupted (input))))
; Answer = 388713


; Part 2

(defn wo-corruption [inp]
  (->> inp
       (map find-corrupted)
       (map (fn [line corr]
              (if corr
                nil
                line))
            inp)
       (filter some?)))

(defn input-wo-corruption []
  (wo-corruption (input)))

(def test-input-wo-corruption
  (wo-corruption test-input))

(defn find-completion [line]
  (loop [stack []
         line line]
    (let [x (first line)]
      (cond
        (empty? line) (map opposite stack)
        (openning x) (recur (cons x stack) (rest line))
        (= x (opposite (first stack))) (recur (rest stack) (rest line))
        :else (throw "error")))))

(def completion-points
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn score-completion [completion]
  (reduce (fn [acc c]
            (-> (* acc 5)
                (+ (completion-points c))))
          0 
          completion))

(defn score-all-completions [lines]
  (->> (map find-completion lines)
       (map score-completion)
       (sort)))

(defn final-score [inp]
  (let [n (count inp)
        scores (score-all-completions inp)]
    (nth scores (int (/ n 2)))))

(final-score (input-wo-corruption))
; Answer = 3539961434
