(ns advent-of-code-clojure.2022.day-7
  (:require [advent-of-code-clojure.inputs :as inputs]
            [clojure.walk :as walk]
            [clojure.core.match :refer [match]]))

(def input
  (->> (inputs/get-input-for-this-day)
       (inputs/lines-and-words)))

(def test-input
  (->> "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
       (inputs/lines-and-words)))

;; Part 1


(defn build-dir-tree [input]
  (letfn [(update-node
            ([tree path] (update-node tree path {}))
            ([tree path content]
             (update-in tree path merge content)))
          (add-to-path [path x]
            (case x
              "/" ["/"]
              ".." (vec (drop-last path))
              (into path [x])))]
    (loop [lines    input
           dir-tree {}
           cwd      []]
      (if-let [line (first lines)]
        (match line
          ["$" "cd" dir] (let [path (add-to-path cwd dir)]
                           (recur (rest lines) (update-node dir-tree path) path))
          ["$" "ls"] (recur (rest lines) dir-tree cwd)
          ["dir" dir] (recur (rest lines) (update-node dir-tree (add-to-path cwd dir)) cwd)
          [num file] (let [size (parse-long num)]
                       (recur (rest lines) (update-node dir-tree (add-to-path cwd file) {:size size}) cwd)))
        dir-tree))))

(defn calc-sizes [dir-tree]
  (-> (walk/postwalk (fn [node]
                       (if (or (not (map? node)) (:size node))
                         node
                         (->> (map :size (vals node))
                              (reduce +)
                              (assoc node :size))))
                     dir-tree)
      (dissoc :size)))

(defn find-all [dir-tree op size]
  (reduce-kv (fn [acc k v]
               (if (and (map? v) (> (count (keys v)) 1))
                 (let [acc (into acc (find-all v op size))]
                   (if (op (:size v) size)
                    (conj acc [k (:size v)])
                     acc))
                 acc))
          #{}
          dir-tree))

(comment
  (let [dirs (-> (build-dir-tree input)
                 (calc-sizes)
                 (find-all <= 100000))]
    (->> dirs
         (map second)
         (reduce +)))
  ;; Answer = 1350966
  ) 


;; Part 2

(comment
  (let [dirs (-> (build-dir-tree input)
                 (calc-sizes))
        unused-space (- 70000000 (:size (dirs "/")))
        space-needed (- 30000000 unused-space)]
    (->> (find-all dirs >= space-needed)
         (apply min-key second)))
  ; Answer = 6296435
  ) 

