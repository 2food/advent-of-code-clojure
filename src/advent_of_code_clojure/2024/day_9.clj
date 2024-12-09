(ns advent-of-code-clojure.2024.day-9
  (:require [advent-of-code-clojure.inputs :as inputs]))

(def input
  (->> (inputs/get-input-for-this-day)
       (drop-last)
       (mapv (comp parse-long str))))

;; Part 1

(defn split-parts [ns]
  (reduce (fn [acc n]
            (-> (if (:file? acc)
                  (update acc :files conj n)
                  (update acc :spaces conj n))
                (update :file? not)))
          {:file? true :files [] :spaces []}
          ns))

(defn expand-format [ns]
  (let [{:keys [files spaces]} (split-parts ns)]
    (->> (interleave
          (map-indexed (fn [idx n] (repeat n idx)) files)
          (concat (map (fn [n] (repeat n nil)) spaces) [[]]))
         (apply concat)
         (vec))))

(defn de-expand [expanded]
  (:ns (reduce (fn [acc n]
                 (if (= (:last-val acc) n)
                   (update-in acc [:ns (dec (count (:ns acc)))] inc)
                   (if (and (:last-val acc) n)              ; going straight from one file to another with no space
                     (-> acc
                         (assoc :last-val n)
                         (update :ns conj 0 1))
                     (-> acc
                         (assoc :last-val n)
                         (update :ns conj 1)))))
               {:last-val nil
                :ns       []}
               expanded)))

(defn swap [v ia ib]
  (let [a (get v ia)
        b (get v ib)]
    (assoc v ia b ib a)))

(defn compact-step [disk first-nil last-digit]
  (if (< first-nil last-digit)
    (swap disk first-nil last-digit)
    disk))

(defn compact [disk]
  (loop [disk   disk
         nils   (keep-indexed (fn [idx n] (when-not n idx)) disk)
         digits (keep-indexed (fn [idx n] (when n (- (dec (count disk)) idx))) (reverse disk))]
    (let [new-disk (compact-step disk (first nils) (first digits))]
      (if (not= disk new-disk)
        (recur new-disk (rest nils) (rest digits))
        disk))))

(defn checksum [ns]
  (->> ns
       (keep-indexed (fn [idx n] (when n (* idx n))))
       (reduce +)))

(comment
  (= input (de-expand (expand-format input)))

  (def expanded (expand-format input))
  (let [first-nil  (some identity (map-indexed (fn [idx n] (when-not n idx)) expanded))
        last-digit (- (dec (count expanded))
                      (some identity (map-indexed (fn [idx n] (when n idx)) (reverse expanded))))]
    (time (compact-step expanded first-nil last-digit)))

  (checksum (compact (expand-format input)))

  ; Answer = 6262891638328

  )

;; Part 2

(defn group-contiguous [ns]
  (:segments (reduce (fn [{:keys [last-val segments] :as acc} n]
                       (-> (if (or (not last-val) (= (- last-val n) -1))
                             (assoc-in acc [:segments (dec (count segments)) 1] n)
                             (update acc :segments conj [n n]))
                           (assoc :last-val n)))
                     {:last-val nil
                      :segments [[(first ns) (first ns)]]}
                     ns)))

(defn segment-size [[n m]] (- (inc m) n))

(comment
  (group-contiguous [1 2 3 5 6 7])
  )

(defn segment-format [expanded]
  (let [nils (keep-indexed (fn [idx n] (when-not n idx)) expanded)]
    {:spaces (group-by segment-size (group-contiguous nils))
     :files  (->> (distinct expanded)
                  (filter some?)
                  (map (fn [n] {n ((juxt first last)
                                   (keep-indexed (fn [idx m] (when (= m n) idx)) expanded))}))
                  (apply merge))}))

(defn de-segment [{:keys [spaces files]}]
  (let [disk (vec (repeat (apply max (concat (flatten (vals spaces))
                                             (flatten (vals files)))) nil))]
    (reduce (fn [disk [file-id file-segment]]
              (reduce (fn [disk idx] (assoc disk idx file-id))
                      disk
                      (range (first file-segment) (inc (last file-segment)))))
            disk
            files)))

(comment
  (reverse (sort (keys (:files (segment-format input)))))

  (def expanded (expand-format input))
  (def segments (segment-format expanded))
  (= expanded (de-segment segments))

  (def file-ids (reverse (sort (keys (:files segments)))))

  (->> (reduce (fn [{:keys [spaces files]} file-id]
                 (let [file-segment  (get files file-id)
                       sizes         (filter #(>= % (segment-size file-segment)) (keys spaces))
                       fitting-space (first (sort-by first (mapcat #(get spaces %) sizes)))]
                   (if (or (not fitting-space) (> (first fitting-space) (first file-segment)))
                     {:spaces spaces :files files}
                     (let [new-file-segment  [(first fitting-space)
                                              (+ (first fitting-space) (dec (segment-size file-segment)))]
                           new-space-segment [(+ (first fitting-space) (segment-size file-segment))
                                              (last fitting-space)]]
                       {:spaces (-> spaces
                                    (update (segment-size fitting-space) (fn [s] (vec (remove #(= fitting-space %) s))))
                                    (update (segment-size file-segment) (fn [s] (vec (sort-by first (conj s file-segment)))))
                                    (update (- (segment-size fitting-space)
                                               (segment-size file-segment)) (fn [s] (vec (sort-by first (conj s new-space-segment))))))
                        :files  (assoc files file-id new-file-segment)}))))
               segments
               file-ids)
       (de-segment)
       (checksum))

  ; Answer = 6287317016845

  )
