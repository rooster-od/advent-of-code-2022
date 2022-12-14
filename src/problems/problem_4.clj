(ns problems.problem-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input-file
  [file-location]
  (str/split (slurp (io/resource file-location)) #"\n"))

(defn calculate-fully-overlapping-pairs
  [file-input]
  (reduce (fn [total pairs]
            (let [[pair-1-low pair-1-high pair-2-low pair-2-high] (map #(Integer/parseInt %) (flatten (map #(str/split % #"-") (str/split pairs #","))))]
              (if (or (<= pair-1-low pair-2-low pair-2-high pair-1-high)
                      (<= pair-2-low pair-1-low pair-1-high pair-2-high))
                (+ total 1)
                total)))
          0 file-input))

(defn calculate-partially-overlapping-pairs
  [file-input]
  (reduce (fn [total pairs]
            (let [[pair-1-low pair-1-high pair-2-low pair-2-high] (map #(Integer/parseInt %) (flatten (map #(str/split % #"-") (str/split pairs #","))))]
              (if (or (<= pair-1-low pair-2-low pair-1-high)
                      (<= pair-1-low pair-2-high pair-1-high)
                      (<= pair-2-low pair-1-low pair-2-high)
                      (<= pair-2-low pair-1-high pair-2-high))
                (+ total 1)
                total)))
          0 file-input))

(comment
  (calculate-partially-overlapping-pairs (read-input-file "resources/problem-4.txt"))
  (calculate-fully-overlapping-pairs (read-input-file "resources/problem-4.txt"))
  )