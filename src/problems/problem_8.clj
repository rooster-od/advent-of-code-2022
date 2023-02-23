(ns problems.problem-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input-file
  [file-location]
  (mapv (fn [char-string]
          (mapv #(Integer/parseInt %) (str/split char-string #"")))  (seq (str/split (slurp (io/resource file-location)) #"\n"))))

(defn count-edge-trees
  [[first-row & remaining-trees :as all-trees]]
  (let [row-size (count first-row)
        column-size (count all-trees)]
    (+ (* 2 column-size) (* 2 row-size) -4)))

(defn create-columns-and-rows
  [rows]
  (let [row-length (count (first rows))]
    (loop [columns []
           counter 0]
      (if (= (count columns) row-length)
        {:rows (vec rows)
         :columns columns}
        (recur (conj columns (mapv #(nth % counter) rows))
               (inc counter))))))

(defn count-interior-trees
  [{:keys [rows columns]}]
  (let [number-of-columns (count columns)
        number-of-rows (count rows)
        counter (atom 0)]
    (doseq [y-pos (range 1 (- number-of-rows 1))
            x-pos (range 1 (- number-of-columns 1))
            :let [row (nth rows y-pos)
                  column (nth columns x-pos)
                  tree-height (nth (nth rows y-pos) x-pos)
                  east-view (subvec row 0 x-pos)
                  west-view (subvec row (+ x-pos 1) number-of-columns)
                  north-view (subvec column 0 y-pos)
                  south-view (subvec column (+ y-pos 1) number-of-rows)]]
      (when (some true? [(> tree-height (apply max east-view))
                         (> tree-height (apply max west-view))
                         (> tree-height (apply max north-view))
                         (> tree-height (apply max south-view))])
        (swap! counter inc)))
    @counter))

(defn take-while+
  [pred coll]
  (lazy-seq
    (when-let [[f & r] (seq coll)]
      (if (pred f)
        (cons f (take-while+ pred r))
        [f]))))
(defn calculate-scenic-score
  [views tree-height]
  (reduce (fn [scenic-score view]
            (* scenic-score
               (count (take-while+ #(< % tree-height) view)))) 1 views))

(defn calculate-highest-scenic-score
  [{:keys [rows columns]}]
  (let [number-of-columns (count columns)
        number-of-rows (count rows)
        highest-scenic-score (atom 0)]
    (doseq [y-pos (range 1 (- number-of-rows 1))
            x-pos (range 1 (- number-of-columns 1))
            :let [row (nth rows y-pos)
                  column (nth columns x-pos)
                  tree-height (nth (nth rows y-pos) x-pos)
                  east-view (reverse (subvec row 0 x-pos))
                  west-view (subvec row (+ x-pos 1) number-of-columns)
                  north-view (reverse (subvec column 0 y-pos))
                  south-view (subvec column (+ y-pos 1) number-of-rows)
                  scenic-score (calculate-scenic-score [east-view west-view north-view south-view] tree-height)]]
      (when (> scenic-score @highest-scenic-score)
        (reset! highest-scenic-score scenic-score)))
    @highest-scenic-score))

(comment
  (read-input-file "resources/problem-8-example.txt")
  (count-edge-trees (read-input-file "resources/problem-8.txt"))
  (create-columns-and-rows (read-input-file "resources/problem-8.txt"))
  (count-interior-trees (create-columns-and-rows (read-input-file "resources/problem-8.txt")))
  (calculate-highest-scenic-score (create-columns-and-rows (read-input-file "resources/problem-8.txt")))
  )