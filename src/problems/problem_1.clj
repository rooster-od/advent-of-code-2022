(ns problems.problem-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input-file
  [file-location]
  (mapv (fn [vec-of-strings]
          (mapv #(Integer/parseInt %) vec-of-strings))
        (mapv #(str/split % #"\n") (str/split (slurp (io/resource file-location)) #"\n\n"))))

(defn sum-up-group
  [group]
  (reduce + group))

(defn find-max-in-contents
  [contents]
  (reduce (fn [current-max group]
            (let [new-val (sum-up-group group)]
              (if (> new-val current-max)
                new-val
                current-max)))
          0 contents))

(defn find-top-three-in-contents
  [contents]
  (reduce (fn [top-three group]
            (let [new-val (sum-up-group group)
                  top-four (sort (conj top-three new-val))]
              (take-last 3 top-four)))
          [0 0 0] contents))

(comment
  (read-input-file "resources/problem-1.txt")
  (find-max-in-contents (read-input-file "resources/problem-1.txt"))
  (find-top-three-in-contents (read-input-file "resources/problem-1.txt"))
  (sum-up-group (find-top-three-in-contents (read-input-file "resources/problem-1.txt")))
  )