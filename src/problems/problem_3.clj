(ns problems.problem-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def priority-dict {\a 1
                    \b 2
                    \c 3
                    \d 4
                    \e 5
                    \f 6
                    \g 7
                    \h 8
                    \i 9
                    \j 10
                    \k 11
                    \l 12
                    \m 13
                    \n 14
                    \o 15
                    \p 16
                    \q 17
                    \r 18
                    \s 19
                    \t 20
                    \u 21
                    \v 22
                    \w 23
                    \x 24
                    \y 25
                    \z 26
                    \A 27
                    \B 28
                    \C 29
                    \D 30
                    \E 31
                    \F 32
                    \G 33
                    \H 34
                    \I 35
                    \J 36
                    \K 37
                    \L 38
                    \M 39
                    \N 40
                    \O 41
                    \P 42
                    \Q 43
                    \R 44
                    \S 45
                    \T 46
                    \U 47
                    \V 48
                    \W 49
                    \X 50
                    \Y 51
                    \Z 52})

(defn read-input-file
  [file-location]
  (str/split (slurp (io/resource file-location)) #"\n"))

(defn calculate-sum-of-priorities
  [file-input]
  (reduce (fn [total rucksack-items]
            (let [split-point (/ (count rucksack-items) 2)
                  split-regex (re-pattern (str ".{1," split-point "}"))]
              (->> (re-seq split-regex rucksack-items)
                  (#(apply set/intersection (map set %)))
                  (#(get priority-dict (first %)))
                  (+ total))))
          0 file-input))

(defn calculate-sum-of-badge-priorities
  [file-input]
  (reduce (fn [total rucksack-items]
            (->> (apply set/intersection (map set rucksack-items))
                 (#(get priority-dict (first %)))
                 (+ total)))
          0 (partition 3 file-input)))

(comment
  (read-input-file "resources/problem-3.txt")
  (calculate-sum-of-priorities (read-input-file "resources/problem-3.txt"))
  (calculate-sum-of-badge-priorities (read-input-file "resources/problem-3.txt"))
  )