(ns problems.problem-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def pick-key {"A X" 4
                "A Y" 8
                "A Z" 3
                "B X" 1
                "B Y" 5
                "B Z" 9
                "C X" 7
                "C Y" 2
                "C Z" 6})

(def outcome-key {"A X" 3
                "A Y" 4
                "A Z" 8
                "B X" 1
                "B Y" 5
                "B Z" 9
                "C X" 2
                "C Y" 6
                "C Z" 7})

(defn read-input-file
  [file-location]
  (str/split (slurp (io/resource file-location)) #"\n"))

(defn get-round-result-from-pick
  [round]
  (get pick-key round))

(defn get-round-result-from-outcome
  [round]
  (get outcome-key round))

(defn calculate-score-from-pick
  [game-input]
  (reduce (fn [total game-round]
            (+ total (get-round-result-from-pick game-round)))
          0 game-input))

(defn calculate-score-from-outcome
  [game-input]
  (reduce (fn [total game-round]
            (+ total (get-round-result-from-outcome game-round)))
          0 game-input))

(comment
  (read-input-file "resources/problem-2.txt")
  (calculate-score-from-pick (read-input-file "resources/problem-2.txt"))
  (calculate-score-from-outcome (read-input-file "resources/problem-2.txt"))
  (calculate-score-from-pick ["A Y"
                              "B X"
                              "C Z"])
  (calculate-score-from-outcome ["A Y"
                                 "B X"
                                 "C Z"])
  )