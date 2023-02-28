(ns problems.problem-9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(defn read-input-file
  [file-location]
  (map (fn [[direction value]]
         [direction (Integer/parseInt value)]) (map (fn [move-command-str]
                                                      (str/split move-command-str #" ")) (str/split (slurp (io/resource file-location)) #"\n"))))

(defn initialise-grid
  [knots]
  {:grid #{}
   :head [0 0]
   :tails (vec (repeat knots [0 0]))})

(defn move-head
  [{:keys [head] :as state} direction]
  (case direction
    "R" (assoc state :head (update head 0 inc))
    "L" (assoc state :head (update head 0 dec))
    "U" (assoc state :head (update head 1 inc))
    "D" (assoc state :head (update head 1 dec))))

(defn average-of-two-points
  [a b]
  (/ (+ a b) 2))

(defn calculate-new-position
  [a b delta]
  (if (neg? delta)
    (int (Math/floor (average-of-two-points a b)))
    (int (Math/ceil (average-of-two-points a b)))))

(defn calculate-new-tail
  [knot tail]
  (let [[kx ky] knot
        [tx ty] tail
        dx (- kx tx)
        dy (- ky ty)]
    (if (or (> (abs dx) 1) (> (abs dy) 1))
      [(calculate-new-position kx tx dx) (calculate-new-position ky ty dy)]
      tail)))
(defn move-tails-if-seperated
  [{:keys [head tails] :as state}]
  (loop [knot head
         remaining-tails tails
         prev-tails []]
    (let [new-tail (calculate-new-tail knot (first remaining-tails))]
      (if (empty? (rest remaining-tails))
        (-> (update state :grid conj new-tail)
            (assoc :tails (conj prev-tails new-tail)))
        (recur new-tail (rest remaining-tails) (conj prev-tails new-tail))))))

(defn count-squares-visited
  [input num-tails]
  (->> (reduce (fn [state [direction value]]
                 (reduce (fn [temp-state _]
                           (-> (move-head temp-state direction)
                               (move-tails-if-seperated))) state (range value)))
               (initialise-grid num-tails)
               input)
      :grid
       count))
(comment
  (read-input-file "resources/problem-9.txt")
  (count-squares-visited (read-input-file "resources/problem-9.txt") 1)
  (count-squares-visited (read-input-file "resources/problem-9.txt") 9)
  )