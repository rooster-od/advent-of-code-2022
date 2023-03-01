(ns problems.problem-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-command
  [command-str]
  (cond
    (= "noop" command-str) [:noop]
    (re-matches #"addx -?\d+" command-str) (let [[command val] (str/split command-str #" ")]
                                             [(keyword command) (Integer/parseInt val)])
    :default nil))

(defn read-input-file
  [file-location]
  (map parse-command (str/split (slurp (io/resource file-location)) #"\n")))
(defn calculate-signal-strength
  [commands]
  (->> (reduce (fn [state [command val]]
                (reduce (fn [{:keys [cycle-count x] :as temp-state} cycle]
                          (cond-> temp-state
                                  (= 0 (mod cycle-count 40)) (update :signal-strengths conj (* x (+ 20 cycle-count)))
                                  true (update :cycle-count inc)
                                  (and (= cycle 1)
                                       (= command :addx)) (update :x + val)))
                        state
                        (range (case command
                                 :noop 1
                                 :addx 2))))
              {:cycle-count -19
               :x 1
               :signal-strengths []}
              commands)
      :signal-strengths
      (take 6)
      (apply +)))

(defn render-pixel
  [{:keys [cycle-count x] :as temp-state}]
  (let [sprite-positions #{(dec x) x (inc x)}]
    (if (contains? sprite-positions (mod cycle-count 40))
      (update temp-state :crt-buffer conj "#")
      (update temp-state :crt-buffer conj "."))))
(defn render-word
  [commands]
  (->> (reduce (fn [state [command val]]
                 (reduce (fn [{:keys [cycle-count x] :as temp-state} cycle]
                           (cond-> temp-state
                                   true (render-pixel)
                                   true (update :cycle-count inc)
                                   (and (= cycle 1)
                                        (= command :addx)) (update :x + val)))
                         state
                         (range (case command
                                  :noop 1
                                  :addx 2))))
               {:cycle-count 0
                :x 1
                :crt-buffer []}
               commands)
       :crt-buffer
       (partition 40)
       (map println)))

(comment
  (read-input-file "resources/problem-10-example.txt")
  (calculate-signal-strength (read-input-file "resources/problem-10.txt"))
  (render-word (read-input-file "resources/problem-10.txt"))
  )