(ns problems.problem-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def stacks {1 '() 2 '() 3 '() 4 '() 5 '() 6 '() 7 '() 8 '() 9 '() :index 0})

(defn read-input-file
  [file-location]
  (str/split (slurp (io/resource file-location)) #"\n"))

(defn create-stacks
  [stack-input]
  (dissoc (reduce (fn [current-stacks item]
                    (let [index (mod (:index current-stacks) 9)]
                      (-> (if (not= \space item)
                            (update current-stacks (+ 1 index) conj item)
                            current-stacks)
                          (assoc :index (+ index 1))))) stacks (flatten (map #(take-nth 4 (drop 1 %)) stack-input)))
          :index))

(defn parse-move-commands
  [move-input]
  (map (fn [command]
         (-> (str/replace command #"move | from| to" "")
             (str/split #" ")
             (#(map (fn [str]
                      (Integer/parseInt str)) %)))) move-input))

(defn determine-crate-order-part-1
  [file-input]
  (let [raw-stack-input (take 8 file-input)
        raw-move-input (nthrest file-input 10)
        stacks (update-vals (create-stacks raw-stack-input) vec)
        move-commands (parse-move-commands raw-move-input)]
    (reduce (fn [stacks [amount from to]]
              (loop [stacks stacks
                     idx amount]
                (let [top-crate (peek (get stacks from))]
                  (if (= 0 idx)
                    stacks
                    (recur (-> (update stacks from pop)
                               (update to conj top-crate)) (- idx 1))))))
            stacks move-commands)))

(defn determine-crate-order-part-2
  [file-input]
  (let [raw-stack-input (take 8 file-input)
        raw-move-input (nthrest file-input 10)
        stacks (update-vals (create-stacks raw-stack-input) vec)
        move-commands (parse-move-commands raw-move-input)]
    (reduce (fn [stacks [amount from to]]
              (-> (update stacks to concat (take-last amount (get stacks from)))
                   (update from (partial drop-last amount))))
            stacks move-commands)))

(comment
  (take 8 (read-input-file "resources/problem-5.txt"))
  (nthrest (read-input-file "resources/problem-5.txt") 10)
  (determine-crate-order-part-1 (read-input-file "resources/problem-5.txt"))
  (determine-crate-order-part-2 (read-input-file "resources/problem-5.txt"))
  )
