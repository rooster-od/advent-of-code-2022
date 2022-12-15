(ns problems.problem-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input-file
  [file-location]
  (slurp (io/resource file-location)))

(defn find-start-of-packet
  [comms-input buffer-size]
  (loop [buffer []
         idx 0
         stream comms-input]
    (cond
      (> buffer-size (count buffer)) (recur (conj buffer (first stream)) (inc idx) (drop 1 stream))
      (apply distinct? buffer) idx
      (= buffer-size (count buffer)) (recur (conj (vec (drop 1 buffer)) (first stream)) (inc idx) (drop 1 stream)))))

(comment

  (find-start-of-packet (read-input-file "resources/problem-6.txt") 4)
  (find-start-of-packet (read-input-file "resources/problem-6.txt") 14)
  )