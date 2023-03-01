(ns problems.problem-7

(defn is-directory?
  [input]
  (str/starts-with? input "dir "))

(defn parse-directory
  [{:keys [current-dir] :as state} dir-input]
  (let [[_ dir] (str/split dir-input #" ")
        ptr (conj current-dir (keyword dir))]
    (assoc-in state ptr {:size 0})))

(defn is-file?
  [input]
  (string? (re-matches #"\d+ [a-zA-Z.1-9]+" input)))

(defn parse-file
  [{:keys [current-dir] :as state} file-input]
  (let [[file-size] (str/split file-input #" ")
        size (Integer/parseInt file-size)]
    (loop [path current-dir
           result state]
      (if (< 1 (count path))
        (recur (pop path)
               (update-in result (conj path :size) + size))
        result))))

(defn is-command?
  [input]
  (str/starts-with? input "$ "))

(defn parse-cmd
  [state cmd-input]
  (let [[_ cmd dir] (str/split cmd-input #" ")]
    (case cmd
      "ls" state
      "cd" (if (= ".." dir)
             (update state :current-dir pop)
             (update state :current-dir conj (keyword dir))))))


(defn create-dir-structure
  [input]
  (:dir-structure
    (reduce (fn [state element]
              (cond
                (is-command? element) (parse-cmd state element)
                (is-directory? element) (parse-directory state element)
                (is-file? element) (parse-file state element)))
            {:dir-structure {:/ {:size 0}}
             :current-dir [:dir-structure]} input)))

(defn walk-dir-and-sum-small-files                          ;part 1
  [tree]
  (let [files (atom [])]
    (walk/postwalk (fn [x]
                     (if-let [size (:size x)]
                       (if (<= size 100000)
                         (swap! files conj size)
                         x)
                       x) x) tree)
    (reduce + @files)))

(defn walk-dir-and-find-smallest-large-file                 ;part 2
  [tree]
  (let [files (atom [])]
    (walk/postwalk (fn [x]
                     (if-let [size (:size x)]
                       (if (>= size 208860)
                         (swap! files conj size)
                         x)
                       x) x) tree)
    (apply min @files)))

(comment
  (read-input-file "resources/problem-7.txt")
  (walk-dir-and-sum-small-files (create-dir-structure (read-input-file "resources/problem-7.txt")))
  (walk-dir-and-find-smallest-large-file (create-dir-structure (read-input-file "resources/problem-7.txt")))
  )


