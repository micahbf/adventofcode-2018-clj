(require '[clojure.string :as str])

(def input (slurp "01.input.txt"))

(def input-ints (map #(Integer. %) (str/split-lines input)))

(def answer1 (reduce + input-ints))

(def answer2
  (loop [input-c (cycle input-ints) seen #{} sum 0]
    (if (contains? seen sum)
      sum
      (recur (rest input-c) (conj seen sum) (+ sum (first input-c))))))
