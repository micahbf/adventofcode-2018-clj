(require '[clojure.string :as str])

(def input (str/split-lines (slurp "02.input.txt")))

(defn count-with-n-chars [words n]
  (count (filter #(contains? (set (vals (frequencies %))) n) words)))

(def answer1 (* (count-with-n-chars input 2) (count-with-n-chars input 3)))

(defn count-different-chars [word1 word2]
  (let [pairs (map vector (seq word1) (seq word2))]
    (count (filter #(apply not= %) pairs))))

(defn pairs [aseq]
  (loop [pairs [] x (first aseq) xs (rest aseq)]
    (if (empty? xs)
        pairs
        (recur (concat pairs (map #(vector x %) xs)) (first xs) (rest xs)))))

(def answer2 (filter #(= 1 (apply count-different-chars %)) (pairs input)))
