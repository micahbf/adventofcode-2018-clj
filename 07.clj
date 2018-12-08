(ns adv.day07
  (:refer-clojure)
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (->> (slurp "07.input.txt")
                string/split-lines
                (map #(vector (nth % 5) (nth % 36)))))

(defn all-nodes [input]
  (set (flatten input)))

(defn input->deps [input]
  (reduce (fn [deps node]
            (update deps (second node)
                    #(conj % (first node))))
          (into {} (map #(vector % #{}) (all-nodes input)))
          input))

(defn root-nodes [input]
  (set/difference (all-nodes input) (set (keys (input->deps input)))))

(defn ready [complete deps]
  (let [done (set complete)]
    (set/difference
     (->> deps
          (filter #(set/subset? (second %) done))
          (map first)
          set)
     done)))

(defn resolve-graph [input]
  (let [deps (input->deps input) all (all-nodes input)]
    (loop [order []]
      (if (= (set order) all)
        order
        (recur (->> deps
                    (ready order)
                    sort
                    first
                    (conj order)
                    )))
      )))

(def answer1 (string/join (resolve-graph input)))

(defn cost [char]
  (- (int char) 4))

(defn filter-keys [f m]
  (map first (filter #(f (second %)) m)))

(defn min-val [m]
  (apply min (vals m)))

(defn pull-next [order working n time deps]
  (let [done (filter-keys #(= time %) working)
        next-order (concat order (sort done))
        ready (set/difference (ready next-order deps) (set (keys working)))]
    (->> ready
         sort
         (take (- n (- (count working) (count done))))
         (map #(vector % (+ time (cost %))))
         (into (apply dissoc working done))
         (vector next-order)
         )))

(defn p-resolve [input n]
  (let [deps (input->deps input) all (all-nodes input)]
    (loop [order [] working {} time 0]
      (let [[next-order next-working] (pull-next order working n time deps)]
        (if (empty? next-working)
          time
          (recur next-order next-working (min-val next-working))))
      )))

(def answer2 (p-resolve input 5))
