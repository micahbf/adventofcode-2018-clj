(ns adv.day08
  (:refer-clojure)
  (:require [clojure.string :as string]))

(def input (map read-string
                (string/split (slurp "08.input.txt") #"\s+")))

(declare children)

(defn parse-node [v]
  (let [[n-child n-meta] (take 2 v)
        data (drop 2 v)
        [children data] (children n-child data)]
    [{:children children :meta (take n-meta data)} (drop n-meta data)]))

(defn children
  ([n-child v] (children n-child v []))
  ([n-child v children]
   (if (= n-child 0)
     [children v]
     (let [[c r] (parse-node v)]
       (recur (- n-child 1) r (conj children c)))
     )))

(def root (first (parse-node input)))

(defn sum-meta [node]
  (+ (reduce + (:meta node))
     (reduce + (map sum-meta (:children node)))))

(def answer1 (sum-meta root))

(def null-node {:children [] :meta []})

(defn value [node]
  (if (empty? (:children node))
      (sum-meta node)
      (->> (:meta node)
           (map #(nth (:children node) (dec %) null-node))
           (map #(value %))
           (reduce +))
      ))

(def answer2 (value root))
