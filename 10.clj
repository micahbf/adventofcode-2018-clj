(ns adv.day10
  (:refer-clojure)
  (:require [clojure.string :as string]))

(def input (->> (slurp "10.input.txt")
                string/split-lines
                (map #(re-seq #"-?\d+" %))
                (map #(map read-string %))
                (map #(zipmap [:x :y :dx :dy] %))))

(defn area [pts]
  (let [xs (map :x pts) ys (map :y pts)]
    (* (- (apply max xs)
          (apply min xs))
       (- (apply max ys)
          (apply min ys)))))

(defn step-pt [pt]
  (-> pt
      (update :x #(+ % (:dx pt)))
      (update :y #(+ % (:dy pt)))))

(defn step [pts]
  (map step-pt pts))

(defn find-smallest [input]
  (loop [pts input steps 0]
    (let [next-pts (step pts)]
      (if (> (area next-pts) (area pts))
        [steps pts]
        (recur next-pts (inc steps))))))

(defn print-row [row xmin xmax]
  (let [xs (into #{} (map :x row))]
    (println (string/join
              (map #(if (contains? xs %) "#" ".")
                   (range xmin (inc xmax)))
              ))))

(defn print-area [pts]
  (let [by-row (into (sorted-map) (group-by :y pts))
        xs (map :x pts)
        xmin (apply min xs)
        xmax (apply max xs)]
    (map #(print-row (second %) xmin xmax) by-row)))

;; answer 1
(print-area (second (find-smallest input)))

(def answer2 (first (find-smallest input)))
