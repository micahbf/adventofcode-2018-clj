(ns adv.day11
  (:refer-clojure))

(defn power-level [x y s]
  (-> x
      (+ 10)
      (* y)
      (+ s)
      (* (+ x 10))
      (quot 100)
      (mod 10)
      (- 5)))


(defn powers [s]
  (into {} (for [x (range 1 301)
                 y (range 1 301)]
             [[x y] (power-level x y s)])))


(defn greatest-factor [n]
  (first (filter #(zero? (rem n %))
                 (range (quot n 2) 0 -1))))

(defn area-power [x y size powers]
  (reduce +
          (for [x (range x (+ size x))
                y (range y (+ size y))]
            (powers [x y]))))

(defn max-kv [f x]
  (let [max-k (apply max-key f x)]
    [max-k (f max-k)]))

;; returns [[x y] power]
(defn find-max [size powers]
  (max-kv (fn [[x y]] (area-power x y size powers))
          (for [x (range 1 (- 301 size))
                y (range 1 (- 301 size))]
            [x y])))

(def ps (powers 9435))

(def answer1 (first (find-max 3 ps)))

(def answer2 (->> (range 1 301)
                  (map #(vector % (find-max % ps)))
                  (max-key #(last (last %)))))
