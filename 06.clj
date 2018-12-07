(ns adv.day06
  (:refer-clojure)
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input
  (->> (slurp "06.input.txt")
       string/split-lines
       (map #(string/split % #", "))
       (map #(mapv read-string %))))

(defn seed->area [pt]
  {:done #{} :growing #{pt} :new #{}})

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn grow-pt [pt]
  (into #{} (map #(mapv + pt %) directions)))

(defn grow-area [area]
  (assoc area :new
         (set/difference
          (into #{} (mapcat grow-pt (:growing area)))
          (:growing area)
          (:done area))))

(defn non-colliding [pts area]
  (set/difference pts
                  (:new area)
                  (:growing area)
                  (:done area)))

(defn handle-collisions [areas]
  (let [indexed (map-indexed #(vec [%1 %2]) areas)]
    (map (fn [[idx area]]
           (reduce (fn [next [other-idx other]]
                     (if (= idx other-idx)
                       next
                       (update next :new #(non-colliding % other))))
                   area
                   indexed))
         indexed)))

(defn grow-all [areas]
  (map grow-area areas))

(defn step [area]
  (-> area
      (update :done #(set/union % (area :growing)))
      (assoc :growing (area :new))
      (assoc :new #{})))

(defn bounding-box [pts]
  (let [xs (map first pts) ys (map second pts)]
    {:xmin (apply min xs) :ymin (apply min ys)
     :xmax (apply max xs) :ymax (apply max ys)}))

(defn outside? [[x y] {:keys [xmin xmax ymin ymax]}]
  (or
   (< x xmin)
   (> x xmax)
   (< y ymin)
   (> y ymax)))

(defn infinite? [area box]
  (some #(outside? % box) (:done area)))

(defn expand [seeds]
  (let [areas (map seed->area seeds) box (bounding-box seeds)]
    (loop [areas init-areas]
      (if (every? #(or (infinite? % box) (empty? (:growing %))) areas)
        areas
        (recur (->> areas
                   grow-all
                   handle-collisions
                   (map step)))))))

(def box (bounding-box input))

(def answer1
  (->> (expand input)
       (filter #(not (infinite? % box)))
       (map #(count (:done %)))))

(defn dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn total-dist [pt pts]
  (reduce + (map #(dist pt %) pts)))

(defn points-in-bounds [{:keys [xmin xmax ymin ymax]}]
  (for [x (range xmin (inc xmax))
        y (range ymin (inc ymax))]
    [x y]))

(def answer2
  (->> (points-in-bounds {:xmin 0 :ymin 0 :xmax 500 :ymax 500})
       (map #(total-dist % input))
       (filter #(< % 10000))
       count))
