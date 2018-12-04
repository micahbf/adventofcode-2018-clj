(require '[clojure.string :as str])
(def input (slurp "03.input.txt"))

(def line-matcher #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
(def line-keys [:id :x :y :w :h])

(defn parse-line [line]
  (->> line
       (re-matches line-matcher)
       (drop 1)
       (map read-string)
       (zipmap line-keys)))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(defn rect-to-corners [rect]
  (let [{:keys [x y w h]} rect]
    {:x1 x :y1 y :x2 (+ x w) :y2 (+ y h)}))

(defn corners-to-rect [corners]
  (let [{:keys [x1 y1 x2 y2]} corners]
    {:x x1
     :y x2
     :w (- x2 x1)
     :h (- y2 y1)}))

(defn intersection [& rects]
  (let [rect-cs (map rect-to-corners rects)
        inter {:x1 (apply max (map :x1 rect-cs))
               :y1 (apply max (map :y1 rect-cs))
               :x2 (apply min (map :x2 rect-cs))
               :y2 (apply min (map :y2 rect-cs))}]
    (if (and (> (:x2 inter) (:x1 inter))
             (> (:y2 inter) (:y1 inter)))
      (corners-to-rect inter))))

(defn pairs [s]
  (loop [pairs [] x (first s) xs (rest s)]
    (if (empty? xs)
      pairs
      (recur (concat pairs (map #(vector x %) xs)) (first xs) (rest xs)))))

(defn intersections [rects]
  (->> rects
       (pairs)
       (map #(apply intersection %))
       (remove nil?)))

(defn area [rect]
  (* (:w rect) (:h rect)))

(def ints (intersections (parse input)))

(defn points [rect]
  (let [
(def answer1 [input]
  (loop [points (mapcat points input) seen #{} counted #{}]
    (if (empty? points)
      (count counted)
      (let [point (first points) next-points (rest points)]
        (if (contains? seen point)
          ())))

(def seen #{})
(def counted #{})

(def rect-points (mapcat points input))

corners (rect-to-corners rect)]
    (for [x (range (corners :x1) (corners :x2))
          y (range (corners :y1) (corners :y2))]
         [x y])))

(defn intersection-check [point])

(reduce (fn [{:keys [seen counted] result} point
             (cond
               (not (contains? seen counted)) (assoc result ))])
        {:seen #{} :counted #{}})
