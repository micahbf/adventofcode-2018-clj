(ns adv.day09
  (:refer-clojure))

;; implement a circular doubly linked list

(defn make-el [p n v]
  {:prev (atom p) :next (atom n) :val v})

(defn init-el [v]
  (let [el (make-el nil nil v)]
    (reset! (:prev el) el)
    (reset! (:next el) el)
    el))

(defn insert-after [{anext :next :as prev} v]
  (let [next @anext el (make-el prev next v)]
    (reset! (:prev next) el)
    (reset! (:next prev) el)
    el))

(defn remove-el [{:keys [prev next]}]
  (reset! (:next @prev) @next)
  (reset! (:prev @next) @prev)
  @next)

(defn traverse [el n]
  (cond (= n 0) el
        (< n 0) (recur @(:prev el) (inc n))
        (> n 0) (recur @(:next el) (dec n))))

;; marble actions
;; return tuple of [current marble, points scored]

(defn place-marble [cur val]
  [(insert-after (traverse cur 1) val) 0])

(defn remove-marble [cur val]
  (let [removing (traverse cur -7)]
    [(remove-el removing)
     (+ (:val removing) val)]))

(defn multiple? [x y]
  (and (not (zero? x))
       (zero? (mod x y))))

(defn turn [cur val]
  (if (multiple? val 23)
    (remove-marble cur val)
    (place-marble cur val)))

(defn turn-reducer [[scores cur] val]
  (let [cur-player (mod val (count scores))
        [next-cur scored] (turn cur val)]
    [(update scores cur-player #(+ % scored))
     next-cur]))

(defn play [n-players max-marble]
  (let [scores (vec (repeat n-players 0))]
    (reduce turn-reducer
            [scores (init-el 0)]
            (range 1 (inc max-marble)))))

(defn max-score [n-players max-marble]
  (->> (play n-players max-marble)
       first
       (apply max)))

(def answer1 (max-score 459 71320))
(def answer2 (max-score 459 7132000))
