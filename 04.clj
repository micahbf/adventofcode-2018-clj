(ns adv.day04
  (:refer-clojure)
  (:require [clojure.string :as string]))

(def input (sort (string/split-lines (slurp "04.input.txt"))))


(def line-matcher #"\[[\d-]+ \d{2}:(\d{2})\] (.+)")
(def line-keys [:min :msg])

(defn parse-line [line]
  (update
   (->> line
        (re-matches line-matcher)
        (drop 1)
        (zipmap line-keys))
   :min #(Integer. %)))

(defn msg->event [msg]
  (let [guard-m (re-matcher #"Guard #(\d+) begins shift" msg)
        sleep-m (re-matcher #"falls asleep" msg)
        wake-m  (re-matcher #"wakes up" msg)]
    (cond
      (re-find guard-m) [:guard (read-string (second (re-groups guard-m)))]
      (re-find sleep-m) [:sleep]
      (re-find wake-m)  [:wake])))

(defn next-state [state {:keys [min event]}]
  (case (first event)
    :guard {:guard (second event)}
    :sleep (assoc state :sleep min)
    :wake  (assoc state :wake min)))

(defn states [lines]
  (let [with-events (map #(assoc % :event (msg->event (:msg %))) lines)]
    (reductions next-state {} with-events)))

(defn complete-states [states]
  (filter :wake states))

(defn guard-sleep-sum [states]
  (reduce (fn [sums {:keys [guard sleep wake]}]
            (update sums guard #(+ (or % 0) (- wake sleep))))
          {}
          states))

(def sleeps
  (->> input
       (map parse-line)
       (states)
       (complete-states)))

(defn max-val-by-key [h]
  (key (apply max-key val h)))

(def sleepiest-guard
  (max-val-by-key (guard-sleep-sum sleeps)))

(defn minute-sleepiness [events]
  (frequencies
   (reduce (fn [sleep-mins {:keys [sleep wake]}]
             (concat sleep-mins (range sleep wake)))
           []
           events)))

(defn sleepiest-minute [events]
  (max-val-by-key (minute-sleepiness events)))

(defn sleeps-for-guard [guard sleeps]
  (filter #(= guard (:guard %)) sleeps))

(def sleepiest-minute-for-guard
  (sleepiest-minute (sleeps-for-guard sleepiest-guard sleeps)))

(def answer1
  (* sleepiest-guard sleepiest-minute-for-guard))

(defn update-map [f m]
  (reduce-kv (fn [m k v]
               (assoc m k (f v))) {} m))

(def answer2
  (->> sleeps
       (group-by :guard)
       (update-map minute-sleepiness)
       (update-map #(apply max-key val %))))
