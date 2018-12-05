(ns adv.day05
  (:refer-clojure)
  (:require [clojure.string :as string :refer [upper-case]]))

(def input (string/trim-newline (slurp "05.input.txt")))

(def letters "abcdefghijklmnopqrstuvwxyz")
(def reactions
  (re-pattern (string/join "|"
               (map #(string/join [% (upper-case %) \| (upper-case %) %]) letters))))

(defn react [str]
  (loop [s str]
    (let [next (string/replace s reactions "")]
      (if (= s next)
        s
        (recur next)))))

(def answer1 (count (react input)))

(defn letter-pattern [char]
  (re-pattern (string/join [char \| (upper-case char)])))

(defn edited-lengths [str]
  (reduce (fn [h char]
            (let [edited (string/replace str (letter-pattern char) "")]
              (assoc h char (count (react edited)))))
          {}
          letters))

(def answer2 (apply min (vals (edited-lengths input))))
