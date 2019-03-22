(ns ursula.utils
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defn dissoc-in
  "Dissociates a value in a nested associative structure, where ks is a
  sequence of keys and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  [m [k & ks]]
  (if ks
    (if (contains? m k)
      (assoc m k (dissoc-in (get m k) ks))
      m)
    (dissoc m k)))

(def other-player
  {:player/white :player/black
   :player/black :player/white})

(def player-side
  {:player/white :a
   :player/black :c})

(defn rolled-states
  "Returns the states where the dice have been rolled
  as well as the probability of that roll occuring"
  [s dice-chances]
  (for [[dice prop] dice-chances]
    [(assoc s :game/dice dice) prop]))

(defn roll-dice
  "Returns a state where the dice have been rolled
  as well as the probability of that roll occuring"
  [s dice-chances]
  (let [target (rand)]
    (loop [states (rolled-states s dice-chances)
           acc 0N]
      (let [[state weight :as curr] (first states)]
        (if (> (+ acc weight) target)
          state
          (recur (next states)
                 (+ acc weight)))))))

(defn take-upto
  "Returns a lazy sequence of successive items from coll up to and including
  the first item for which `(pred item)` returns true.
  https://github.com/weavejester/medley/blob/1.1.0/src/medley/core.cljc#L263"
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [x (first s)]
        (cons x (if-not (pred x) (take-upto pred (rest s)))))))))

(defn merge-lines
  "Takes two multiline strings and joins them line by line.
  Assumes that all lines in s1 are of equal length."
  [s1 s2]
  (str/join \newline
            (map str
                 (str/split-lines s1)
                 (str/split-lines s2))))
