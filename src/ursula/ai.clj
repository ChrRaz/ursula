(ns ursula.ai
  (:require [clojure.core.match :refer [match]]
            [ursula.game :as game]))

(defn random
  "Returns a random action from the state s"
  [s]
  (-> s
      game/actions
      rand-nth))

(defn expectiminimax
  "Returns the optimal action from the state s"
  [s]
  )

#_(let [s ursula.core/initial-state]
    (map #(result (first %) (ai/random (first %)))
         (roll-dice s)))
