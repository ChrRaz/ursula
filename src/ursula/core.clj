(ns ursula.core
  (:gen-class)
  (:require [ursula.ai :as ai]
            [ursula.game :as game]
            [ursula.utils :as utils]))

(def initial-state
  {:game/turn :player/white
   ;; :game/dice nil
   :game/pre-board {:player/white 7
                    :player/black 7}
   :game/board {}
   :game/post-board {:player/white 0
                     :player/black 0}})

(defn run-turn
  [[_ state] players]
  (let [player (get players (:game/turn state))
        rolled-state (utils/roll-dice state game/dice-chances)
        action (player rolled-state)]
    #_(prn
       (:game/turn rolled-state)
       (:game/dice rolled-state)
       action)
    [action
     (game/result rolled-state action)]))

(defn run-game
  [initial players]
  (->> [:state/initial initial]
       (iterate (fn [state]
                  (run-turn state players)))
       (take-while (fn [[_ state]]
                     (not (game/terminal? state))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
