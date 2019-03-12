(ns ursula.core
  (:gen-class))

(defprotocol GameState
  (player [s] "Which player has the move in a state")
  (chance-node? [s] "Is s a chance-node?")
  (actions [s] "The set of legal moves in the state")
  (result [s a] "The result of a move")
  (terminal? [s] "Is the game over?")
  (utility [s p] "The utility of the state to player p")
  )

#_(def initial-state
  {:game/turn :player/white
   :player/white {:pregame 7
                  :board #{}
                  :postgame 0}
   :player/black {:pregame 7
                  :board #{}
                  :postgame 0}})

(def initial-state
  {:game/turn :player/white
   :game/dice nil
   :game/pre-board {:player/white 7
                     :player/black 7}
   :game/board {}
   :game/post-board {:player/white 0
                     :player/black 0}})

(defn actions
  [state roll]
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
