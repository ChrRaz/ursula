(ns ursula.core
  (:gen-class)
  (:require [ursula.ai :as ai]
            [ursula.game :as game]
            [ursula.ui :as ui]
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
    ;; TODO: good response instead
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
       (utils/take-upto (fn [[_ state]]
                          (game/terminal? state)))))

(def agents
  [{:ai/name "Player input"
    :ai/fn ui/user-input}
   {:ai/name "Random AI"
    :ai/fn (ai/random true)}
   {:ai/name "Heuristic AI"
    :ai/fn (ai/heuristic true)}
   {:ai/name "Expectiminimax (easy)"
    :ai/fn (ai/expectiminimax-cutoff true 0.05)}
   {:ai/name "Expectiminimax (hard)"
    :ai/fn (ai/expectiminimax-cutoff true 0.001)}])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (ui/present-agents agents)
  (let [player1 (ui/get-choice "Select white player" agents)
        player2 (ui/get-choice "Select black player" agents)
        [winning-move final-board] (last
                                    (run-game initial-state
                                              {:player/white (:ai/fn player1)
                                               :player/black (:ai/fn player2)}))]
    (if (= 1 (game/utility final-board))
      (println "White player wins!")
      (println "Black player wins!"))))
