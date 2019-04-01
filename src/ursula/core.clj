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
  [[_ state] players print?]
  (let [player (get players (:game/turn state))
        rolled-state (utils/roll-dice state game/dice-chances)
        action (player rolled-state)
        new-state (game/result rolled-state action)]
    (when print?
      (println)
      (println (utils/merge-lines (ui/board->string new-state)
                                 (ui/board-info new-state))))
    ;; TODO: good response instead
    #_(prn
       (:game/turn rolled-state)
       (:game/dice rolled-state)
       action)
    [action new-state]))

(defn run-game
  [initial players print?]
  (when print?
    (println)
    (println (utils/merge-lines (ui/board->string initial-state)
                                (ui/board-info initial-state))))
  (->> [:state/initial initial]
       (iterate (fn [state]
                  (run-turn state players print?)))
       (utils/take-upto (fn [[_ state]]
                          (game/terminal? state)))))

(def agents
  [{:ai/name "Player input"
    :ai/fn ui/user-input}
   {:ai/name "Random AI"
    :ai/fn (ai/random true)}
   {:ai/name "Greedy AI"
    :ai/fn (ai/greedy true)}
   {:ai/name "Expectiminimax (easy)"
    :ai/fn (ai/expectiminimax-cutoff true 0.05)}
   {:ai/name "Expectiminimax (medi)"
    :ai/fn (ai/expectiminimax-cutoff true 0.01)}
   {:ai/name "Expectiminimax (hard)"
    :ai/fn (ai/expectiminimax-cutoff true 0.001)}
   #_{:ai/name "Expectiminimax (graph)"
    :ai/fn (ai/expectiminimax-graph true)}])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (ui/present-agents agents)
  (let [player1 (ui/get-choice "Select white player" agents)
        player2 (ui/get-choice "Select black player" agents)
        [winning-move final-board] (last
                                    (run-game initial-state
                                              {:player/white (:ai/fn player1)
                                               :player/black (:ai/fn player2)}
                                              true))]
    (println)
    (if (= 1 (game/utility final-board))
      (println "White player wins!")
      (println "Black player wins!"))))
