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

(def agents
  [{:ai/name "Player input"
    :ai/fn ui/user-input}
   {:ai/name "Random AI"
    :ai/fn (ai/random true)}
   {:ai/name "Greedy AI"
    :ai/fn (ai/greedy true ai/evaluate-sum-distance)}
   {:ai/name "Expectiminimax (easy)"
    :ai/fn (ai/expectiminimax-cutoff true 0.05 ai/evaluate-sum-distance)}
   {:ai/name "Expectiminimax (easy) eval2"
    :ai/fn (ai/expectiminimax-cutoff true 0.1 ai/evaluate-monte-carlo)}
   {:ai/name "Expectiminimax (medi)"
    :ai/fn (ai/expectiminimax-cutoff true 0.01 ai/evaluate-sum-distance)}
   {:ai/name "Expectiminimax (hard)"
    :ai/fn (ai/expectiminimax-cutoff true 0.001 ai/evaluate-sum-distance)}
   #_{:ai/name "Expectiminimax (graph)"
      :ai/fn (ai/expectiminimax-graph true)}])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (ui/present-agents agents)
  (let [player1 (ui/get-choice "Select white player" agents)
        player2 (ui/get-choice "Select black player" agents)
        [winning-move final-board] (last
                                    (game/run-game initial-state
                                                   {:player/white (:ai/fn player1)
                                                    :player/black (:ai/fn player2)}
                                                   true))]
    (println)
    (if (= 1 (game/utility final-board))
      (println "White player wins!")
      (println "Black player wins!"))))
