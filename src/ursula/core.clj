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
   {:ai/name "H-Expectiminimax (5% sum-distance)"
    :ai/fn (ai/expectiminimax-cutoff true 0.05 ai/evaluate-sum-distance)}
   {:ai/name "H-Expectiminimax (1% sum-distance)"
    :ai/fn (ai/expectiminimax-cutoff true 0.01 ai/evaluate-sum-distance)}
   {:ai/name "H-Expectiminimax (10% monte-carlo)"
    :ai/fn (ai/expectiminimax-cutoff true 0.1 ai/evaluate-monte-carlo)}
   {:ai/name "H-Expectiminimax (5% monte-carlo)"
    :ai/fn (ai/expectiminimax-cutoff true 0.05 ai/evaluate-monte-carlo)}
   {:ai/name "Expectiminimax (graph search) [Overflows]"
    :ai/fn (ai/expectiminimax-graph true ai/evaluate-sum-distance)}])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if args
    (time (let [n (Integer/parseInt (nth args 2))
                a1 (nth agents (Integer/parseInt (nth args 0)))
                a2 (nth agents (Integer/parseInt (nth args 1)))]
            (prn (:ai/name a1) (:ai/name a2) n)
            (println (.. Runtime getRuntime availableProcessors) "available processors")
            (->> (repeat n initial-state)
                 (pmap #(->> (game/run-game %
                                            {:player/white (:ai/fn a1)
                                             :player/black (:ai/fn a2)}
                                            nil)
                             last
                             second
                             game/utility))
                 (doall)
                 (reduce + 0)
                 println)
            (shutdown-agents)))
    (do
      (ui/present-agents agents)
      (let [player1 (ui/get-choice "Select white player" agents)
            player2 (ui/get-choice "Select black player" agents)
            [winning-move final-board] (last
                                        (game/run-game initial-state
                                                       {:player/white (:ai/fn player1)
                                                        :player/black (:ai/fn player2)}
                                                       ui/print-board))]
        (println)
        (if (= 1 (game/utility final-board))
          (println "White player wins!")
          (println "Black player wins!"))))))
