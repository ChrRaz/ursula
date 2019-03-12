(ns ursula.core
  (:gen-class))

(def initial-state
  {:game/turn :player/white
   :game/dice nil
   :game/pre-board {:player/white 7
                    :player/black 7}
   :game/board {}
   :game/post-board {:player/white 0
                     :player/black 0}})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
