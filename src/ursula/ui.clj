(ns ursula.ui
  (:require [ursula.game :as game]))

(defn position->string
  [tile]
  (cond
    (= tile :pre-board) "  "
    (= tile :post-board) "  "
    :else (str (name (first tile)) (second tile))))

(defn action->string
  [[from to]]
  (if (= [nil nil] [from to])
    "Pass the turn"
    (str (position->string from) " -> " (position->string to))))

(defn user-input
  [s]
  (println "Playing as" (game/player s))
  (println "You rolled a" (:game/dice s))
  (let [actions (game/actions s)]
    (dorun
     (map-indexed (fn [idx action]
                    (println (str idx ": " (action->string action)))) actions))
    (loop [choice -1]
      (if (< -1 choice (count actions))
        (nth actions choice)
        (recur (do (println "Select an action")
                   (Integer/parseInt (read-line))))))))
