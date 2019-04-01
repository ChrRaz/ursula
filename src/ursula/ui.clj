(ns ursula.ui
  (:require [clojure.string :as str]
            [ursula.game :as game]
            [ursula.utils :as utils]))

(def player-names
  {:player/white "W"
   :player/black "B"})

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

(defn tile->string
  [tile s]
  (let [p (-> s :game/board (get tile))]
    (cond
      p (player-names p)
      (game/flower-tile? tile) "*"
      :else " ")))

(defn board->string
  [s]
  (apply format
   (str/join \newline
             ["  0   1   2   3   4   5   6   7    "
              "+---+---+       +---+---+---+---+  "
              "| %s | %s |       | %s | %s | %s | %s | C"
              "+---+---+---+---+---+---+---+---+  "
              "| %s | %s | %s | %s | %s | %s | %s | %s | B"
              "+---+---+---+---+---+---+---+---+  "
              "| %s | %s |       | %s | %s | %s | %s | A"
              "+---+---+       +---+---+---+---+  "])
   (->> game/board-tiles
        (map #(tile->string % s)))))

(defn board-info
  [s]
  (str/join \newline
            [(format "  Black: %d/7" (-> s :game/post-board :player/black))
             (format "  White: %d/7" (-> s :game/post-board :player/white))
             " "
             " "
             " "
             " "
             " "
             " "
             ]))

(defn get-choice
  [prompt options]
  (loop [choice -1]
    (if (< -1 choice (count options))
      (nth options choice)
      (recur (do (print (str prompt ": "))
                 (flush)
                 (try
                   (Integer/parseInt (read-line))
                   (catch NumberFormatException e
                     -1)))))))

(defn print-turn-info
  [s player]
  (println)
  (println "Playing as" (game/player s))
  (println player "rolled a" (:game/dice s)))

(defn user-input
  [s]
  (print-turn-info s "You")
  (let [actions (sort-by #(game/distance-to-goal (first %)) > (game/actions s))]
    (println "Actions:")
    (dorun
     (map-indexed (fn [idx action]
                    (println (str "  " idx ": " (action->string action))))
                  actions))
    (get-choice "Select an action" actions)))

(defn present-agents
  [agents]
  (println "Implemented agents:")
  (dorun
   (map-indexed (fn [idx a]
                  (println (str "  " idx ": " (:ai/name a))))
                agents)))
