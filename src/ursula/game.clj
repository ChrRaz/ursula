(ns ursula.game
  (:require [ursula.utils :as utils]))

;; Game definition

(def dice-chances
  {0 1/16, 1 4/16, 2 6/16, 3 4/16, 4 1/16})

(def flower-tile?
  #{[:a 1] [:c 1]
    [:b 4]
    [:a 7] [:c 7]})

(def board-tiles
  (->> (for [s [:a :b :c]
             l (range 8)]
         [s l])
       (remove #{[:a 2] [:a 3]
                 [:c 2] [:c 3]})))

(defn legal-move?
  "Is the move legal from the current state?
  A move is [from distance]."
  [[tile dist] player board]
  (let [target (utils/next-tile tile player dist)]
    (and
     ;; Player has a piece on the tile
     (= player (get board tile))
     ;; Not already occupied
     (not= player (get board target))
     ;; Not occupied on flower square
     (not (and (flower-tile? target)
               (get board target))))))

;; Game state functions

(defn player
  "Which player has the move in a state (includes chance nodes)"
  [s]
  (if (:game/dice s)
    (:game/turn s)
    :player/chance))

(defn actions
  "The set of legal moves in the state"
  [s]
  (let [p (player s)
        dice (:game/dice s)
        board (:game/board s)]
    (->> board-tiles
         ;; Construct move [from distance]
         (map #(vector % dice))
         ;; Only return legal moves
         (filter #(legal-move? % p board)))))

(defn result
  "The result of a move"
  [s a])

(defn terminal?
  "Is the game over?"
  [s]
  (or (-> s :game/post-board :player/white (= 7))
      (-> s :game/post-board :player/black (= 7))))

(defn utility
  "The utility of the state to player p"
  [s p]
  (let [o (utils/other-player p)]
    (cond
      (-> s :game/post-board (get p)) +1
      (-> s :game/post-board (get o)) -1)))
