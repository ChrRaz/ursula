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

;; Game state functions

(defn player
  "Which player has the move in a state (includes chance nodes)"
  [s]
  (if (:game/dice s)
    (:game/turn s)
    :player/chance))

(defn legal-move?
  "Is the move legal from the current state?
  A move is [from distance]."
  [[tile target] s]
  (let [p (player s)
        board (:game/board s)]
    (and
     ;; Player has a piece on the tile
     (if (= tile :pre-board)
       (pos? (-> s :game/pre-board p))
       (= p (get board tile)))
     ;; Not already occupied
     (not= p (get board target))
     ;; Not occupied on flower square
     (not (and (flower-tile? target)
               (get board target)))
     ;; Cannot overshoot post-game
     target)))

(defn actions
  "The set of legal moves in the state"
  [s]
  (let [p (player s)
        dice (:game/dice s)
        board (:game/board s)]
    (if (= dice 0)
      '([nil nil])
      (->> board-tiles
           ;; Construct move [from distance]
           (map #(vector % (utils/next-tile % p dice)))
           ;; Only return legal moves
           (filter #(legal-move? % s))))))

(defn result
  "The result of a move"
  [s a]
  (let [p (player s)
        [from dist] a]
    (if (= dist 0)
      (update s :game/turn utils/other-player)
      #_(cond-> s
        (= from :pre-board)
        (update-in [:game/pre-board player] dec)
        (= from :pre-board)
        (update-in [:game/pre-board player] dec)

        (update :game/board dissoc from)
          ))))

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
