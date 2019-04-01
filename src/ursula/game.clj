(ns ursula.game
  (:require [clojure.core.match :refer [match]]
            [ursula.utils :as utils :refer [dissoc-in]]))

;; Game definition

(def dice-chances
  {0 1/16, 1 4/16, 2 6/16, 3 4/16, 4 1/16})

(def flower-tile?
  #{[:a 1] [:c 1]
    [:b 4]
    [:a 7] [:c 7]})

(def board-tiles
  (->> (for [s [:c :b :a]
             l (range 8)]
         [s l])
       (remove #{[:a 2] [:a 3]
                 [:c 2] [:c 3]})))

(defn next-tile
  ([tile player]
   (match tile
          nil             nil
          :pre-board      [(utils/player-side player) 4]
          :post-board     nil
          [(:or :a :c) 7] [:b 7]
          [:b          0] [(utils/player-side player) 0]
          [(:or :a :c) 1] :post-board
          [:a          l] [:a (inc l)]
          [:b          l] [:b (dec l)]
          [:c          l] [:c (inc l)]))
  ([tile player n]
   (nth (iterate #(next-tile % player) tile) n)))

(def distance-to-goal
  (->> board-tiles
       (cons :pre-board)
       (map (fn [tile]
              [tile (count (take-while #(not= % :post-board)
                                       (iterate #(next-tile % :player/white) tile)))]))
       (into {})))


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
      ;; We can't move
      '([nil nil])
      ;; We must try to move
      (let [acts (->> board-tiles
                      (cons :pre-board)
                      ;; Construct move [from distance]
                      (map #(vector % (next-tile % p dice)))
                      ;; Only return legal moves
                      (filter #(legal-move? % s)))]
        ;; If there are no legal moves, then do nothing
        (if (seq acts)
          acts
          '([nil nil]))))))

(defn result
  "The result of a move"
  [s a]
  (let [[from to] a
        p (player s)]
    (if (= from to)
      (-> s
          (update :game/turn utils/other-player)
          (dissoc :game/dice))
      (as-> s s
        (if (= from :pre-board)
          (update-in s [:game/pre-board p] dec)
          (dissoc-in s [:game/board from]))
        (if (= (-> s :game/board (get to))
               (utils/other-player p))
          (update-in s [:game/pre-board (utils/other-player p)] inc)
          s)
        (if (= to :post-board)
          (update-in s [:game/post-board p] inc)
          (assoc-in s [:game/board to] p))
        (if (flower-tile? to)
          (identity s)
          (update s :game/turn utils/other-player))
        (dissoc s :game/dice)))))

(defn terminal?
  "Is the game over?"
  [s]
  (or (-> s :game/post-board :player/white (= 7))
      (-> s :game/post-board :player/black (= 7))))

(defn utility
  "The utility of the state to player white.
  1 if white has won. -1 if black has won."
  [s]
  (if (-> s :game/post-board :player/white (= 7))
    +1
    -1))

(defn run-turn
  [[_ state] players]
  (let [player (get players (:game/turn state))
        rolled-state (utils/roll-dice state dice-chances)
        action (player rolled-state)]
    ;; TODO: good response instead
    #_(prn
       (:game/turn rolled-state)
       (:game/dice rolled-state)
       action)
    [action
     (result rolled-state action)]))

(defn run-game
  [initial players]
  (->> [:state/initial initial]
       (iterate (fn [state]
                  (run-turn state players)))
       (utils/take-upto (fn [[_ state]]
                          (terminal? state)))))

