(ns ursula.utils
  (:require [clojure.core.match :refer [match]]))

(def other-player
  {:player/white :player/black
   :player/black :player/white})

(def player-side
  {:player/white :a
   :player/black :c})

(defn next-tile
  ([tile player]
   (match tile
          nil             nil
          :pre-board      [(player-side player) 4]
          :post-board     nil
          [(:or :a :c) 7] [:b 7]
          [:b          0] [(player-side player) 0]
          [(:or :a :c) 1] :post-board
          [:a          l] [:a (inc l)]
          [:b          l] [:b (dec l)]
          [:c          l] [:c (inc l)]))
  ([tile player n]
   (nth (iterate #(next-tile % player) tile) n)))
