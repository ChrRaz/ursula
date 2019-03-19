(ns ursula.game-test
  (:require [ursula.game :refer :all]
            [clojure.test :refer :all]))



(deftest tile-order
  (testing "The order of tiles on the board"
    (are [player tile-seq] (= (->> :pre-board
                                   (iterate #(next-tile % player))
                                   (take 18))
                              tile-seq)
      :player/white '(:pre-board [:a 4] [:a 5] [:a 6] [:a 7] [:b 7] [:b 6] [:b 5] [:b 4] [:b 3] [:b 2] [:b 1] [:b 0] [:a 0] [:a 1] :post-board nil nil)
      :player/black '(:pre-board [:c 4] [:c 5] [:c 6] [:c 7] [:b 7] [:b 6] [:b 5] [:b 4] [:b 3] [:b 2] [:b 1] [:b 0] [:c 0] [:c 1] :post-board nil nil))))

(deftest nth-tile
  (testing "Stepping n tiles"
    (are [player start steps end] (= end (next-tile start player steps))
      :player/white :pre-board 0 :pre-board
      :player/black :pre-board 0 :pre-board
      :player/white :pre-board 1 [:a 4]
      :player/black :pre-board 1 [:c 4]
      :player/white :pre-board 7 [:b 5]
      :player/white :pre-board 7 [:b 5]
      :player/black :pre-board 15 :post-board
      :player/black :pre-board 15 :post-board
      )))
