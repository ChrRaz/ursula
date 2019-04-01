(ns ursula.ai
  (:require [ursula.game :as game]
            [ursula.ui :as ui]
            [ursula.utils :as utils]))

(defn random
  "Returns a random action from the state s"
  [print?]
  (fn [s]
    (if print?
      (ui/print-turn-info s "Random"))
    (-> s
        game/actions
        rand-nth)))

(defn greedy
  "Returns an action from the state s based on the heuristic"
  [print? eval-fn]
  (fn [s]
    (if print?
      (ui/print-turn-info s "Greedy"))
    (case (game/player s)
      :player/white
      (->> s
           game/actions
           (reduce (fn [acc new] (max-key #(eval-fn (game/result s %)) acc new))))
      :player/black
      (->> s
           game/actions
           (reduce (fn [acc new] (min-key #(eval-fn (game/result s %)) acc new)))))))

(defn expectiminimax-cutoff
  "Returns the optimal action from the state s"
  [print? cutoff eval-fn]
  (fn [s]
    (if print?
      (ui/print-turn-info s "Minimax"))
    (let [times-run (atom 0)]
      (letfn [(step [s prob]
                (swap! times-run inc)
                (if (or (game/terminal? s)
                        (< prob cutoff))
                  (eval-fn s)
                  (case (game/player s)
                    ;; Maximize utility as white
                    :player/white
                    (->> (game/actions s)
                         (map #(step (game/result s %) prob))
                         (reduce max))
                    ;; Minimize utility as black
                    :player/black
                    (->> (game/actions s)
                         (map #(step (game/result s %) prob))
                         (reduce min))

                    ;; Chance nodes
                    :player/chance
                    (reduce
                     (fn [acc [new-state probability]]
                       (+ acc
                          (* probability
                             (step new-state (* prob probability)))))
                     0
                     (utils/rolled-states s game/dice-chances)))))]
        (let [result (case (game/player s)
                       :player/white
                       (->> (game/actions s)
                            (map (fn [action]
                                   [action (step (game/result s action) 1)]))
                            (reduce #(max-key second %1 %2))
                            first)
                       :player/black
                       (->> (game/actions s)
                            (map (fn [action]
                                   [action (step (game/result s action) 1)]))
                            (reduce #(min-key second %1 %2))
                            first))]
          (if print?
            (println "Explored" @times-run "nodes"))
          result)))))

(defn expectiminimax-graph
  "Returns the optimal action from the state s
  (Does not work. Overflows.)"
  [print? eval-fn]
  (fn [s]
    (if print?
      (ui/print-turn-info s "Minimax"))
    (letfn [(step [s prob visited?]
              (if (or (game/terminal? s)
                      (visited? s))
                (eval-fn s)
                (case (game/player s)
                  ;; Maximize utility as white
                  :player/white
                  (->> (game/actions s)
                       (map #(step (game/result s %) prob (conj visited? s)))
                       (reduce max))
                  ;; Minimize utility as black
                  :player/black
                  (->> (game/actions s)
                       (map #(step (game/result s %) prob (conj visited? s)))
                       (reduce min))

                  ;; Chance nodes
                  :player/chance
                  (reduce
                   (fn [acc [new-state probability]]
                     (+ acc
                        (* probability
                           (step new-state (* prob probability) (conj visited? s)))))
                   0
                   (utils/rolled-states s game/dice-chances)))))]
      (case (game/player s)
        :player/white
        (->> (game/actions s)
             (map (fn [action]
                    [action (step (game/result s action) 1 #{s})]))
             (reduce #(max-key second %1 %2))
             first)
        :player/black
        (->> (game/actions s)
             (map (fn [action]
                    [action (step (game/result s action) 1 #{s})]))
             (reduce #(min-key second %1 %2))
             first)))))

(defn evaluate-sum-distance
  [s]
  (- (+ (* 15 (-> s :game/pre-board :player/black))
        (->> (:game/board s)
             (keep (fn [[tile p]]
                     (if (= p :player/black)
                       tile)))
             (map game/distance-to-goal)
             (reduce +)))
     (+ (* 15 (-> s :game/pre-board :player/white))
        (->> (:game/board s)
             (keep (fn [[tile p]]
                     (if (= p :player/white)
                       tile)))
             (map game/distance-to-goal)
             (reduce + 0)))))

(defn evaluate-monte-carlo
  [s]
  (let [[winning-move final-board]
        (last
         (game/run-game s
                        {:player/white (random false)
                         :player/black (random false)}
                        nil))]
    (game/utility final-board)))
