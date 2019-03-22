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

(defn expectiminimax-cutoff
  "Returns the optimal action from the state s"
  [print? cutoff]
  (fn [s]
    (if print?
      (ui/print-turn-info s "Minimax"))
    (letfn [(step [s prob]
              (if (or (game/terminal? s)
                      (< prob cutoff))
                (game/evaluate s)
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
      (case (game/player s)
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
             first)))))

(defn expectiminimax
  "Returns the optimal action from the state s"
  [s]
  (if (game/terminal? s)
    (game/utility s)
    (case (game/player s)
      ;; Maximize utility as white
      :player/white
      (->> (game/actions s)
           (map #(expectiminimax (game/result s %)))
           (reduce max))
      ;; Minimize utility as black
      :player/black
      (->> (game/actions s)
           (map #(expectiminimax (game/result s %)))
           (reduce min))

      ;; Chance nodes
      :player/chance
      (reduce
       (fn [acc [new-state probability]]
         (+ acc
            (* probability
               (expectiminimax new-state))))
       0
       (utils/rolled-states s game/dice-chances)))))

#_(let [s ursula.core/initial-state]
    (map #(result (first %) (ai/random (first %)))
         (roll-dice s)))
