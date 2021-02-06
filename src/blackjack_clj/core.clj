(ns blackjack-clj.core)

;; Define the data model
(def initial-state
  {:score       {:wins 0 :losses 0}
   :cards       (shuffle (for [type [:hearts :clubs :diamonds :spades]
                               number [\A 2 3 4 5 6 7 8 9 \J \Q \K]]
                           (hash-map :type type :number number)))
   :round       1
   :turn        :dealer
   :player-hand []
   :dealer-hand []})


(comment
 initial-state

  ,)