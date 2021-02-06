(ns blackjack-clj.core)

;; Small helper to create queues
(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; Define the data model
(def initial-state
  {:score       {:wins 0 :losses 0}
   :cards       (-> (for [type [:hearts :clubs :diamonds :spades]
                          number [\A 2 3 4 5 6 7 8 9 \J \Q \K]
                          face-down? [false]]
                      (hash-map :type type :number number :face-down? face-down?))
                    shuffle
                    queue)
   :round       1
   :turn        :dealer
   :player-hand []
   :dealer-hand []})

(defn deal
  "Deal a card to a contender, optionally the card can be dealt face-down"
  ([state contender]
   (deal state contender false))
  ([state contender face-down?]
   (let [card (-> state :cards peek (assoc :face-down? face-down?))
         deal-helper (fn [state contender-hand]
                       (-> state
                           (update :cards pop)
                           (update contender-hand conj card)))]
     (case contender
       :player (deal-helper state :player-hand)
       :dealer (deal-helper state :dealer-hand)
       (throw (Error. (str "Only the dealer or player can be a contender. (got: " contender ")")))))))

(defn count-cards
  "Count the cards' values in the hand of a contender
   `ace-as` should be either `11` or `1`"
  [state contender ace-as]
  (let [cards (case contender
                :player (get state :player-hand)
                :dealer (get state :dealer-hand))]
    (->> cards
         (map #(case (:number %)
                 (\J \Q \K) 10
                 \A (if (= ace-as 11) 11 1)
                 (:number %)))
         (reduce +))))


(comment
  initial-state
  (count-cards initial-state :player 11)

  (-> initial-state
      (deal :player true)
      (deal :player)
      (count-cards :player 11)))

