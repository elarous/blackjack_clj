(ns blackjack-clj.core)

;; Small helper to create queues
(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; Define the data model
(def initial-state
  {:score  {:wins 0 :losses 0}
   :cards  (-> (for [type [:hearts :clubs :diamonds :spades]
                     number [\A 2 3 4 5 6 7 8 9 \J \Q \K]
                     face-down? [false]]
                 (hash-map :type type :number number :face-down? face-down?))
               shuffle
               queue)
   :round  1
   :turn   :dealer
   :player []
   :dealer []})

(defn deal
  "Deal a card to a contender, optionally the card can be dealt face-down"
  ([state contender]
   (deal state contender false))
  ([state contender face-down?]
   (let [card (-> state :cards peek (assoc :face-down? face-down?))
         deal-helper (fn [state contender]
                       (-> state
                           (update :cards pop)
                           (update contender conj card)))]
     (deal-helper state contender))))

(defn count-cards
  "Count the cards' values in the hand of a contender
   `ace-as` should be either `11` or `1`"
  [state contender ace-as]
  (let [cards (get state contender)]
    (->> cards
         (map #(case (:number %)
                 (\J \Q \K) 10
                 \A (if (= ace-as 11) 11 1)
                 (:number %)))
         (reduce +))))

(defn blackjack?
  "Check if the contender has a blackjack, try for both Ace values: 1 & 11"
  [state contender]
  (let [counter (partial count-cards state contender)]
    (or (= 21 (counter 11))
        (= 21 (counter 1)))))

(defn bust?
  "Check if the contender went bust (Cards' values exceed 21)"
  [state contender]
  (let [counter (partial count-cards state contender)]
    (and (> (counter 11) 21)
         (> (counter 1) 21))))

(defn face-up-cards
  "Flip all contender's cards to be face-up"
  [state contender]
  (update state contender #(mapv (fn [card] (assoc card :face-down? false)) %)))

(defn add-win [state] (update-in state [:score :wins] inc))

(defn add-loss [state] (update-in state [:score :losses] inc))

(defn stand
  "Give the turn to the other contender"
  [state]
  (update state :turn #(case % :player :dealer :dealer :player)))

(comment
  )

