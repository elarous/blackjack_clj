(ns blackjack-clj.core)

(def app-state (atom nil))

;; Small helper to create queues
(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; Define the data model
(defn initial-state
  "Get a fresh initial state"
  []
  {:wins      0
   :losses    0
   :has-won?  false
   :has-lost? false
   :draw?     false
   :cards     (-> (for [type [:hearts :clubs :diamonds :spades]
                        number [\A 2 3 4 5 6 7 8 9 \J \Q \K]
                        face-down? [false]]
                    (hash-map :type type :number number :face-down? face-down?))
                  shuffle
                  queue)
   :round     0
   :player    []
   :dealer    []})

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

(def hit deal)                                              ;; alias

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

(defn add-win [state]
  (-> state
      (update :wins inc)
      (assoc :has-won? true)))

(defn add-loss [state]
  (-> state
      (update :losses inc)
      (assoc :has-lost? true)))

(defn set-draw [state] (assoc state :draw? true))

(defn new-round
  "Start new round, but keep track of the wins and losses and the round number"
  [state]
  (let [preserved-data (-> (select-keys state [:wins :losses :round])
                           (update :round inc))]
    (merge (initial-state) preserved-data)))

(defn initial-deal
  "
  Dealer deals 1 card to the player and 1 card to himself
  If the player has a blackjack then he a win is added and a new round is started
  Otherwise the turn is switched to the player
  "
  [state]
  (let [new-state (-> state
                      (deal :player)
                      (deal :dealer)
                      (deal :player)
                      (deal :dealer true))]
    (if (blackjack? new-state :player)
      (-> new-state
          add-win
          new-round)
      (-> new-state
          (turn :player)))))

(defn soft-17? [state contender]
  (let [cards (get state contender)]
    (= (set (map :number cards)) #{\A \6})))

(defn post-check
  "
  Check to run after the player takes an action.
  Start a new round if player ahs gone busted
  "
  [state]
  (if (bust? state :player)
    (-> state add-loss new-round)
    state))

(defn dealer-check
  [state]
  (let [counter (partial count-cards state :dealer)
        cnt-dealer (counter :dealer)
        cnt-player (counter :player)]
    (cond
      (= cnt-dealer cnt-player) (-> state new-round)        ;; a draw, start a new round
      (bust? state :dealer) (-> state add-win new-round)    ;; dealer's bust, add a new win to player, and start new round
      ;; cards' value of the dealer is less than 17, or he has a soft 17 then he must `hit`
      ;; make a recursive call to this same function to run the checks again
      (or (< cnt-dealer 17) (soft-17? state :dealer)) (-> state (hit :dealer) dealer-check)
      ;; compare the dealer's and player's card values
      (>= cnt-dealer 17) (cond
                           (> cnt-player cnt-dealer) (-> state add-win new-round)
                           (< cnt-player cnt-dealer) (-> state add-loss new-round)))))


(comment
  (-> (initial-deal (initial-state))
      (hit :player)
      (post-check)
      (hit :player)
      (post-check)
      (hit :player)
      (post-check)
      (hit :player)))


