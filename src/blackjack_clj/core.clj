(ns blackjack-clj.core
  (:require [clojure.string :as s]))

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
                        number [\A 2 3 4 5 6 7 8 9 10 \J \Q \K]]
                    (hash-map :type type :number number :face-down? false))
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
  ([state contender]
   [(count-cards state contender 1)
    (count-cards state contender 11)])
  ([state contender ace-as]
   (let [cards (get state contender)]
     (->> cards
          (map #(case (:number %)
                  (\J \Q \K) 10
                  \A (if (= ace-as 11) 11 1)
                  (:number %)))
          (reduce +)))))

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
      (add-win new-state)
      new-state)))

(defn soft-17? [state contender]
  (let [cards (get state contender)]
    (= (set (map :number cards)) #{\A 6})))

(defn lose-if-bust
  "
  Check to run after the player takes an action.
  If gone bust add a loss to the score.
  "
  [state]
  (if (bust? state :player)
    (add-loss state)
    state))

(defn comp-max
  "Compare the maximum values for vectors `values1` and `values2` using fn `comp-fn`"
  [comp-fn values1 values2]
  (let [v1 (if (seq values1) (apply max values1) 0)
        v2 (if (seq values2) (apply max values2) 0)]
    (comp-fn v1 v2)))

(defn dealer-check
  [state]
  (let [counter (partial count-cards state)
        dealer-counts (->> (counter :dealer) (filter (partial >= 21)) vec)
        player-counts (->> (counter :player) (filter (partial >= 21)) vec)]
    (cond
      ;; check all possible combinations for cards values that might contains aces
      (comp-max = dealer-counts player-counts)
      (set-draw state)                                      ;; a draw, start a new round
      (bust? state :dealer)
      (add-win state)                                       ;; dealer's bust, add a new win to player, and start new round
      ;; cards' value of the dealer is less than 17, or he has a soft 17 then he must `hit`
      ;; make a recursive call to this same function to run the checks again
      (or (some (partial > 17) dealer-counts) (soft-17? state :dealer))
      (-> state (hit :dealer) dealer-check)
      ;; compare the dealer's and player's card values
      (some (partial <= 17) dealer-counts)
      (cond
        (comp-max > player-counts dealer-counts) (add-win state)
        (comp-max < player-counts dealer-counts) (add-loss state)))))

;; display functions
(defn prn-heading! []
  (let [msg (format "\n\n\t\t----------ROUND (%s) WINS (%d) LOSSES (%d)----------\n\n"
                    (:round @app-state)
                    (:wins @app-state)
                    (:losses @app-state))]
    (println msg)
    (flush)))

(defn prn-cards! []
  (let [cards-str (fn [cards]
                    (->> cards
                         (map (fn [card]
                                (if (:face-down? card)
                                  (str "(XXXX)")
                                  (str "(" (:number card) " of " (-> (:type card) name s/capitalize) ")"))))
                         (interpose " ")
                         (reduce str)))
        msg (format "\n\tDealer Cards: %s\n\tPlayer Cards: %s\n"
                    (cards-str (:dealer @app-state))
                    (cards-str (:player @app-state)))]
    (println msg)
    (flush)))


(defn player-win! []
  (println "\n\t\t\t********YOU WIN!********")
  (flush))

(defn player-loss! []
  (println "\n\t\t\t********YOU LOSE!********")
  (flush))

(defn draw! []
  (println "\n\t\t\t********DRAW!********")
  (flush))

(defn prn-status! []
  (cond
    (:has-won? @app-state) (player-win!)
    (:has-lost? @app-state) (player-loss!)
    (:draw? @app-state) (draw!)))

(defn get-action!
  "Get the action to do from the player (Hit or Stand)"
  []
  (let [msg "What do you want to do? Hit (h) or Stand (s): "]
    (loop [answer ""]
      (when-not (#{"h" "s"} answer)
        (print msg))
      (flush)
      (case answer
        "h" :hit
        "s" :stand
        (recur (-> (read-line) s/trim))))))

(declare player-action!)
(declare dealer-action!)

(defn player-hit! []
  (swap! app-state #(hit % :player))
  (prn-cards!)
  (swap! app-state lose-if-bust)
  (if (:has-lost? @app-state)
    (prn-status!)
    (player-action!)))

(defn player-stand! []
  (dealer-action!))

(defn player-action! []
  (let [action (get-action!)]
    (case action
      :hit (player-hit!)
      :stand (player-stand!))))

(defn dealer-action
  "The actions the dealer take after the initial deal"
  [state]
  (-> (face-up-cards state :dealer)
      (dealer-check)))

(defn dealer-action! []
  (swap! app-state dealer-action)
  (prn-status!)
  (prn-cards!))

(defn play-round! []
  (prn-heading!)
  (swap! app-state initial-deal)
  (prn-cards!)
  (prn-status!)
  (when-not (:has-won? @app-state)
    (player-action!)))

(defn -main [& args]
  (reset! app-state (initial-state))
  (while true
    (swap! app-state new-round)
    (play-round!)))
