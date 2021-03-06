(ns blackjack-clj.core-test
  (:require [blackjack-clj.core :as game]
            [clojure.test :refer :all]
            [clojure.set :refer [intersection]]))

(deftest initial-state
  (testing "Initial state"
    (let [initial-state (game/initial-state)]
      (is (contains? initial-state :cards) "Has cards")
      (is (= (count (:cards initial-state)) 52) "Has 52 cards"))))

(deftest deal
  (let [state-before (game/initial-state)
        contender :player
        cards-before (get state-before contender)]
    (testing "Dealing cards"
      (let [state-after (game/deal state-before contender)
            cards-after (get state-after contender)]
        (is (= (-> cards-after count) (-> cards-before count inc))
            "Contender cards count increase by 1")
        (let [cards-in-deck (map #(dissoc % :face-down?) (:cards state-after))
              cards-in-hand (map #(dissoc % :face-down?) (get state-after contender))]
          (is (empty? (intersection (set cards-in-deck)
                                    (set cards-in-hand)))
              "Cards can't be on deck and hands at the same time"))
        (let [player-cards (->> (get state-after :player) (map #(dissoc % :face-down?)) set)
              dealer-cards (->> (get state-after :dealer) (map #(dissoc % :face-down?)) set)]
          (is (empty? (intersection player-cards dealer-cards))
              "Contenders shouldn't have the same card"))))
    (testing "Dealing face down cards"
      (let [state-after (game/deal state-before contender true)
            card (-> state-after contender last)]
        (is (get card :face-down?)
            "Card should be face down")))))

(deftest count-cards
  (let [card-A {:type :hearts :number \A :face-down? false}
        card-K {:type :spades :number \K :face-down? false}
        card-5 {:type :hearts :number 5 :face-down? false}
        state-no-aces (merge (game/initial-state)
                             {:player [card-K card-5]})
        state-with-aces (merge (game/initial-state)
                               {:player [card-K card-5 card-A]})]
    (testing "Counting cards"
      (is (= (game/count-cards state-no-aces :player) 15)
          "Counting a hand with King and 5 equals 15")
      (is (= (game/count-cards state-with-aces :player) 16)
          "Counting a hand with King and 5 and Ace equals 16"))))

(deftest blackjack?
  (let [card-A {:type :hearts :number \A :face-down? false}
        card-K {:type :spades :number \K :face-down? false}
        card-10 {:type :hearts :number 10 :face-down? false}
        state-blackjack-11 (merge (game/initial-state) {:player [card-A card-K]})
        state-blackjack-1 (merge (game/initial-state) {:player [card-A card-K card-10]})
        state-no-blackjack (merge (game/initial-state) {:player [card-K card-10]})]
    (testing "Contender has a blackjack"
      (is (game/blackjack? state-blackjack-11 :player)
          "Detecting blackjack when ace is equal to 11 and total is 21")
      (is (game/blackjack? state-blackjack-1 :player)
          "Detecting blackjack when ace is equal to 1 and total is 21")
      (is (not (game/blackjack? state-no-blackjack :player))
          "Not detecting blackjack when total is not 21 "))))

(deftest bust?
  (let [card-Q {:type :hearts :number \Q :face-down? false}
        card-K {:type :spades :number \K :face-down? false}
        card-2 {:type :hearts :number 2 :face-down? false}
        state-bust (merge (game/initial-state) {:player [card-Q card-K card-2]})
        state-no-bust (merge (game/initial-state) {:player [card-Q card-K]})]
    (testing "Contender has gone bust"
      (is (game/bust? state-bust :player)
          "Detecting bust when card values exceed 21")
      (is (not (game/bust? state-no-bust :player))
          "Not detecting bust when card values are less than 21"))))

(deftest face-up-cards
  (let [card-2 {:type :hearts :number 2 :face-down? true}
        card-3 {:type :hearts :number 3 :face-down? true}
        state-before (merge (game/initial-state)
                            {:dealer [card-2 card-3]})
        state-after (game/face-up-cards state-before :dealer)]
    (testing "Flipping cards to be face up"
      (is (= (->> (get state-after :dealer) (map :face-down?) set)
             #{false})
          "All cards are flipped to face up"))))

(deftest add-win
  (let [state (-> (game/initial-state)
                  (game/add-win))]
    (is (:has-won? state) "Player has won flag is set to true")
    (is (= (:wins state) 1) "Player wins count is incremented")))

(deftest add-loss
  (let [state (-> (game/initial-state)
                  (game/add-loss))]
    (is (:has-lost? state) "Player has lost flag is set to true")
    (is (= (:losses state) 1) "Player wins count is incremented")))

(deftest set-draw
  (let [state (-> (game/initial-state)
                  (game/set-draw))]
    (is (not (:has-lost? state)) "Player has lost flag wasn't set")
    (is (not (:has-won? state)) "Player has won flag wasn't set")
    (is (:draw? state) "Draw flag was set")))

(deftest new-round
  (let [state-before (-> (game/initial-state)
                         (game/add-win)
                         (game/add-loss)
                         (game/deal :player)
                         (game/deal :dealer))
        state-after (game/new-round state-before)]
    (is (= (select-keys state-before [:wins :losses])
           (select-keys state-after [:wins :losses]))
        "Preserves data that is not related to a particular round")
    (is (not= (select-keys state-before [:cards :player :dealer :has-won? :has-lost? :draw?])
              (select-keys state-after [:cards :player :dealer :has-won? :has-lost? :draw?])))
    (is (= (:round state-after) (inc (:round state-before)))
        "Increments the round counter by 1")))

(deftest initial-deal
  (let [state (game/initial-deal (game/initial-state))
        card-Q {:type :hearts :number \Q :face-down? false}
        card-K {:type :spades :number \K :face-down? false}
        card-2 {:type :hearts :number 2 :face-down? false}
        card-3 {:type :hearts :number 3 :face-down? false}
        card-A {:type :hearts :number \A :face-down? false}
        ;; A state with cards arranged so that the player wins (blackjack) on the initial deal
        state-win (-> (merge (game/initial-state) {:cards [card-2 card-Q card-K card-A]})
                      (game/initial-deal))
        state-no-win (-> (merge (game/initial-state) {:cards [card-2 card-Q card-K card-3]})
                         (game/initial-deal))]
    (is (= 2 (-> state :player count) (-> state :dealer count))
        "Deals 2 cards for each contender")
    (is (-> state :dealer last :face-down?)
        "The second card dealt to dealer is face down")
    (is (and (:has-won? state-win) (= (:wins state-win) 1))
        "Player wins when he get a blackjack")
    (is (and (not (:has-won? state-no-win)) (zero? (:wins state-no-win)))
        "Player doesn't win when he doesn't get a blackjack")))

(deftest soft-17?
  (let [card-2 {:type :hearts :number 2 :face-down? false}
        card-6 {:type :hearts :number 6 :face-down? false}
        card-A {:type :hearts :number \A :face-down? false}
        state-soft-17 (merge (game/initial-state)
                             {:player [card-6 card-A]})
        state-not-soft-17 (merge (game/initial-state)
                                 {:player [card-2 card-A]})]
    (is (game/soft-17? state-soft-17 :player)
        "Returns true when player has 2 cards the Ace and the 6")
    (is (not (game/soft-17? state-not-soft-17 :player))
        "Returns false when player doesn't have the 2 cards Ace and 6")))

(deftest lose-if-bust
  (let [card-K {:type :hearts :number \K :face-down? false}
        card-Q {:type :hearts :number \Q :face-down? false}
        card-2 {:type :hearts :number 2 :face-down? false}
        bust-cards [card-K card-Q card-2]
        no-bust-cards [card-K card-Q]
        bust-state (-> (merge (game/initial-state) {:player bust-cards})
                       (game/lose-if-bust))
        no-bust-state (-> (merge (game/initial-state) {:player no-bust-cards})
                          (game/lose-if-bust))]
    (is (and (:has-lost? bust-state) (= (:losses bust-state) 1))
        "Add a loss when user's gone bust")
    (is (and (not (:has-lost? no-bust-state)) (zero? (:losses no-bust-state)))
        "Doesn't add a loss when the user hasn't gone bust")))

(deftest dealer-check
  (let [card-A {:type :hearts :number \A :face-down? false}
        card-K {:type :hearts :number \K :face-down? false}
        card-4 {:type :hearts :number 4 :face-down? false}
        card-3 {:type :hearts :number 3 :face-down? false}
        card-6 {:type :hearts :number 6 :face-down? false}
        card-7 {:type :hearts :number 7 :face-down? false}
        card-8 {:type :hearts :number 8 :face-down? false}
        card-2 {:type :hearts :number 2 :face-down? false}]
    (testing "When there is a draw"
      (let [dealer-cards [card-K card-2 card-6]
            player-cards [card-4 card-8 card-6]
            dealer-cards-ace [card-4 card-A card-K card-3]
            player-cards-ace [card-A card-2 card-2 card-K card-3]
            state-no-ace (-> (merge (game/initial-state)
                                    {:player player-cards :dealer dealer-cards})
                             (game/dealer-check))
            state-with-ace (-> (merge (game/initial-state)
                                      {:player player-cards-ace
                                       :dealer dealer-cards-ace})
                               (game/dealer-check))]
        (is (:draw? state-no-ace)
            "Sets the draw flag when card values are equal for contenders")
        (is (:draw? state-with-ace)
            "Sets the draw flag when card values are equal and contain aces")))
    (testing "When dealer needs to hit again"
      (testing "When dealer got one or zero Aces"
        (let [dealer-cards-less-17 [card-2 card-4]
              dealer-cards-soft-17 [card-A card-6]
              player-cards [card-4]
              initial-state (game/initial-state)
              state-less-17 (-> (merge initial-state
                                       {:dealer dealer-cards-less-17
                                        :player player-cards})
                                (game/dealer-check))
              state-soft-17 (-> (merge initial-state
                                       {:dealer dealer-cards-soft-17
                                        :player player-cards})
                                (game/dealer-check))]
          (is (> (-> state-less-17 :dealer count) (-> initial-state :dealer count))
              "Dealer hits (one or many times) so he has more cards in hand")
          (is (> (-> state-soft-17 :dealer count) (-> initial-state :dealer count))
              "Dealer hits (one or many times) so he has more cards in hand")))
      (testing "When dealer got more than one Aces"
        (let [dealer-cards [card-A card-3]
              player-cards [card-K card-2]
              cards [card-6 card-7 card-A]
              state (merge (game/initial-state)
                           {:player player-cards :dealer dealer-cards :cards cards})
              new-state (game/dealer-check state)]
          (is (> (count (:dealer new-state))
                 (count (:dealer state)))
              "When dealer gets 2 aces he needs to hit (if value is less than 17)")
          (is (= (and (:has-lost? new-state) (= (:losses new-state) 1)))
              "The player loses"))))

    (testing "When dealer or player wins / loses"
      (let [big-hand [card-A card-K card-8]                 ;; 19 not 29
            small-hand [card-K card-4 card-3]               ;; 14
            initial-state (game/initial-state)
            state-player-win (-> (merge initial-state
                                        {:player big-hand
                                         :dealer small-hand})
                                 (game/dealer-check))
            state-player-lose (-> (merge initial-state
                                         {:player small-hand
                                          :dealer big-hand})
                                  (game/dealer-check))]
        (is (and (:has-won? state-player-win) (= (:wins state-player-win) 1))
            "The player wins when he has a larger card values")
        (is (and (:has-lost? state-player-lose) (= (:losses state-player-lose) 1))
            "The player loses when he has a smaller card values")))))

