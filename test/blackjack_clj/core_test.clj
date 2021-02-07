(ns blackjack-clj.core_test
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
      (is (= (game/count-cards state-no-aces :player 1) 15)
          "Counting a hand with King and 5 equals 15")
      (is (= (game/count-cards state-with-aces :player 1) 16)
          "Counting a hand with King and 5 and Ace (with 1 value) equals 16")
      (is (= (game/count-cards state-with-aces :player 11) 26)
          "Counting a hand with King and 5 and Ace (with 11 value) equals 26"))))

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


