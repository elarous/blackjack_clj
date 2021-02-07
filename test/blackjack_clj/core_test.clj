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

