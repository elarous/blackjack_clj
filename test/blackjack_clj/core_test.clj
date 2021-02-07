(ns blackjack-clj.core_test
  (:require [blackjack-clj.core :as game]
            [clojure.test :refer :all]))

(testing "Initial state"
  (let [initial-state (game/initial-state)]
    (is (contains? initial-state :cards) "Has cards")
    (is (= (count (:cards initial-state)) 52) "Has 52 cards")))

