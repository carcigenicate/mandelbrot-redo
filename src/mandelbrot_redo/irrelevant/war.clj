(ns mandelbrot-redo.irrelevant.war
  (:require  [clojure.test :refer :all]))

(defrecord Card [rank suit])

(def suits [::spades ::clubs ::diamonds ::hearts])

(def suit-value (->> suits
                     (map #(vector %2 %) (range))
                     (into {})))

(def cards
  (for [suit suits
        rank (range 2 15)]
    (->Card rank suit)))

(defn suit-greater? [card1 card2]
  (->> [card1 card2]
       (map (comp suit-value :suit))
       (apply >)))

(defn rank-greater? [card1 card2]
  (->> [card1 card2]
       (map :rank)
       (apply >)))

(defn ranks-equal? [card1 card2]
  (->> [card1 card2]
       (map :rank)
       (apply =)))

(defn card-wins? [card1 card2]
  (if (ranks-equal? card1 card2)
    (suit-greater? card1 card2)
    (rank-greater? card1 card2)))

(defn sub-divide
  "Returns a pair of [first-half second-half] based on the given deck."
  [deck]
  (let [vdeck (vec deck)
        split-i (int (/ (count deck) 2))]
    [(subvec vdeck 0 split-i)
     (subvec vdeck split-i)]))

(defn play
  "Plays War until one deck runs out.
  Returns the cards remaining as [rest-p1-deck rest-p2-deck]."
  [deck]
  (let [[p1-cards p2-cards] (-> deck
                                (shuffle)
                                (sub-divide))]

    (loop [[p1-card & rest-p1] p1-cards
           [p2-card & rest-p2] p2-cards]

      (if (and p1-card p2-card)
        (let [[new-p1-card new-p2-cards] (if (card-wins? p1-card p2-card)
                                           [(conj (vec rest-p1) p1-card p2-card) rest-p2]
                                           [rest-p1 (conj (vec rest-p2) p2-card p1-card)])]

          (recur new-p1-card new-p2-cards))

        [rest-p1 rest-p2]))))

(deftest test-play-round
         (testing "the highest rank wins the cards in the round"
           (is (rank-greater? (->Card 6 ::hearts)
                              (->Card 2 ::hearts))))

         (testing "queens are higher rank than jacks"
           (is (rank-greater? (->Card 12 ::hearts)
                              (->Card 11 ::hearts))))

         (testing "kings are higher rank than queens"
           (is (rank-greater? (->Card 13 ::hearts)
                              (->Card 12 ::hearts))))

         (testing "aces are higher rank than kings"
           (is (rank-greater? (->Card 14 ::hearts)
                              (->Card 13 ::hearts))))

         (testing "if the ranks are equal, clubs beat spades"
           (is (card-wins? (->Card 12 ::clubs)
                           (->Card 12 ::spades))))

         (testing "if the ranks are equal, diamonds beat clubs"
           (is (card-wins? (->Card 12 ::diamonds)
                           (->Card 12 ::clubs))))

         (testing "if the ranks are equal, hearts beat diamonds"
           (is (card-wins? (->Card 12 ::hearts)
                           (->Card 12 ::diamonds)))))

(deftest test-play-game
         (testing "the player loses when they run out of cards"
           (is (->> cards
                 (play)
                 (some empty?)))))