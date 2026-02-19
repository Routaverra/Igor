(ns igor.extensional-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]))

;; ============================================================
;; table tests
;; ============================================================

(deftest table-basic-test
  (testing "table constrains vars to one of the allowed tuples"
    (let [x1 (i/fresh-int (range 10))
          x2 (i/fresh-int (range 10))
          x3 (i/fresh-int (range 10))
          allowed [[1 2 3] [4 5 6] [7 8 9]]
          constraint (i/table [x1 x2 x3] allowed)
          solution (i/satisfy constraint)]
      (is (some? solution))
      (let [result [(get solution x1) (get solution x2) (get solution x3)]]
        (is (some #{result} allowed))))))

(deftest table-ground-passthrough-test
  (testing "table with ground vars returns boolean directly"
    (is (true? (i/table [1 2 3] [[1 2 3] [4 5 6]])))
    (is (false? (i/table [1 2 4] [[1 2 3] [4 5 6]])))))

;; ============================================================
;; regular tests
;; ============================================================

(deftest regular-basic-test
  (testing "regular constrains sequence to DFA-accepted language"
    ;; DFA: 2 states, alphabet {0 1}, accepts sequences ending in 1
    ;; state 0 = "last was 0 or start", state 1 = "last was 1"
    (let [dfa {:states      2
               :alphabet    #{0 1}
               :transitions [{0 0, 1 1}   ; state 0
                             {0 0, 1 1}]  ; state 1
               :start       0
               :accept      #{1}}
          vars (vec (repeatedly 4 #(i/fresh-int #{0 1})))
          solution (i/satisfy (i/regular vars dfa))]
      (is (some? solution))
      ;; last element must be 1
      (is (= 1 (get solution (last vars)))))))

(deftest regular-negative-alphabet-test
  (testing "regular with negative alphabet values (set-overload)"
    ;; DFA: 2 states, alphabet {-1 0 1}, accepts if sum of steps >= 1
    ;; state 0 = "not yet positive enough", state 1 = "positive enough"
    ;; Simplification: state 0 ->  {-1 0, 0 0, 1 1}, state 1 -> {-1 0, 0 1, 1 1}
    (let [dfa {:states      2
               :alphabet    #{-1 0 1}
               :transitions [{-1 0, 0 0, 1 1}
                             {-1 0, 0 1, 1 1}]
               :start       0
               :accept      #{1}}
          vars (vec (repeatedly 3 #(i/fresh-int #{-1 0 1})))
          solution (i/satisfy (i/regular vars dfa))]
      (is (some? solution))
      ;; must end in accepting state — at least one 1 that isn't cancelled
      (let [vals (mapv #(get solution %) vars)]
        (is (contains? #{1} (last (reduce (fn [state sym]
                                            (conj state
                                                  (get (nth (:transitions dfa) (peek state)) sym)))
                                          [0] vals))))))))

(deftest regular-ground-passthrough-test
  (testing "regular with ground sequence simulates DFA"
    (let [dfa {:states      2
               :alphabet    #{0 1}
               :transitions [{0 0, 1 1}
                             {0 0, 1 1}]
               :start       0
               :accept      #{1}}]
      (is (true? (i/regular [0 1 0 1] dfa)))
      (is (false? (i/regular [0 1 0 0] dfa)))
      (is (true? (i/regular [1] dfa)))
      (is (false? (i/regular [0] dfa))))))

;; ============================================================
;; cost_regular tests
;; ============================================================

(deftest cost-regular-basic-test
  (testing "cost_regular constrains sequence and binds cost"
    ;; DFA: 2 states, alphabet {1 2 3}, all transitions stay in state 0
    ;; costs: symbol 1 costs 0, symbol 2 costs 1, symbol 3 costs 2
    (let [dfa {:states      1
               :alphabet    #{1 2 3}
               :transitions [{1 0, 2 0, 3 0}]
               :start       0
               :accept      #{0}
               :costs       [{1 0, 2 1, 3 2}]}
          vars (vec (repeatedly 3 #(i/fresh-int #{1 2 3})))
          cost (i/fresh-int (range 0 10))
          solution (i/satisfy (i/cost-regular vars cost dfa))]
      (is (some? solution))
      (let [vals (mapv #(get solution %) vars)
            cost-val (get solution cost)
            expected-cost (reduce + (map {1 0, 2 1, 3 2} vals))]
        (is (= expected-cost cost-val))))))

(deftest cost-regular-minimize-test
  (testing "cost_regular with minimization finds optimal cost"
    ;; 1 state DFA, alphabet {1 2 3}, always accepts
    ;; costs: 1->5, 2->1, 3->3
    (let [dfa {:states      1
               :alphabet    #{1 2 3}
               :transitions [{1 0, 2 0, 3 0}]
               :start       0
               :accept      #{0}
               :costs       [{1 5, 2 1, 3 3}]}
          vars (vec (repeatedly 3 #(i/fresh-int #{1 2 3})))
          cost (i/fresh-int (range 0 100))
          ;; minimize cost by maximizing (- cost)
          solution (i/maximize (i/- 0 cost) (i/cost-regular vars cost dfa))]
      (is (some? solution))
      (let [cost-val (get solution cost)]
        ;; minimum cost is 3 * 1 = 3 (all symbols = 2)
        (is (= 3 cost-val))
        (doseq [v vars]
          (is (= 2 (get solution v))))))))

(deftest cost-regular-negative-alphabet-test
  (testing "cost_regular with negative alphabet values (requires remapping)"
    ;; 1 state, alphabet {-1 0 1}, always accepts
    ;; costs: -1->3, 0->1, 1->2
    (let [dfa {:states      1
               :alphabet    #{-1 0 1}
               :transitions [{-1 0, 0 0, 1 0}]
               :start       0
               :accept      #{0}
               :costs       [{-1 3, 0 1, 1 2}]}
          vars (vec (repeatedly 2 #(i/fresh-int #{-1 0 1})))
          cost (i/fresh-int (range 0 20))
          solution (i/maximize (i/- 0 cost) (i/cost-regular vars cost dfa))]
      (is (some? solution))
      ;; minimum cost = 2 * 1 = 2 (all symbols = 0)
      (is (= 2 (get solution cost))))))

;; ============================================================
;; Music examples
;; ============================================================

(deftest chord-progression-table-test
  (testing "chord progressions constrained by table"
    ;; chord IDs: 0=I, 1=ii, 2=iii, 3=IV, 4=V, 5=vi
    (let [progressions [[0 3] [0 4] [3 0] [3 4] [4 0] [5 3] [5 4]]
          vars (vec (repeatedly 4 #(i/fresh-int (range 6))))
          ;; constrain each adjacent pair
          constraints (for [idx (range 3)]
                        (i/table [(vars idx) (vars (inc idx))] progressions))
          ;; start on I (0)
          solution (i/satisfy (apply i/and (i/= (first vars) 0) constraints))]
      (is (some? solution))
      (let [vals (mapv #(get solution %) vars)]
        ;; first chord is I
        (is (= 0 (first vals)))
        ;; each adjacent pair is in the allowed set
        (doseq [idx (range 3)]
          (is (some #{[(vals idx) (vals (inc idx))]} progressions)))))))

(deftest melodic-contour-regular-test
  (testing "melodic intervals constrained by regular DFA"
    ;; Simple contour grammar: after two ascending intervals (>0),
    ;; must descend (<=0). States: 0=neutral, 1=one-ascending, 2=must-descend
    ;; alphabet: -2 -1 0 1 2
    (let [alphabet #{-2 -1 0 1 2}
          asc #{1 2}
          dfa {:states      3
               :alphabet    alphabet
               :transitions [;; state 0 (neutral): ascending -> state 1, else -> 0
                             {-2 0, -1 0, 0 0, 1 1, 2 1}
                             ;; state 1 (one ascending): ascending -> state 2, else -> 0
                             {-2 0, -1 0, 0 0, 1 2, 2 2}
                             ;; state 2 (must descend): only descend/stay allowed -> 0
                             {-2 0, -1 0, 0 0}]
               :start       0
               :accept      #{0 1}}
          intervals (vec (repeatedly 6 #(i/fresh-int alphabet)))
          solution (i/satisfy (i/regular intervals dfa))]
      (is (some? solution))
      ;; verify the constraint: no three consecutive ascending intervals
      (let [vals (mapv #(get solution %) intervals)]
        (doseq [idx (range (- (count vals) 2))]
          (is (not (and (asc (vals idx))
                        (asc (vals (inc idx)))
                        (asc (vals (+ idx 2)))))))))))
