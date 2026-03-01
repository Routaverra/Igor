(ns igor.evaluate-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.api :as api]
            [igor.protocols :as protocols]
            [igor.solver :as solver]
            [igor.graph :as graph]
            [igor.extensional :as extensional]
            [igor.utils.test :refer [only-val throws?]]))

;; ============================================================
;; 1. Unit tests — per-term-type evaluate with hand-crafted solutions
;; ============================================================

(deftest arithmetic-evaluate-test
  (testing "basic arithmetic terms"
    (let [x (api/fresh-int (range 10))
          y (api/fresh-int (range 10))
          sol {x 3 y 5}]
      (is (= 8 (protocols/evaluate (i/+ x y) sol)))
      (is (= 15 (protocols/evaluate (i/* x y) sol)))
      (is (= -2 (protocols/evaluate (i/- x y) sol)))
      (is (= 1 (protocols/evaluate (i/- y x 1) sol)))
      (is (= 4 (protocols/evaluate (i/inc x) sol)))
      (is (= 2 (protocols/evaluate (i/dec x) sol)))
      (is (= 2 (protocols/evaluate (i/abs (i/- x y)) sol)))
      (is (= 5 (protocols/evaluate (i/max x y) sol)))
      (is (= 3 (protocols/evaluate (i/min x y) sol)))
      (is (= 3 (protocols/evaluate (i/mod x y) sol)))
      (is (= 3 (protocols/evaluate (i/rem x y) sol))))))

(deftest comparison-evaluate-test
  (testing "comparison terms"
    (let [x (api/fresh-int (range 10))
          y (api/fresh-int (range 10))
          sol {x 3 y 5}]
      (is (true? (protocols/evaluate (i/= x x) sol)))
      (is (false? (protocols/evaluate (i/= x y) sol)))
      (is (true? (protocols/evaluate (i/> y x) sol)))
      (is (false? (protocols/evaluate (i/> x y) sol)))
      (is (true? (protocols/evaluate (i/< x y) sol)))
      (is (true? (protocols/evaluate (i/>= y x) sol)))
      (is (true? (protocols/evaluate (i/<= x y) sol))))))

(deftest logic-evaluate-test
  (testing "logic terms"
    (let [a (api/fresh-bool)
          b (api/fresh-bool)
          sol {a true b false}]
      (is (false? (protocols/evaluate (i/and a b) sol)))
      (is (true? (protocols/evaluate (i/and a a) sol)))
      (is (true? (protocols/evaluate (i/or a b) sol)))
      (is (false? (protocols/evaluate (i/or b b) sol)))
      (is (false? (protocols/evaluate (i/not a) sol)))
      (is (true? (protocols/evaluate (i/not b) sol)))
      ;; when: false -> anything = true (implication)
      (is (true? (protocols/evaluate (i/when b a) sol)))
      ;; when: true -> false = false
      (is (false? (protocols/evaluate (i/when a b) sol))))))

(deftest predicate-evaluate-test
  (testing "predicate terms"
    (let [x (api/fresh-int (range -5 6))
          sol-pos {x 3}
          sol-neg {x -2}
          sol-zero {x 0}
          sol-even {x 4}
          sol-odd {x 3}]
      (is (true? (protocols/evaluate (i/pos? x) sol-pos)))
      (is (false? (protocols/evaluate (i/pos? x) sol-neg)))
      (is (true? (protocols/evaluate (i/neg? x) sol-neg)))
      (is (true? (protocols/evaluate (i/zero? x) sol-zero)))
      (is (true? (protocols/evaluate (i/even? x) sol-even)))
      (is (true? (protocols/evaluate (i/odd? x) sol-odd))))))

(deftest conditional-evaluate-test
  (testing "if/cond terms"
    (let [x (api/fresh-int (range 10))
          b (api/fresh-bool)
          sol-t {x 3 b true}
          sol-f {x 3 b false}]
      (is (= 3 (protocols/evaluate (i/if b x 99) sol-t)))
      (is (= 99 (protocols/evaluate (i/if b x 99) sol-f)))
      ;; x=3: (> 3 5) false, (> 3 2) true => 20
      (is (= 20 (protocols/evaluate (i/cond (i/> x 5) 10
                                             (i/> x 2) 20
                                             :else 30) sol-t)))
      ;; x=3: (> 3 10) false, (> 3 0) true => 99
      (is (= 99 (protocols/evaluate (i/cond (i/> x 10) 50
                                             (i/> x 0) 99
                                             :else 0) sol-t))))))

(deftest collection-evaluate-test
  (testing "nth, contains?, all-different, count"
    (let [x (api/fresh-int (range 5))
          y (api/fresh-int (range 5))
          z (api/fresh-int (range 5))
          s (api/fresh-set (range 10))
          sol {x 1 y 2 z 3 s #{2 5 8}}]
      (is (= 20 (protocols/evaluate (i/nth [10 20 30] x) sol)))
      (is (true? (protocols/evaluate (i/contains? s x) {x 2 s #{2 5 8}})))
      (is (false? (protocols/evaluate (i/contains? s x) {x 4 s #{2 5 8}})))
      (is (true? (protocols/evaluate (i/all-different x y z) sol)))
      (is (false? (protocols/evaluate (i/all-different x y z) {x 1 y 1 z 3})))
      (is (= 3 (protocols/evaluate (i/count s) sol))))))

(deftest set-ops-evaluate-test
  (testing "set operations"
    (let [a (api/fresh-set (range 10))
          b (api/fresh-set (range 10))
          sol {a #{1 2 3} b #{2 3 4}}]
      (is (= #{2 3} (protocols/evaluate (i/intersection a b) sol)))
      (is (= #{1} (protocols/evaluate (i/difference a b) sol)))
      (is (= #{1 2 3 4} (protocols/evaluate (i/union a b) sol)))
      (is (= #{1 4} (protocols/evaluate (i/sym-diff a b) sol)))
      (is (true? (protocols/evaluate (i/subset? a (i/union a b)) sol)))
      (is (true? (protocols/evaluate (i/superset? (i/union a b) a) sol))))))

(deftest table-evaluate-test
  (testing "table constraint"
    (let [x (api/fresh-int (range 5))
          y (api/fresh-int (range 5))
          tuples [[1 2] [3 4] [0 0]]
          term (extensional/table [x y] tuples)]
      (is (true? (protocols/evaluate term {x 1 y 2})))
      (is (true? (protocols/evaluate term {x 3 y 4})))
      (is (false? (protocols/evaluate term {x 1 y 3}))))))

(deftest regular-evaluate-test
  (testing "regular constraint"
    (let [;; DFA: accepts sequences that start with 1 and end with 2
          dfa {:states 3
               :alphabet #{1 2}
               :transitions [{1 1 2 2}   ;; state 0: start
                             {1 1 2 2}   ;; state 1: seen 1
                             {1 1 2 2}]  ;; state 2: seen 2
               :start 0
               :accept #{2}}
          x (api/fresh-int #{1 2})
          y (api/fresh-int #{1 2})
          z (api/fresh-int #{1 2})
          term (extensional/regular [x y z] dfa)]
      (is (true? (protocols/evaluate term {x 1 y 1 z 2})))
      (is (true? (protocols/evaluate term {x 2 y 1 z 2})))
      (is (false? (protocols/evaluate term {x 1 y 2 z 1}))))))

(deftest forall-evaluate-test
  (testing "forall quantifier"
    (let [x (api/fresh-int (range 10))
          ;; forall i in {0..4}: x > i means x > 4, i.e., x >= 5
          term (i/forall (sorted-set 0 1 2 3 4)
                         (fn [i] (i/> x i)))]
      (is (true? (protocols/evaluate term {x 5})))
      (is (false? (protocols/evaluate term {x 3}))))))

(deftest for-set-evaluate-test
  (testing "for-set generator"
    (let [x (api/fresh-int (range 10))
          ;; {i + x | i in {0,1,2}} with x=10 => {10, 11, 12}
          term (i/for-set (sorted-set 0 1 2)
                          (fn [i] (i/+ i x)))]
      (is (= #{10 11 12} (protocols/evaluate term {x 10}))))))

(deftest as-evaluate-test
  (testing "TermAs delegates to inner"
    (let [x (api/fresh-int (range 10))
          term (i/as :my-name (i/+ x 1))]
      (is (= 6 (protocols/evaluate term {x 5}))))))

;; ============================================================
;; 2. Solver parity — solve real problems, then validate-solution
;; ============================================================

(deftest send-more-money-validation-test
  (testing "SEND+MORE=MONEY: solver solution validates"
    (let [digits-domain (range 10)
          s (i/fresh-int digits-domain) e (i/fresh-int digits-domain)
          n (i/fresh-int digits-domain) d (i/fresh-int digits-domain)
          m (i/fresh-int digits-domain) o (i/fresh-int digits-domain)
          r (i/fresh-int digits-domain) y (i/fresh-int digits-domain)
          digits [s e n d m o r y]
          send  (i/+ (i/* s 1000) (i/* e 100) (i/* n 10) d)
          more  (i/+ (i/* m 1000) (i/* o 100) (i/* r 10) e)
          money (i/+ (i/* m 10000) (i/* o 1000) (i/* n 100) (i/* e 10) y)
          constraint (i/and
                      (apply i/all-different digits)
                      (i/= (i/+ send more) money)
                      (i/> s 0)
                      (i/> m 0))
          solution (i/satisfy constraint)]
      (is (true? (i/validate-solution constraint solution))))))

(deftest n-queens-validation-test
  (testing "N-Queens: solver solution validates"
    (let [n 5
          queens (vec (for [_ (range n)] (i/fresh-int (range n))))
          constraint (apply i/and
                            (for [i (range n)
                                  j (range (inc i) n)]
                              (i/and
                               (i/not= (nth queens i) (nth queens j))
                               (i/not= (i/abs (i/- (nth queens i) (nth queens j)))
                                        (- j i)))))
          solution (i/satisfy constraint)]
      (is (true? (i/validate-solution constraint solution))))))

(deftest table-constraint-validation-test
  (testing "table constraint: solver solution validates"
    (let [x (i/fresh-int (range 5))
          y (i/fresh-int (range 5))
          tuples [[0 1] [1 2] [2 3] [3 4]]
          constraint (i/and (extensional/table [x y] tuples)
                            (i/> x 1))
          solution (i/satisfy constraint)]
      (is (true? (i/validate-solution constraint solution))))))

(deftest regular-constraint-validation-test
  (testing "regular constraint: solver solution validates"
    (let [dfa {:states 2
               :alphabet #{0 1}
               :transitions [{0 0 1 1}    ;; state 0
                             {0 0 1 1}]   ;; state 1
               :start 0
               :accept #{1}}
          vars (vec (for [_ (range 4)] (i/fresh-int #{0 1})))
          constraint (extensional/regular vars dfa)
          solution (i/satisfy constraint)]
      (is (true? (i/validate-solution constraint solution))))))

;; ============================================================
;; 3. Negative tests — deliberately wrong solutions return false
;; ============================================================

(deftest negative-validation-test
  (testing "wrong solutions return false"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          constraint (i/and (i/= (i/+ x y) 10)
                            (i/> x 3))]
      ;; x=2, y=8: sum is 10 but x not > 3
      (is (false? (i/validate-solution constraint {x 2 y 8})))
      ;; x=5, y=6: x>3 but sum is 11
      (is (false? (i/validate-solution constraint {x 5 y 6})))
      ;; x=5, y=5: both constraints satisfied
      (is (true? (i/validate-solution constraint {x 5 y 5}))))))

;; ============================================================
;; 4. Integration test — *validate?* binding
;; ============================================================

(deftest validate-binding-test
  (testing "binding *validate?* true doesn't throw for valid problems"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          constraint (i/and (i/= (i/+ x y) 10)
                            (i/> x 0)
                            (i/> y 0))
          solution (binding [solver/*validate?* true]
                     (i/satisfy constraint))]
      (is (some? solution))
      (is (= 10 (+ (get solution x) (get solution y)))))))
