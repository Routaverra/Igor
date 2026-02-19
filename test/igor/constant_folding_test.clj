(ns igor.constant-folding-test
  "Parity tests: verify that each term evaluates identically whether
   its arguments are ground literals (Clojure evaluation) or decision
   variables constrained to the same values (solver evaluation)."
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.graph :as graph]
            [igor.utils.test :refer [only-val]]))

;; Helper: solve for a result decision constrained to equal the term expression,
;; where the term's inputs are decisions constrained to specific values.

(def ^:private int-domain (range -100 101))

(defn- solve-numeric
  "Given a term-fn that takes igor expressions and returns a numeric expression,
   and concrete values to constrain the decisions to, returns the solver's answer."
  [term-fn values]
  (let [decisions (mapv (fn [_] (i/fresh-int int-domain)) values)
        result (i/fresh-int int-domain)
        constraints (apply i/and
                           (i/= result (apply term-fn decisions))
                           (map (fn [d v] (i/= d v)) decisions values))]
    (get (i/satisfy constraints) result)))

(defn- solve-bool
  "Like solve-numeric but for boolean-returning terms."
  [term-fn values]
  (let [decisions (mapv (fn [_] (i/fresh-int int-domain)) values)
        result (i/fresh "result")
        constraints (apply i/and
                           (i/= result (apply term-fn decisions))
                           (map (fn [d v] (i/= d v)) decisions values))]
    (get (i/satisfy constraints) result)))

;; ============================================================
;; Numeric → Numeric terms
;; ============================================================

(deftest constant-fold-plus-test
  (testing "i/+ folds and matches solver"
    (doseq [[a b] [[3 4] [-5 10] [0 0]]]
      (is (= (i/+ a b) (solve-numeric i/+ [a b]))
          (str "i/+ " a " " b)))))

(deftest constant-fold-minus-test
  (testing "i/- folds and matches solver"
    (doseq [[a b] [[10 3] [-5 -10] [0 7]]]
      (is (= (i/- a b) (solve-numeric i/- [a b]))
          (str "i/- " a " " b)))))

(deftest constant-fold-product-test
  (testing "i/* folds and matches solver"
    (doseq [[a b] [[3 4] [-5 2] [0 99]]]
      (is (= (i/* a b) (solve-numeric i/* [a b]))
          (str "i/* " a " " b)))))

(deftest constant-fold-divide-test
  (testing "i// folds and matches solver (integer division)"
    (doseq [[a b] [[10 3] [7 2] [-7 2] [6 3]]]
      (is (= (i// a b) (solve-numeric i// [a b]))
          (str "i// " a " " b)))))

(deftest constant-fold-inc-test
  (testing "i/inc folds and matches solver"
    (doseq [x [0 5 -3]]
      (is (= (i/inc x) (solve-numeric (fn [a] (i/inc a)) [x]))
          (str "i/inc " x)))))

(deftest constant-fold-dec-test
  (testing "i/dec folds and matches solver"
    (doseq [x [0 5 -3]]
      (is (= (i/dec x) (solve-numeric (fn [a] (i/dec a)) [x]))
          (str "i/dec " x)))))

(deftest constant-fold-abs-test
  (testing "i/abs folds and matches solver"
    (doseq [x [-7 5 0]]
      (is (= (i/abs x) (solve-numeric (fn [a] (i/abs a)) [x]))
          (str "i/abs " x)))))

(deftest constant-fold-max-test
  (testing "i/max folds and matches solver"
    (is (= (i/max 3 7 1) (solve-numeric i/max [3 7 1])))
    (is (= (i/max -5 -1 -10) (solve-numeric i/max [-5 -1 -10])))))

(deftest constant-fold-min-test
  (testing "i/min folds and matches solver"
    (is (= (i/min 3 7 1) (solve-numeric i/min [3 7 1])))
    (is (= (i/min -5 -1 -10) (solve-numeric i/min [-5 -1 -10])))))

(deftest constant-fold-mod-test
  (testing "i/mod folds and matches solver"
    (doseq [[a b] [[7 3] [-7 3] [7 -3] [-7 -3] [6 3]]]
      (is (= (i/mod a b) (solve-numeric i/mod [a b]))
          (str "i/mod " a " " b)))))

(deftest constant-fold-rem-test
  (testing "i/rem folds and matches solver"
    (doseq [[a b] [[7 3] [-7 3] [7 -3] [-7 -3] [6 3]]]
      (is (= (i/rem a b) (solve-numeric i/rem [a b]))
          (str "i/rem " a " " b)))))

;; ============================================================
;; Numeric → Bool terms
;; ============================================================

(deftest constant-fold-gt-test
  (testing "i/> folds and matches solver"
    (doseq [[a b expected] [[5 3 true] [3 5 false] [3 3 false]]]
      (is (= expected (i/> a b)))
      (is (= expected (solve-bool i/> [a b]))
          (str "i/> " a " " b)))))

(deftest constant-fold-lt-test
  (testing "i/< folds and matches solver"
    (doseq [[a b expected] [[3 5 true] [5 3 false] [3 3 false]]]
      (is (= expected (i/< a b)))
      (is (= expected (solve-bool i/< [a b]))
          (str "i/< " a " " b)))))

(deftest constant-fold-gte-test
  (testing "i/>= folds and matches solver"
    (doseq [[a b expected] [[5 3 true] [3 3 true] [2 3 false]]]
      (is (= expected (i/>= a b)))
      (is (= expected (solve-bool i/>= [a b]))
          (str "i/>= " a " " b)))))

(deftest constant-fold-lte-test
  (testing "i/<= folds and matches solver"
    (doseq [[a b expected] [[3 5 true] [3 3 true] [5 3 false]]]
      (is (= expected (i/<= a b)))
      (is (= expected (solve-bool i/<= [a b]))
          (str "i/<= " a " " b)))))

(deftest constant-fold-equals-test
  (testing "i/= folds and matches solver"
    (doseq [[a b expected] [[3 3 true] [3 4 false]]]
      (is (= expected (i/= a b)))
      (is (= expected (solve-bool i/= [a b]))
          (str "i/= " a " " b)))))

(deftest constant-fold-not-equals-test
  (testing "i/not= folds and matches solver"
    (doseq [[a b expected] [[3 4 true] [3 3 false]]]
      (is (= expected (i/not= a b)))
      (is (= expected (solve-bool i/not= [a b]))
          (str "i/not= " a " " b)))))

(deftest constant-fold-pos?-test
  (testing "i/pos? folds and matches solver"
    (doseq [[x expected] [[5 true] [0 false] [-3 false]]]
      (is (= expected (i/pos? x)))
      (is (= expected (solve-bool (fn [a] (i/pos? a)) [x]))
          (str "i/pos? " x)))))

(deftest constant-fold-neg?-test
  (testing "i/neg? folds and matches solver"
    (doseq [[x expected] [[-3 true] [0 false] [5 false]]]
      (is (= expected (i/neg? x)))
      (is (= expected (solve-bool (fn [a] (i/neg? a)) [x]))
          (str "i/neg? " x)))))

(deftest constant-fold-zero?-test
  (testing "i/zero? folds and matches solver"
    (doseq [[x expected] [[0 true] [5 false] [-3 false]]]
      (is (= expected (i/zero? x)))
      (is (= expected (solve-bool (fn [a] (i/zero? a)) [x]))
          (str "i/zero? " x)))))

(deftest constant-fold-even?-test
  (testing "i/even? folds and matches solver"
    (doseq [[x expected] [[4 true] [3 false] [0 true]]]
      (is (= expected (i/even? x)))
      (is (= expected (solve-bool (fn [a] (i/even? a)) [x]))
          (str "i/even? " x)))))

(deftest constant-fold-odd?-test
  (testing "i/odd? folds and matches solver"
    (doseq [[x expected] [[3 true] [4 false] [0 false]]]
      (is (= expected (i/odd? x)))
      (is (= expected (solve-bool (fn [a] (i/odd? a)) [x]))
          (str "i/odd? " x)))))

(deftest constant-fold-all-different-test
  (testing "i/all-different folds and matches solver"
    (is (true? (i/all-different 1 2 3)))
    (is (false? (i/all-different 1 2 1)))))

;; ============================================================
;; Bool → Bool terms
;; ============================================================

(deftest constant-fold-and-test
  (testing "i/and folds"
    (is (true? (i/and true true)))
    (is (false? (i/and true false)))
    (is (false? (i/and false false)))))

(deftest constant-fold-or-test
  (testing "i/or folds"
    (is (true? (i/or true false)))
    (is (true? (i/or false true)))
    (is (false? (i/or false false)))))

(deftest constant-fold-not-test
  (testing "i/not folds"
    (is (true? (i/not false)))
    (is (false? (i/not true)))))

(deftest constant-fold-when-test
  (testing "i/when folds (implication: not test or body)"
    ;; true -> true = true
    (is (true? (i/when true true)))
    ;; true -> false = false
    (is (false? (i/when true false)))
    ;; false -> anything = true (vacuous)
    (is (true? (i/when false false)))
    (is (true? (i/when false true)))))

(deftest constant-fold-true?-test
  (testing "i/true? folds"
    (is (true? (i/true? true)))
    (is (false? (i/true? false)))))

(deftest constant-fold-false?-test
  (testing "i/false? folds"
    (is (true? (i/false? false)))
    (is (false? (i/false? true)))))

;; ============================================================
;; Conditional terms
;; ============================================================

(deftest constant-fold-if-test
  (testing "i/if folds"
    (is (= 10 (i/if true 10 20)))
    (is (= 20 (i/if false 10 20)))))

(deftest constant-fold-cond-test
  (testing "i/cond folds"
    (is (= 1 (i/cond true 1 :else 2)))
    (is (= 2 (i/cond false 1 :else 2)))
    (is (= 3 (i/cond false 1 true 3 :else 5)))))

;; ============================================================
;; Collection terms
;; ============================================================

(deftest constant-fold-nth-test
  (testing "i/nth folds"
    (is (= 20 (i/nth [10 20 30] 1)))
    (is (= 10 (i/nth [10 20 30] 0)))
    (is (= 30 (i/nth [10 20 30] 2)))))

(deftest constant-fold-contains?-test
  (testing "i/contains? folds"
    (is (true? (i/contains? #{1 2 3} 2)))
    (is (false? (i/contains? #{1 2 3} 5)))))

(deftest constant-fold-count-test
  (testing "i/count folds"
    (is (= 3 (i/count #{1 2 3})))
    (is (= 0 (i/count #{})))))

;; ============================================================
;; Set → Set terms
;; ============================================================

(deftest constant-fold-intersection-test
  (testing "i/intersection folds and matches solver"
    (let [a #{1 2 3 4 5 6}
          b #{4 5 6 7 8 9}]
      (is (= #{4 5 6} (i/intersection a b)))
      ;; solver parity
      (let [da (i/fresh-set (range 12))
            db (i/fresh-set (range 12))
            dr (i/fresh-set (range 12))
            solution (i/satisfy (i/and (i/= da a) (i/= db b) (i/= dr (i/intersection da db))))]
        (is (= (i/intersection a b) (get solution dr)))))))

(deftest constant-fold-difference-test
  (testing "i/difference folds and matches solver"
    (let [a #{1 2 3 4 5 6}
          b #{4 5 6 7 8 9}]
      (is (= #{1 2 3} (i/difference a b)))
      (let [da (i/fresh-set (range 12))
            db (i/fresh-set (range 12))
            dr (i/fresh-set (range 12))
            solution (i/satisfy (i/and (i/= da a) (i/= db b) (i/= dr (i/difference da db))))]
        (is (= (i/difference a b) (get solution dr)))))))

(deftest constant-fold-sym-diff-test
  (testing "i/sym-diff folds and matches solver"
    (let [a #{1 2 3 4 5 6}
          b #{4 5 6 7 8 9}]
      (is (= #{1 2 3 7 8 9} (i/sym-diff a b)))
      (let [da (i/fresh-set (range 12))
            db (i/fresh-set (range 12))
            dr (i/fresh-set (range 12))
            solution (i/satisfy (i/and (i/= da a) (i/= db b) (i/= dr (i/sym-diff da db))))]
        (is (= (i/sym-diff a b) (get solution dr)))))))

(deftest constant-fold-union-test
  (testing "i/union folds and matches solver"
    (let [a #{1 2 3 4 5 6}
          b #{4 5 6 7 8 9}]
      (is (= #{1 2 3 4 5 6 7 8 9} (i/union a b)))
      (let [da (i/fresh-set (range 12))
            db (i/fresh-set (range 12))
            dr (i/fresh-set (range 12))
            solution (i/satisfy (i/and (i/= da a) (i/= db b) (i/= dr (i/union da db))))]
        (is (= (i/union a b) (get solution dr)))))))

(deftest constant-fold-subset?-test
  (testing "i/subset? folds and matches solver"
    (is (true? (i/subset? #{1 2} #{1 2 3})))
    (is (false? (i/subset? #{1 2 3} #{1 2})))))

(deftest constant-fold-superset?-test
  (testing "i/superset? folds and matches solver"
    (is (true? (i/superset? #{1 2 3} #{1 2})))
    (is (false? (i/superset? #{1 2} #{1 2 3})))))

;; ============================================================
;; Cascading folding — compound expressions with all literals
;; ============================================================

(deftest cascading-fold-test
  (testing "nested all-literal expressions collapse to Clojure values"
    ;; (+ (* 3 4) (- 10 5)) = 12 + 5 = 17
    (is (= 17 (i/+ (i/* 3 4) (i/- 10 5))))
    ;; (abs (- 3 10)) = 7
    (is (= 7 (i/abs (i/- 3 10))))
    ;; (max (+ 1 2) (* 2 3)) = max(3, 6) = 6
    (is (= 6 (i/max (i/+ 1 2) (i/* 2 3))))
    ;; (if (> 5 3) (+ 1 2) (* 3 4)) = 3
    (is (= 3 (i/if (i/> 5 3) (i/+ 1 2) (i/* 3 4))))
    ;; (and (> 5 3) (even? 4)) = true
    (is (true? (i/and (i/> 5 3) (i/even? 4))))
    ;; (= (mod 10 3) 1) = true
    (is (true? (i/= (i/mod 10 3) 1)))))

;; ============================================================
;; Graph ground pass-through
;; ============================================================

(deftest constant-fold-dreachable-test
  (testing "i/dreachable with ground root folds to boolean"
    (let [g (graph/digraph [[0 1] [1 2] [2 3]])]
      ;; all nodes reachable from 0
      (is (true? (i/dreachable g 0)))
      ;; node 0 not reachable from 3 (directed)
      (is (false? (i/dreachable g 3))))))

(deftest constant-fold-reachable-test
  (testing "i/reachable with ground root folds to boolean"
    (let [g (graph/digraph [[0 1] [1 0] [1 2] [2 1] [2 3] [3 2]])]
      ;; all nodes reachable undirected from 0
      (is (true? (i/reachable g 0)))
      ;; also reachable from 3
      (is (true? (i/reachable g 3))))
    ;; disconnected graph
    (let [g (graph/digraph [[0 1] [1 0] [2 3] [3 2]])]
      ;; not all nodes reachable from 0
      (is (false? (i/reachable g 0))))))
