(ns igor.notation-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.notation :as notation]
            [igor.api :as api]))

;; ============================================================
;; TermAs transparency — does not affect solving
;; ============================================================

(deftest as-transparency-test
  (testing "as wrapper does not affect solve results"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          constraint (i/and (i/= x 3) (i/= y 7))
          wrapped (i/and (i/as :c1 (i/= x 3)) (i/as :c2 (i/= y 7)))
          sol1 (i/satisfy constraint)
          sol2 (i/satisfy wrapped)]
      (is (= (get sol1 x) (get sol2 x)))
      (is (= (get sol1 y) (get sol2 y))))))

;; ============================================================
;; Leaf rendering
;; ============================================================

(deftest decision-var-latex-test
  (testing "decision variable with trailing digits gets subscript"
    (let [x (api/fresh "x1")]
      (is (= "x_{1}" (notation/render-notation x :format :latex))))))

(deftest decision-var-no-digits-test
  (testing "decision variable without trailing digits renders as-is"
    (let [x (api/fresh "abc")]
      (is (= "abc" (notation/render-notation x :format :latex))))))

(deftest ground-number-test
  (is (= "42" (notation/render-notation 42 :format :latex)))
  (is (= "42" (notation/render-notation 42 :format :unicode))))

(deftest ground-boolean-test
  (is (= "\\top" (notation/render-notation true :format :latex)))
  (is (= "\\bot" (notation/render-notation false :format :latex)))
  (is (= "⊤" (notation/render-notation true :format :unicode)))
  (is (= "⊥" (notation/render-notation false :format :unicode))))

(deftest ground-set-test
  (is (= "\\{1, 2, 3\\}" (notation/render-notation #{1 2 3} :format :latex)))
  (is (= "{1, 2, 3}" (notation/render-notation #{1 2 3} :format :unicode))))

;; ============================================================
;; Operator rendering
;; ============================================================

(deftest binary-arithmetic-test
  (let [x (api/fresh "x")
        y (api/fresh "y")]
    (testing "plus"
      (is (= "x + y" (notation/render-notation (i/+ x y) :format :latex))))
    (testing "minus"
      (is (= "x - y" (notation/render-notation (i/- x y) :format :latex))))
    (testing "product"
      (is (= "x \\cdot y" (notation/render-notation (i/* x y) :format :latex))))
    (testing "divide"
      (is (= "\\frac{x}{y}" (notation/render-notation (i// x y) :format :latex))))))

(deftest comparison-test
  (let [x (api/fresh "x")
        y (api/fresh "y")]
    (is (= "x = y" (notation/render-notation (i/= x y) :format :latex)))
    (is (= "x > y" (notation/render-notation (i/> x y) :format :latex)))
    (is (= "x < y" (notation/render-notation (i/< x y) :format :latex)))
    (is (= "x \\geq y" (notation/render-notation (i/>= x y) :format :latex)))
    (is (= "x \\leq y" (notation/render-notation (i/<= x y) :format :latex)))))

(deftest logic-test
  (let [x (api/fresh "x")
        y (api/fresh "y")
        a (i/= x 1)
        b (i/= y 2)]
    (testing "and"
      (is (= "x = 1 \\wedge y = 2" (notation/render-notation (i/and a b) :format :latex))))
    (testing "or"
      (is (= "x = 1 \\vee y = 2" (notation/render-notation (i/or a b) :format :latex))))
    (testing "not"
      (is (= "\\neg x = 1" (notation/render-notation (i/not a) :format :latex))))
    (testing "implication"
      (is (= "x = 1 \\Rightarrow y = 2" (notation/render-notation (i/when a b) :format :latex))))))

(deftest abs-test
  (let [x (api/fresh "x")]
    (is (= "\\left|x\\right|" (notation/render-notation (i/abs x) :format :latex)))
    (is (= "|x|" (notation/render-notation (i/abs x) :format :unicode)))))

(deftest contains-test
  (let [x (api/fresh "x")]
    (is (= "x \\in \\{1, 2, 3\\}"
           (notation/render-notation (i/contains? #{1 2 3} x) :format :latex)))
    (is (= "x ∈ {1, 2, 3}"
           (notation/render-notation (i/contains? #{1 2 3} x) :format :unicode)))))

(deftest all-different-test
  (let [x (api/fresh "x")
        y (api/fresh "y")
        z (api/fresh "z")]
    (is (= "\\text{allDiff}(x, y, z)"
           (notation/render-notation (i/all-different x y z) :format :latex)))))

;; ============================================================
;; Parenthesization
;; ============================================================

(deftest parens-product-of-sum-test
  (testing "(x + y) * z needs parens around the sum"
    (let [x (api/fresh "x")
          y (api/fresh "y")
          z (api/fresh "z")]
      (is (= "\\left(x + y\\right) \\cdot z"
             (notation/render-notation (i/* (i/+ x y) z) :format :latex))))))

(deftest no-parens-sum-of-product-test
  (testing "x * y + z does NOT need parens"
    (let [x (api/fresh "x")
          y (api/fresh "y")
          z (api/fresh "z")]
      (is (= "x \\cdot y + z"
             (notation/render-notation (i/+ (i/* x y) z) :format :latex))))))

(deftest parens-unicode-test
  (testing "parenthesization in unicode format"
    (let [x (api/fresh "x")
          y (api/fresh "y")
          z (api/fresh "z")]
      (is (= "(x + y) · z"
             (notation/render-notation (i/* (i/+ x y) z) :format :unicode))))))

;; ============================================================
;; Definition collection and rendering
;; ============================================================

(deftest single-definition-test
  (testing "single as definition renders with := and reference"
    (let [x (api/fresh "x")
          y (api/fresh "y")
          c (i/as :C (i/= x y))]
      (is (= "C := x = y\nC"
             (notation/render-notation c :format :latex))))))

(deftest nested-definitions-test
  (testing "nested definitions are topo-sorted"
    (let [x (api/fresh "x")
          y (api/fresh "y")
          inner (i/as :A (i/+ x y))
          outer (i/as :B (i/= inner 10))]
      (let [result (notation/render-notation outer :format :latex)]
        ;; A should come before B in the output
        (is (clojure.string/includes? result "A := x + y"))
        (is (clojure.string/includes? result "B := A = 10"))
        (is (< (.indexOf result "A :=") (.indexOf result "B :=")))))))

(deftest definition-in-conjunction-test
  (testing "definitions within a conjunction"
    (let [x (api/fresh "x")
          y (api/fresh "y")
          c1 (i/as :C1 (i/= x 3))
          c2 (i/as :C2 (i/= y 7))
          top (i/and c1 c2)]
      (let [result (notation/render-notation top :format :latex)]
        (is (clojure.string/includes? result "C_{1} := x = 3"))
        (is (clojure.string/includes? result "C_{2} := y = 7"))
        (is (clojure.string/includes? result "C_{1} \\wedge C_{2}"))))))

;; ============================================================
;; Unicode format
;; ============================================================

(deftest unicode-basic-test
  (let [x (api/fresh "x")
        y (api/fresh "y")]
    (is (= "x + y" (notation/render-notation (i/+ x y) :format :unicode)))
    (is (= "x · y" (notation/render-notation (i/* x y) :format :unicode)))))

(deftest unicode-logic-test
  (let [x (api/fresh "x")
        y (api/fresh "y")
        a (i/= x 1)
        b (i/= y 2)]
    (is (= "x = 1 ∧ y = 2" (notation/render-notation (i/and a b) :format :unicode)))
    (is (= "x = 1 ∨ y = 2" (notation/render-notation (i/or a b) :format :unicode)))))

(deftest unicode-subscripts-test
  (testing "decision var trailing digits become unicode subscripts"
    (let [x (api/fresh "x12")]
      (is (= "x₁₂" (notation/render-notation x :format :unicode))))))

;; ============================================================
;; Conditional rendering
;; ============================================================

(deftest if-latex-test
  (let [x (api/fresh "x")
        y (api/fresh "y")]
    (is (clojure.string/includes?
         (notation/render-notation (i/if (i/> x 0) y 0) :format :latex)
         "\\begin{cases}"))))

;; ============================================================
;; Set operations
;; ============================================================

(deftest set-operations-test
  (let [a (api/fresh "A")
        b (api/fresh "B")]
    (is (= "A \\cup B" (notation/render-notation (i/union a b) :format :latex)))
    (is (= "A \\cap B" (notation/render-notation (i/intersection a b) :format :latex)))
    (is (= "A \\setminus B" (notation/render-notation (i/difference a b) :format :latex)))
    (is (= "A \\subseteq B" (notation/render-notation (i/subset? a b) :format :latex)))
    (is (= "A \\supseteq B" (notation/render-notation (i/superset? a b) :format :latex)))))
