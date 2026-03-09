(ns routaverra.igor.notation-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.notation :as notation]
            [routaverra.igor.api :as api]
            [clojure.string :as str]))

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
;; Leaf rendering — auto-named variables
;; ============================================================

(deftest decision-var-auto-named-test
  (testing "single decision variable gets auto name x1"
    (let [x (api/fresh)]
      (is (= "x_{1}" (notation/render-notation x :format :latex)))
      (is (= "x₁" (notation/render-notation x :format :unicode))))))

(deftest decision-var-two-vars-test
  (testing "two decision variables get x1 and x2"
    (let [x (api/fresh)
          y (api/fresh)
          result (notation/render-notation (i/+ x y) :format :latex)]
      (is (str/includes? result "x_{1}"))
      (is (str/includes? result "x_{2}"))
      (is (str/includes? result "+")))))

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
  (let [x (api/fresh)
        y (api/fresh)]
    (testing "plus"
      (let [result (notation/render-notation (i/+ x y) :format :latex)]
        (is (str/includes? result "+"))
        (is (str/includes? result "x_{1}"))
        (is (str/includes? result "x_{2}"))))
    (testing "minus"
      (is (str/includes? (notation/render-notation (i/- x y) :format :latex) "-")))
    (testing "product"
      (is (str/includes? (notation/render-notation (i/* x y) :format :latex) "\\cdot")))
    (testing "divide"
      (is (str/includes? (notation/render-notation (i// x y) :format :latex) "\\frac")))))

(deftest comparison-test
  (let [x (api/fresh)
        y (api/fresh)]
    (is (str/includes? (notation/render-notation (i/= x y) :format :latex) "="))
    (is (str/includes? (notation/render-notation (i/> x y) :format :latex) ">"))
    (is (str/includes? (notation/render-notation (i/< x y) :format :latex) "<"))
    (is (str/includes? (notation/render-notation (i/>= x y) :format :latex) "\\geq"))
    (is (str/includes? (notation/render-notation (i/<= x y) :format :latex) "\\leq"))))

(deftest logic-test
  (let [x (api/fresh)
        y (api/fresh)
        a (i/= x 1)
        b (i/= y 2)]
    (testing "and"
      (is (str/includes? (notation/render-notation (i/and a b) :format :latex) "\\wedge")))
    (testing "or"
      (is (str/includes? (notation/render-notation (i/or a b) :format :latex) "\\vee")))
    (testing "not"
      (is (str/includes? (notation/render-notation (i/not a) :format :latex) "\\neg")))
    (testing "implication"
      (is (str/includes? (notation/render-notation (i/when a b) :format :latex) "\\Rightarrow")))))

(deftest abs-test
  (let [x (api/fresh)]
    (is (str/includes? (notation/render-notation (i/abs x) :format :latex) "\\left|"))
    (is (str/includes? (notation/render-notation (i/abs x) :format :unicode) "|"))))

(deftest contains-test
  (let [x (api/fresh)]
    (is (str/includes? (notation/render-notation (i/contains? #{1 2 3} x) :format :latex) "\\in"))
    (is (str/includes? (notation/render-notation (i/contains? #{1 2 3} x) :format :unicode) "∈"))))

(deftest all-different-test
  (let [x (api/fresh)
        y (api/fresh)
        z (api/fresh)]
    (is (str/includes? (notation/render-notation (i/all-different x y z) :format :latex) "\\text{allDiff}"))))

;; ============================================================
;; Parenthesization
;; ============================================================

(deftest parens-product-of-sum-test
  (testing "(x + y) * z needs parens around the sum"
    (let [x (api/fresh)
          y (api/fresh)
          z (api/fresh)
          result (notation/render-notation (i/* (i/+ x y) z) :format :latex)]
      (is (str/includes? result "\\left("))
      (is (str/includes? result "\\right)"))
      (is (str/includes? result "+"))
      (is (str/includes? result "\\cdot")))))

(deftest no-parens-sum-of-product-test
  (testing "x * y + z does NOT need parens"
    (let [x (api/fresh)
          y (api/fresh)
          z (api/fresh)
          result (notation/render-notation (i/+ (i/* x y) z) :format :latex)]
      (is (not (str/includes? result "\\left(")))
      (is (str/includes? result "\\cdot"))
      (is (str/includes? result "+")))))

(deftest parens-unicode-test
  (testing "parenthesization in unicode format"
    (let [x (api/fresh)
          y (api/fresh)
          z (api/fresh)
          result (notation/render-notation (i/* (i/+ x y) z) :format :unicode)]
      (is (str/includes? result "("))
      (is (str/includes? result ")"))
      (is (str/includes? result "·")))))

;; ============================================================
;; Definition collection and rendering
;; ============================================================

(deftest single-definition-test
  (testing "single as definition renders with := and reference"
    (let [x (api/fresh)
          y (api/fresh)
          c (i/as :C (i/= x y))
          result (notation/render-notation c :format :latex)]
      (is (str/includes? result "C :="))
      (is (str/includes? result "="))
      ;; Last line is the reference
      (is (= "C" (last (str/split-lines result)))))))

(deftest nested-definitions-test
  (testing "nested definitions are topo-sorted"
    (let [x (api/fresh)
          y (api/fresh)
          inner (i/as :A (i/+ x y))
          outer (i/as :B (i/= inner 10))
          result (notation/render-notation outer :format :latex)]
      ;; A should come before B in the output
      (is (str/includes? result "A :="))
      (is (str/includes? result "B := A = 10"))
      (is (< (.indexOf result "A :=") (.indexOf result "B :="))))))

(deftest definition-in-conjunction-test
  (testing "definitions within a conjunction"
    (let [x (api/fresh)
          y (api/fresh)
          c1 (i/as :C1 (i/= x 3))
          c2 (i/as :C2 (i/= y 7))
          top (i/and c1 c2)
          result (notation/render-notation top :format :latex)]
      (is (str/includes? result "C_{1} :="))
      (is (str/includes? result "C_{2} :="))
      (is (str/includes? result "C_{1} \\wedge C_{2}")))))

;; ============================================================
;; Unicode format
;; ============================================================

(deftest unicode-basic-test
  (let [x (api/fresh)
        y (api/fresh)]
    (is (str/includes? (notation/render-notation (i/+ x y) :format :unicode) "+"))
    (is (str/includes? (notation/render-notation (i/* x y) :format :unicode) "·"))))

(deftest unicode-logic-test
  (let [x (api/fresh)
        y (api/fresh)
        a (i/= x 1)
        b (i/= y 2)]
    (is (str/includes? (notation/render-notation (i/and a b) :format :unicode) "∧"))
    (is (str/includes? (notation/render-notation (i/or a b) :format :unicode) "∨"))))

(deftest unicode-subscripts-test
  (testing "auto-named decision var gets unicode subscripts"
    (let [x (api/fresh)]
      (is (= "x₁" (notation/render-notation x :format :unicode))))))

;; ============================================================
;; Conditional rendering
;; ============================================================

(deftest if-latex-test
  (let [x (api/fresh)
        y (api/fresh)]
    (is (str/includes?
         (notation/render-notation (i/if (i/> x 0) y 0) :format :latex)
         "\\begin{cases}"))))

;; ============================================================
;; Set operations
;; ============================================================

(deftest set-operations-test
  (let [a (api/fresh)
        b (api/fresh)]
    (is (str/includes? (notation/render-notation (i/union a b) :format :latex) "\\cup"))
    (is (str/includes? (notation/render-notation (i/intersection a b) :format :latex) "\\cap"))
    (is (str/includes? (notation/render-notation (i/difference a b) :format :latex) "\\setminus"))
    (is (str/includes? (notation/render-notation (i/subset? a b) :format :latex) "\\subseteq"))
    (is (str/includes? (notation/render-notation (i/superset? a b) :format :latex) "\\supseteq"))))

;; ============================================================
;; Domain declarations
;; ============================================================

(deftest domain-declaration-int-test
  (testing "integer variables get domain declarations"
    (let [x (i/fresh-int (range 10))
          result (notation/render-notation x :format :latex)]
      (is (str/includes? result "\\in \\{0, \\ldots, 9\\}")))))

(deftest domain-declaration-grouped-test
  (testing "variables with same domain are grouped on one line"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          result (notation/render-notation (i/+ x y) :format :latex)]
      ;; Both vars on same declaration line
      (is (str/includes? result "x_{1}, x_{2} \\in \\{0, \\ldots, 9\\}")))))

(deftest domain-declaration-set-test
  (testing "set variables use subset notation"
    (let [s (api/fresh-set (range 6))
          result (notation/render-notation s :format :latex)]
      (is (str/includes? result "\\subseteq")))))

(deftest domain-declaration-unicode-test
  (testing "unicode domain declarations"
    (let [x (i/fresh-int (range 10))
          result (notation/render-notation x :format :unicode)]
      (is (str/includes? result "∈"))
      (is (str/includes? result "…")))))

(deftest domain-declaration-alias-test
  (testing "aliased variables use their alias names in declarations"
    (let [tempo (i/as :tempo (i/fresh-int (range 60 200)))
          result (notation/render-problem (i/>= tempo 120) :format :latex)]
      (is (str/includes? result "tempo \\in \\{60, \\ldots, 199\\}")))))

;; ============================================================
;; Quantifier variable naming
;; ============================================================

(deftest quantifier-iterator-names-test
  (testing "every? uses conventional iterator names (i, j, k...)"
    (let [result (notation/render-notation
                  (i/every? #{1 2 3} (fn [k] (i/> k 0)))
                  :format :latex)]
      ;; Should use "i" (first iterator name), not a gensym
      (is (str/includes? result "\\forall i"))
      (is (str/includes? result "\\in"))
      (is (str/includes? result "> 0")))))

;; ============================================================
;; Alias behavior: as wrapping bare Decision
;; ============================================================

(deftest as-alias-no-definition-line-test
  (testing "as wrapping a bare Decision uses the name directly, no definition line"
    (let [x (i/as :tempo (i/fresh-int (range 100)))
          result (notation/render-notation x :format :latex)]
      ;; Should have domain declaration and name, but no ":=" definition
      (is (str/includes? result "tempo"))
      (is (not (str/includes? result ":=")))
      ;; Last line is just the variable reference
      (is (= "tempo" (last (str/split-lines result)))))))

(deftest as-alias-in-expression-test
  (testing "as-aliased decision renders with its name in expressions"
    (let [x (i/as :x (i/fresh-int (range 100)))
          y (i/as :y (i/fresh-int (range 100)))
          result (notation/render-notation (i/+ x y) :format :latex)]
      ;; Last line is the expression using alias names
      (is (= "x + y" (last (str/split-lines result))))
      ;; Domain declaration present
      (is (str/includes? result "\\in")))))

(deftest as-alias-mixed-with-real-definition-test
  (testing "alias + real definition: alias uses name directly, real def gets := line"
    (let [x (i/as :x (i/fresh-int (range 100)))
          y (i/as :y (i/fresh-int (range 100)))
          sum (i/as :S (i/+ x y))
          result (notation/render-notation (i/= sum 42) :format :latex)]
      (is (str/includes? result "S := x + y"))
      (is (str/includes? result "S = 42"))
      (is (not (str/includes? result "x :=")))
      (is (not (str/includes? result "y :="))))))

(deftest as-alias-in-problem-test
  (testing "as-aliased decisions work in render-problem"
    (let [tempo (i/as :tempo (i/fresh-int (range 60 200)))
          result (notation/render-problem
                  (i/and (i/>= tempo 120) (i/<= tempo 140))
                  :format :latex)]
      (is (str/includes? result "tempo"))
      (is (not (str/includes? result ":=")))
      (is (str/includes? result "\\text{satisfy}")))))
