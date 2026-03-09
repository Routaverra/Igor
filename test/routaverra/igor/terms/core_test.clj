(ns routaverra.igor.terms.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.types :as types]
            [routaverra.igor.utils.test :refer [only-val throws?]]))

(def ^:private int-domain (range -100 101))

(deftest gte-test
  (testing ">="
    (let [a (i/fresh-int int-domain)
          b (i/fresh-int int-domain)]
      (is (= 20
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/when (i/>= a 10)
                  (i/= b 20))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/when (i/>= a 10)
                  (i/= b true))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/= b (i/>= 21 a 20 19))))
              b))))))

(deftest lte-test
  (testing "<="
    (let [a (i/fresh-int int-domain)
          b (i/fresh-int int-domain)]
      (is (= 20
             (get
              (i/satisfy
               (i/and
                (i/= a 30)
                (i/when (i/<= a 30)
                  (i/= b 20))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/when (i/<= a 21)
                  (i/= b true))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/= b (i/<= 19 a 20 21))))
              b))))))

(deftest gt-test
  (testing ">"
    (let [a (i/fresh-int int-domain)
          b (i/fresh-int int-domain)]
      (is (= 20
             (get
              (i/satisfy
               (i/and
                (i/= a 31)
                (i/when (i/> a 30)
                  (i/= b 20))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= false
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/when (i/> a 21)
                  (i/= b true))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/= b (i/> 21 a 19))))
              b))))))

(deftest lt-test
  (testing "<"
    (let [a (i/fresh-int int-domain)
          b (i/fresh-int int-domain)]
      (is (= 20
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/when (i/< a 30)
                  (i/= b 20))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/when (i/< a 21)
                  (i/= b true))))
              b))))

    (let [a (i/fresh-int int-domain)
          b (i/fresh)]
      (is (= true
             (get
              (i/satisfy
               (i/and
                (i/= a 20)
                (i/= b (i/< 19 a 21))))
              b))))))

(deftest zero?-test
  (testing "zero?"
    (is (= 0 (only-val (i/satisfy (i/zero? (i/fresh-int int-domain))))))

    (is (= 42 (let [a (i/fresh-int int-domain)
                     b (i/fresh-int int-domain)]
                 (->
                  (i/satisfy
                   (i/and (i/= 0 a)
                          (i/when (i/zero? a)
                            (i/= b 42))))
                  (get b)))))))

(deftest pos?-test
  (testing "pos?"
    (is (clojure.core/pos? (only-val (i/satisfy (i/pos? (i/fresh-int int-domain))))))

    (is (= 42 (let [a (i/fresh-int int-domain)
                     b (i/fresh-int int-domain)]
                 (->
                  (i/satisfy
                   (i/and (i/= -42 a)
                          (i/when (i/not (i/pos? a))
                            (i/= b 42))))
                  (get b)))))))

(deftest neg?-test
  (testing "neg?"
    (is (clojure.core/neg? (only-val (i/satisfy (i/neg? (i/fresh-int int-domain))))))

    (is (= 42 (let [a (i/fresh-int int-domain)
                     b (i/fresh-int int-domain)]
                 (->
                  (i/satisfy
                   (i/and (i/= 42 a)
                          (i/when (i/not (i/neg? a))
                            (i/= b 42))))
                  (get b)))))))

(deftest plus-test
  (testing "+"
    (is (= 2 (only-val (i/satisfy (i/= (i/+ 1 (i/fresh-int int-domain)) 3)))))))

(deftest product-test
  (testing "*"
    (is (= 3 (only-val (i/satisfy (i/= (i/* 1 (i/fresh-int int-domain)) 3)))))))

(deftest minus-test
  (testing "-"
    (is (= -2 (only-val (i/satisfy (i/= (i/- 1 (i/fresh-int int-domain)) 3)))))))

(deftest dec-test
  (testing "dec"
    (is (= 4 (only-val (i/satisfy (i/= (i/dec (i/fresh-int int-domain)) 3)))))))

(deftest inc-test
  (testing "inc"
    (is (= 2 (only-val (i/satisfy (i/= (i/inc (i/fresh-int int-domain)) 3)))))))

(deftest even?-test
  (testing "even?"
    (is (clojure.core/even? (only-val (i/satisfy (i/even? (i/fresh-int int-domain))))))))

(deftest odd?-test
  (testing "odd?"
    (is (clojure.core/odd? (only-val (i/satisfy (i/odd? (i/fresh-int int-domain))))))))

(deftest true?-test
  (testing "true?"
    (is (clojure.core/true? (only-val (i/satisfy (i/true? (i/fresh))))))))

(deftest false?-test
  (testing "false?"
    (is (clojure.core/false? (only-val (i/satisfy (i/false? (i/fresh))))))))

(deftest max-test
  (testing "max"
    (is (= 8 (only-val (i/satisfy (i/= (i/fresh-int int-domain) (i/max 8 4 2))))))))

(deftest min-test
  (testing "min"
    (is (= -2 (only-val (i/satisfy (i/= (i/fresh-int int-domain) (i/min 8 4 -2))))))))

(deftest divide-test
  (testing "/"
    (is (= 2 (only-val (i/satisfy (i/= (i// 6 (i/fresh-int int-domain)) 3)))))))

(deftest equals-test
  (testing "="
    (is (= 1
           (count (only-val (protocols/decisions (i/= (i/fresh) 1))))))

    (is (= 1
           (count (only-val (protocols/decisions (i/= (i/fresh) #{}))))))

    (is (= (count types/all-decision-types)
           (count (only-val (protocols/decisions (i/= (i/fresh) (i/fresh)))))))))

(deftest ambiguous-type-test
  (testing "satisfy throws when a bare fresh is used in both numeric and set contexts"
    (let [x (i/bind (range 5) (i/fresh))]
      (is (throws?
           (i/satisfy
            (i/and
             (i/= (i/+ x 1) 3)
             (i/= (i/intersection x #{1 2}) #{1}))))))))

(deftest not=-test
  (testing "not="
    (is (clojure.core/not= 1 (only-val (i/satisfy (i/not= (i/fresh-int int-domain) 1)))))

    (is (clojure.core/not= #{} (only-val (i/satisfy (i/not= (i/bind (range 100) (i/fresh)) #{})))))))

(deftest when-test
  (testing "when"
    (let [a (i/fresh-int int-domain)]
      (is (= 3
             (get
              (i/satisfy
               (i/when true (i/= a 3)))
              a)))

      (is (clojure.core/not= 3
                              (get
                               (i/satisfy
                                (i/when false (i/= a 3)))
                               a))))))

(deftest not-test
  (testing "not"
    (let [a (i/fresh-int int-domain)]
      (is (clojure.core/not=
           1
           (get
            (i/satisfy (i/when (i/not true)
                         (i/= a 1)))
            a))))))

(deftest if-test
  (testing "if"
    (testing "validates the test is boolean"
      (is (= true (throws? (i/if 1 (i/fresh) (i/fresh)))))
      (is (= false (throws? (i/if (i/= 1 (i/fresh)) (i/fresh) (i/fresh))))))

    (testing "validates the return types are consistent"
      (is (= true (throws? (i/if (i/fresh) 1 #{}))))
      (is (= false (throws? (i/if (i/fresh) #{} #{})))))

    (testing "evaluates"
      (let [a (i/fresh-int int-domain)
            b (i/fresh-int int-domain)]
        (is (= 0
               (get
                (i/satisfy
                 (i/and
                  (i/= a 9)
                  (i/if (i/>= a 10) (i/= b 1) (i/= b 0))))
                b)))

        (is (= 1
               (get
                (i/satisfy
                 (i/and
                  (i/= a 11)
                  (i/if (i/>= a 10) (i/= b 1) (i/= b 0))))
                b)))

        (is (= 10
               (get
                (i/satisfy
                 (i/and
                  (i/= a 11)
                  (i/= b (i/+ 5 (i/if (i/>= a 10) 5 6)))))
                b)))))))

(deftest cond-test
  (testing "cond"
    (testing "validates the test is boolean"
      (is (= false (throws? (i/cond false (i/fresh) :else (i/fresh)))))
      (is (= true (throws? (i/cond (i/= 1 (i/fresh)) (i/fresh) (i/+ 2 3) 4 :else 2)))))

    (testing "validates the return types are consistent"
      (is (= true (throws? (i/cond (i/fresh) 1 (i/fresh) #{} :else #{}))))
      (is (= false (throws? (i/cond (i/fresh) #{1 2 3} :else #{})))))

    (testing "evaluates"
      (let [a (i/fresh-int int-domain)
            b (i/fresh-int int-domain)]
        (is (= 0
               (get
                (i/satisfy
                 (i/and
                  (i/= a 9)
                  (i/cond (i/>= a 10) (i/= b 1) (i/= a 9) (i/= b 0) :else false)))
                b)))

        (is (= 1
               (get
                (i/satisfy
                 (i/and
                  (i/= a 11)
                  (i/cond (i/>= a 10) (i/= b 1) (i/= a 9) (i/= b 0) :else false)))
                b)))

        (is (= 10
               (get
                (i/satisfy
                 (i/and
                  (i/= a 11)
                  (i/= b (i/+ 5 (i/cond (i/= a 1) 6 (i/>= a 10) 5 :else 0)))))
                b)))))))

(deftest count-test
  (testing "count"
    (is (= 1
           (clojure.core/count
            (only-val
             (i/satisfy
              (i/= 1 (i/count (i/bind (range 10) (i/fresh)))))))))))

(deftest nth-test
  (testing "nth with literal index"
    (let [a (i/fresh-int int-domain)
          b (i/fresh-int int-domain)
          c (i/fresh-int int-domain)]
      (is (= 10
             (get
              (i/satisfy
               (i/and
                (i/= a 10)
                (i/= b 20)
                (i/= c (i/nth [a b] 0))))
              c)))
      (is (= 20
             (get
              (i/satisfy
               (i/and
                (i/= a 10)
                (i/= b 20)
                (i/= c (i/nth [a b] 1))))
              c)))))

  (testing "nth with variable index"
    (let [a (i/fresh-int int-domain)
          idx (i/fresh-int (range 3))
          elems [10 20 30]]
      (is (= 20
             (get
              (i/satisfy
               (i/and
                (i/= idx 1)
                (i/= a (i/nth elems idx))))
              a)))))

  (testing "nth single element"
    (is (= 42
           (only-val
            (i/satisfy (i/= (i/nth [42] 0) (i/fresh-int int-domain)))))))

  (testing "nth with decision elements and variable index"
    (let [elems (vec (repeatedly 4 #(i/fresh-int int-domain)))
          idx (i/fresh-int (range 4))
          result (i/fresh-int int-domain)
          solution (i/satisfy
                    (apply i/and
                           (i/= idx 2)
                           (i/= result (i/nth elems idx))
                           (i/= (nth elems 2) 77)
                           (for [i (range 4)]
                             (i/= (nth elems i) (+ (* i 11) 55)))))]
      (is (= 77 (get solution result))))))

(deftest abs-test
  (testing "abs with decisions"
    (let [x (i/fresh-int (range -100 101))
          y (i/fresh-int (range 101))]
      (is (= 7 (get (i/satisfy (i/and (i/= x -7) (i/= y (i/abs x)))) y)))
      (is (= 5 (get (i/satisfy (i/and (i/= x 5) (i/= y (i/abs x)))) y)))
      (is (= 0 (get (i/satisfy (i/and (i/= x 0) (i/= y (i/abs x)))) y)))))

  (testing "abs with literals evaluates in Clojure"
    (is (= 7 (i/abs -7)))
    (is (= 5 (i/abs 5)))
    (is (= 0 (i/abs 0)))))

(deftest all-different-test
  (testing "all-different constraint"
    (let [a (i/fresh-int (range 5))
          b (i/fresh-int (range 5))
          c (i/fresh-int (range 5))
          solution (i/satisfy (i/all-different a b c))
          vals (map solution [a b c])]
      (is (= 3 (count (distinct vals))))))

  (testing "all-different with literals evaluates in Clojure"
    (is (true? (i/all-different 1 2 3)))
    (is (false? (i/all-different 1 2 1)))))

(deftest pow-test
  (testing "pow with literals evaluates in Clojure"
    (is (= 8 (i/pow 2 3)))
    (is (= 1 (i/pow 5 0)))
    (is (= 1024 (i/pow 2 10)))
    (is (= 1 (i/pow 1 100))))

  (testing "pow with decisions"
    (let [x (i/fresh-int (range 1 11))
          y (i/fresh-int (range 101))]
      (is (= 8 (get (i/satisfy (i/and (i/= x 2) (i/= y (i/pow x 3)))) y)))
      (is (= 27 (get (i/satisfy (i/and (i/= x 3) (i/= y (i/pow x 3)))) y)))))

  (testing "pow with variable exponent"
    (let [base (i/fresh-int (range 1 11))
          exp  (i/fresh-int (range 5))
          result (i/fresh-int (range 10000))]
      (is (= 16 (get (i/satisfy (i/and (i/= base 2) (i/= exp 4) (i/= result (i/pow base exp)))) result)))))

  (testing "pow validates against Clojure evaluation"
    (let [x (i/fresh-int (range 1 6))
          y (i/fresh-int (range 4))
          r (i/fresh-int (range 10000))
          solution (i/satisfy (i/and (i/= x 3) (i/= y 3) (i/= r (i/pow x y))))]
      (is (= 27 (get solution r)))
      (is (i/validate-solution
           (i/and (i/= x 3) (i/= y 3) (i/= r (i/pow x y)))
           solution)))))

(deftest mod-rem-test
  (testing "mod and rem"
    (is (some?
         (let [n 3]
           (->> (for [x (concat (range (clojure.core/- 0 n) 0) (range 1 (clojure.core/inc n)))
                      y (concat (range (clojure.core/- 0 n) 0) (range 1 (clojure.core/inc n)))
                      :let [a (i/fresh-int int-domain)
                            b (i/fresh-int int-domain)]]
                  (i/and
                   (i/= a x)
                   (i/= b y)
                   (i/= (clojure.core/rem x y) (i/rem a b))
                   (i/= (clojure.core/mod x y) (i/mod a b))))
                (apply i/and)
                (i/satisfy)))))))
