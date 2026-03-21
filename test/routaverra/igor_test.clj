(ns routaverra.igor-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.utils.test :refer [only-val throws?]]))

(deftest and-does-not-stack-overflow-test
  (is (= false
         (throws?
          (let [n 15
                int-domain (range -100 101)]
            (->> (for [x (range n)
                       y (range n)
                       :let [a (i/fresh-int int-domain)
                             b (i/fresh-int int-domain)]]
                   (i/and
                    (i/= a x)
                    (i/= b y)
                    (i/= (i/+ a b) (clojure.core/+ x y))))
                 (apply i/and)))))))

(deftest unbound-numeric-throws-test
  (testing "unbound numeric decision throws at solve time"
    (is (= true
           (throws?
            (i/satisfy (i/= (i/fresh) 42)))))))

(deftest late-bind-numeric-test
  (testing "bare (i/fresh) with later (i/bind) works for numerics"
    (let [x (i/fresh)]
      (is (= 42
             (only-val
              (i/satisfy
               (i/= (i/bind (range 101) x) 42))))))))

(deftest minimize-basic-test
  (testing "minimize finds the minimum value"
    (let [x (i/fresh-int (range 1 11))
          solution (i/minimize x (i/>= x 0))]
      (is (= 1 (get solution x))))))

(deftest minimize-equivalence-test
  (testing "minimize gives same result as maximize-negated"
    (let [x (i/fresh-int (range 1 11))
          y (i/fresh-int (range 1 11))
          constraint (i/<= (i/+ x y) 15)
          sol-min (i/minimize (i/+ x y) constraint)
          sol-neg (i/maximize (i/- 0 (i/+ x y)) constraint)]
      (is (= (clojure.core/+ (get sol-min x) (get sol-min y))
             (clojure.core/+ (get sol-neg x) (get sol-neg y)))))))

(deftest maximize-unchanged-test
  (testing "maximize still works correctly"
    (let [x (i/fresh-int (range 1 11))
          solution (i/maximize x (i/>= x 0))]
      (is (= 10 (get solution x))))))

(deftest satisfy-data-test
  (let [x (i/fresh-int (range 10))
        y (i/fresh-int (range 10))
        constraint (i/and (i/= x 3) (i/= y 7))]
    (testing "flat vector"
      (is (= [3 7] (i/satisfy-data constraint [x y]))))
    (testing "nested structure"
      (is (= {:vals [3 7] :label "test"}
             (i/satisfy-data constraint {:vals [x y] :label "test"}))))))

(deftest satisfy-data-unsatisfiable-test
  (let [x (i/fresh-int #{1 2 3})]
    (is (nil? (i/satisfy-data (i/and (i/> x 5) (i/< x 0)) [x])))))

(deftest satisfy-data-passthrough-test
  (testing "unconstrained decisions pass through"
    (let [x (i/fresh-int (range 10))
          orphan (i/fresh-int (range 10))
          result (i/satisfy-data (i/= x 5) [x orphan])]
      (is (= 5 (first result)))
      (is (i/dithered? (second result))))))
