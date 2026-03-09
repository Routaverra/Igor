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
