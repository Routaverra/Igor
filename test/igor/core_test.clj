(ns igor.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.utils.test :refer [only-val throws?]]))

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
