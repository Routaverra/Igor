(ns igor.terms.set-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.utils.test :refer [only-val throws?]]))

(deftest intersection-test
  (testing "intersection"
    (is (= #{4 5 6}
           (let [res (i/fresh-set (range 12))
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6})
                      (i/= b #{4 5 6 7 8 9})
                      (i/= res (i/intersection a b))))
              res))))))

(deftest difference-test
  (testing "difference"
    (is (= #{1 2 3}
           (let [res (i/fresh-set (range 12))
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6})
                      (i/= b #{4 5 6 7 8 9})
                      (i/= res (i/difference a b))))
              res))))))

(deftest symdiff-test
  (testing "symdiff"
    (is (= #{1 2 3 7 8 9}
           (let [res (i/fresh-set (range 12))
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6})
                      (i/= b #{4 5 6 7 8 9})
                      (i/= res (i/sym-diff a b))))
              res))))))

(deftest union-test
  (testing "union"
    (is (= #{1 2 3 4 5 6 7 8 9}
           (let [res (i/fresh-set (range 12))
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6})
                      (i/= b #{4 5 6 7 8 9})
                      (i/= res (i/union a b))))
              res))))))

(deftest subset?-test
  (testing "subset?"
    (is (= false
           (let [res (i/fresh "res")
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6})
                      (i/= b #{4 5 6 7 8 9})
                      (i/= res (i/subset? a b))))
              res))))

    (is (= true
           (let [res (i/fresh "res")
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3})
                      (i/= b #{1 2 3 4 5 6 7 8 9})
                      (i/= res (i/subset? a b))))
              res))))))

(deftest superset?-test
  (testing "superset?"
    (is (= false
           (let [res (i/fresh "res")
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6})
                      (i/= b #{4 5 6 7 8 9})
                      (i/= res (i/superset? a b))))
              res))))

    (is (= true
           (let [res (i/fresh "res")
                 a (i/fresh-set (range 12))
                 b (i/fresh-set (range 12))]
             (get
              (i/satisfy
               (i/and (i/= a #{1 2 3 4 5 6 7 8 9})
                      (i/= b #{1 2 3})
                      (i/= res (i/superset? a b))))
              res))))))
