(ns routaverra.igor.soft-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]))

(deftest soft-satisfiable-test
  (testing "soft constraint that can be satisfied has zero violation"
    (let [x (i/fresh-int (range 10))
          s (i/soft (i/> x 5) 100)
          sol (i/satisfy (i/and s (i/= x 7)))]
      (is (= 7 (get sol x)))
      (is (= 0 (get sol (i/violation s)))))))

(deftest soft-violated-test
  (testing "soft constraint that must be violated has penalty cost"
    (let [x (i/fresh-int (range 10))
          s (i/soft (i/> x 100) 50)
          sol (i/satisfy s)]
      ;; x can't be > 100, so constraint is violated
      (is (= 50 (get sol (i/violation s)))))))

(deftest soft-minimize-test
  (testing "minimizing violation cost prefers satisfying soft constraints"
    (let [x (i/fresh-int (range 20))
          s (i/soft (i/> x 5) 100)
          sol (i/minimize (i/violation s) s)]
      ;; Solver should satisfy the constraint to get cost 0
      (is (> (get sol x) 5))
      (is (= 0 (get sol (i/violation s)))))))

(deftest soft-multiple-test
  (testing "multiple soft constraints with total cost minimization"
    (let [x (i/fresh-int (range 10))
          ;; Prefer x > 3 (penalty 10 if violated)
          s1 (i/soft (i/> x 3) 10)
          ;; Prefer x < 2 (penalty 5 if violated)
          s2 (i/soft (i/< x 2) 5)
          ;; These conflict — can't have x > 3 AND x < 2
          ;; Minimizing total cost should violate s2 (cheaper) and satisfy s1
          total-cost (i/+ (i/violation s1) (i/violation s2))
          sol (i/minimize total-cost (i/and s1 s2))]
      (is (> (get sol x) 3) "satisfies the more expensive constraint")
      (is (= 0 (get sol (i/violation s1))) "expensive constraint satisfied")
      (is (= 5 (get sol (i/violation s2))) "cheaper constraint violated"))))

(deftest soft-with-hard-constraints-test
  (testing "soft constraints compose with hard constraints"
    (let [x (i/fresh-int (range 10))
          ;; Hard: x must be even
          hard (i/even? x)
          ;; Soft: prefer x > 7 (penalty 100)
          s (i/soft (i/> x 7) 100)
          sol (i/minimize (i/violation s) (i/and hard s))]
      ;; x must be even AND preferably > 7 — best is 8
      (is (= 8 (get sol x)))
      (is (= 0 (get sol (i/violation s)))))))

(deftest soft-impossible-hard-constraint-test
  (testing "hard constraint forces violation of soft constraint"
    (let [x (i/fresh-int (range 10))
          ;; Hard: x = 3
          hard (i/= x 3)
          ;; Soft: prefer x > 5 (penalty 50)
          s (i/soft (i/> x 5) 50)
          sol (i/satisfy (i/and hard s))]
      (is (= 3 (get sol x)))
      (is (= 50 (get sol (i/violation s)))))))

(deftest soft-violation-in-objective-test
  (testing "violation variable works in arithmetic expressions"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          s1 (i/soft (i/> x 8) 10)
          s2 (i/soft (i/> y 8) 20)
          ;; Weighted objective: violation cost + distance from 5
          total (i/+ (i/violation s1) (i/violation s2))
          sol (i/minimize total (i/and s1 s2 (i/<= (i/+ x y) 12)))]
      ;; x + y <= 12, so both can't be > 8
      ;; Violating s1 (cost 10) is cheaper than s2 (cost 20)
      (is (= 10 (get sol (i/violation s1))) "cheaper one violated")
      (is (= 0 (get sol (i/violation s2))) "expensive one satisfied")
      (is (> (get sol y) 8)))))

(deftest soft-variable-penalty-test
  (testing "penalty can be a decision variable"
    (let [x (i/fresh-int (range 10))
          penalty (i/fresh-int (range 1 20))
          s (i/soft (i/> x 100) penalty)
          ;; x can't be > 100, so constraint is always violated
          ;; violation cost = penalty value
          sol (i/minimize (i/+ penalty (i/violation s))
                          (i/and s (i/= penalty 7)))]
      (is (= 7 (get sol (i/violation s))))
      (is (= 7 (get sol penalty))))))

(deftest readme-soft-scheduling-test
  (testing "README scheduling preferences example"
    (let [start (i/fresh-int (range 8 18))
          duration (i/fresh-int #{1 2 4})
          early (i/soft (i/< start 10) 20)
          short (i/soft (i/<= duration 1) 10)
          hard (i/<= (i/+ start duration) 12)
          total-cost (i/+ (i/violation early) (i/violation short))
          sol (i/minimize total-cost (i/and hard early short))]
      (is (<= (+ (sol start) (sol duration)) 12) "hard constraint satisfied")
      (is (= 0 (get sol (i/violation early))) "early preference satisfied")
      (is (= 0 (get sol (i/violation short))) "short preference satisfied"))))

(deftest soft-variable-penalty-minimize-test
  (testing "solver trades off variable penalty against other objectives"
    (let [x (i/fresh-int (range 10))
          penalty (i/fresh-int #{5 15})
          ;; Soft: prefer x > 8, but penalty depends on another variable
          s (i/soft (i/> x 8) penalty)
          ;; Force x = 3, so soft constraint is violated
          ;; Solver should pick penalty = 5 to minimize cost
          sol (i/minimize (i/violation s)
                          (i/and s (i/= x 3)))]
      (is (= 3 (get sol x)))
      (is (= 5 (get sol penalty)))
      (is (= 5 (get sol (i/violation s)))))))
