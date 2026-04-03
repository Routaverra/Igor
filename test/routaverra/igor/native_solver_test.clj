(ns routaverra.igor.native-solver-test
  "Tests that exercise the native solver backend and cross-validate against MiniZinc."
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.utils.test :refer [only-val throws?]]))

(def native {:solver :native})

(defn verify-both-backends
  "Solve with both backends and verify both produce valid solutions."
  [constraint & {:keys [all?]}]
  (let [mzn-result (if all?
                     (i/satisfy-all constraint)
                     (i/satisfy constraint))
        native-result (if all?
                        (i/satisfy-all constraint native)
                        (i/satisfy constraint native))]
    (if all?
      (do
        (is (= (count mzn-result) (count native-result))
            "Both backends should find the same number of solutions")
        (doseq [sol native-result]
          (is (true? (i/validate-solution constraint sol))
              "Each native solution must be valid")))
      (do
        (is (= (some? mzn-result) (some? native-result))
            "Both backends should agree on satisfiability")
        (when native-result
          (is (true? (i/validate-solution constraint native-result))
              "Native solution must be valid"))))))

;; ============================================================
;; Basic constraint satisfaction
;; ============================================================

(deftest simple-equality-test
  (testing "x = 5"
    (let [x (i/fresh-int (range 10))
          sol (i/satisfy (i/= x 5) native)]
      (is (= 5 (get sol x))))))

(deftest two-variable-equality-test
  (testing "x = y"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          sol (i/satisfy (i/and (i/= x y) (i/= x 3)) native)]
      (is (= 3 (get sol x)))
      (is (= 3 (get sol y))))))

(deftest greater-than-test
  (testing "x > y with bounds"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          sol (i/satisfy (i/and (i/> x 5) (i/< y 3)) native)]
      (is (> (get sol x) 5))
      (is (< (get sol y) 3)))))

(deftest unsatisfiable-test
  (testing "contradictory constraints return nil"
    (let [x (i/fresh-int #{1 2 3})]
      (is (nil? (i/satisfy (i/and (i/> x 5) (i/< x 0)) native))))))

;; ============================================================
;; Arithmetic
;; ============================================================

(deftest addition-test
  (testing "z = x + y"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          sol (i/satisfy (i/and (i/= x 3) (i/= y 4)
                               (i/= (i/+ x y) 7)) native)]
      (is (= 3 (get sol x)))
      (is (= 4 (get sol y))))))

(deftest multiplication-test
  (testing "z = x * y"
    (let [x (i/fresh-int (range 1 10))
          y (i/fresh-int (range 1 10))
          sol (i/satisfy (i/and (i/= x 3) (i/= (i/* x y) 12)) native)]
      (is (= 3 (get sol x)))
      (is (= 4 (get sol y))))))

(deftest subtraction-test
  (testing "z = x - y"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          sol (i/satisfy (i/and (i/= x 7) (i/= (i/- x y) 3)) native)]
      (is (= 7 (get sol x)))
      (is (= 4 (get sol y))))))

;; ============================================================
;; All-different
;; ============================================================

(deftest all-different-test
  (testing "all-different with 3 variables"
    (let [x (i/fresh-int #{1 2 3})
          y (i/fresh-int #{1 2 3})
          z (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/all-different x y z) native)]
      (is (= #{1 2 3} (set [(get sol x) (get sol y) (get sol z)]))))))

;; ============================================================
;; Satisfy-all
;; ============================================================

(deftest satisfy-all-test
  (testing "find all solutions"
    (let [x (i/fresh-int #{1 2 3})
          sols (i/satisfy-all (i/>= x 1) native)]
      (is (= 3 (count sols)))
      (is (= #{1 2 3} (set (map #(get % x) sols)))))))

;; ============================================================
;; Optimization
;; ============================================================

(deftest minimize-test
  (testing "minimize finds minimum"
    (let [x (i/fresh-int (range 1 11))
          sol (i/minimize x (i/>= x 0) native)]
      (is (= 1 (get sol x))))))

(deftest maximize-test
  (testing "maximize finds maximum"
    (let [x (i/fresh-int (range 1 11))
          sol (i/maximize x (i/>= x 0) native)]
      (is (= 10 (get sol x))))))

(deftest minimize-with-constraints-test
  (testing "minimize with additional constraints"
    (let [x (i/fresh-int (range 1 11))
          y (i/fresh-int (range 1 11))
          sol (i/minimize (i/+ x y) (i/and (i/>= x 3) (i/>= y 4)) native)]
      (is (= 3 (get sol x)))
      (is (= 4 (get sol y))))))

;; ============================================================
;; Boolean / logical
;; ============================================================

(deftest boolean-and-test
  (testing "boolean AND"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          sol (i/satisfy (i/and (i/= x 3) (i/= y 7)) native)]
      (is (= 3 (get sol x)))
      (is (= 7 (get sol y))))))

(deftest or-test
  (testing "or selects one alternative"
    (let [x (i/fresh-int (range 50))
          sol (i/satisfy (i/or (i/= x 3) (i/= x 7)) native)]
      (is (contains? #{3 7} (get sol x))))))

(deftest or-with-arithmetic-test
  (testing "or combined with arithmetic"
    (let [x (i/fresh-int (range 50))
          y (i/fresh-int (range 50))
          sol (i/satisfy (i/and
                          (i/or (i/= x 5) (i/= x 10))
                          (i/= y (i/* x 2))) native)]
      (is (= (* 2 (get sol x)) (get sol y)))
      (is (contains? #{5 10} (get sol x))))))

;; ============================================================
;; If-then-else
;; ============================================================

(deftest if-test
  (testing "if-then-else"
    (let [x (i/fresh-int (range 20))
          y (i/fresh-int (range 20))
          sol (i/satisfy (i/and (i/= x 5)
                               (i/= y (i/if (i/> x 3) 10 20))) native)]
      (is (= 5 (get sol x)))
      (is (= 10 (get sol y))))))

;; ============================================================
;; Cross-validation against MiniZinc
;; ============================================================

(deftest cross-validate-simple-test
  (let [x (i/fresh-int (range 10))
        y (i/fresh-int (range 10))]
    (verify-both-backends (i/and (i/= x 3) (i/= y 7)))))

(deftest cross-validate-all-different-test
  (let [x (i/fresh-int #{1 2 3})
        y (i/fresh-int #{1 2 3})
        z (i/fresh-int #{1 2 3})]
    (verify-both-backends (i/all-different x y z) :all? true)))

(deftest cross-validate-satisfy-all-test
  (let [x (i/fresh-int #{1 2 3})]
    (verify-both-backends (i/>= x 1) :all? true)))

;; ============================================================
;; Classic CSPs (native backend)
;; ============================================================

(deftest four-queens-native-test
  (testing "4-Queens via native solver"
    (let [n 4
          queens (vec (repeatedly n #(i/fresh-int (range n))))
          constraints (i/and
                       (apply i/all-different queens)
                       (apply i/and
                              (for [i (range n)
                                    j (range (inc i) n)]
                                (i/and
                                 (i/not= (i/+ (nth queens i) (- j i))
                                         (nth queens j))
                                 (i/not= (i/- (nth queens i) (- j i))
                                         (nth queens j))))))
          sol (i/satisfy constraints native)]
      (is (some? sol))
      (is (true? (i/validate-solution constraints sol))))))

(deftest four-queens-all-solutions-test
  (testing "4-Queens all solutions count"
    (let [n 4
          queens (vec (repeatedly n #(i/fresh-int (range n))))
          constraints (i/and
                       (apply i/all-different queens)
                       (apply i/and
                              (for [i (range n)
                                    j (range (inc i) n)]
                                (i/and
                                 (i/not= (i/+ (nth queens i) (- j i))
                                         (nth queens j))
                                 (i/not= (i/- (nth queens i) (- j i))
                                         (nth queens j))))))
          sols (i/satisfy-all constraints native)]
      (is (= 2 (count sols))))))

(deftest send-more-money-native-test
  (testing "SEND+MORE=MONEY via native solver"
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
          solution (i/satisfy constraint native)]
      (is (some? solution))
      (when solution
        (let [s* (get solution s) e* (get solution e) n* (get solution n) d* (get solution d)
              m* (get solution m) o* (get solution o) r* (get solution r) y* (get solution y)
              send* (+ (* s* 1000) (* e* 100) (* n* 10) d*)
              more* (+ (* m* 1000) (* o* 100) (* r* 10) e*)
              money* (+ (* m* 10000) (* o* 1000) (* n* 100) (* e* 10) y*)]
          (is (= (+ send* more*) money*))
          (is (pos? s*))
          (is (pos? m*))
          (is (= 8 (count (distinct [s* e* n* d* m* o* r* y*])))))))))

;; ============================================================
;; Keyword support
;; ============================================================

(deftest keyword-basic-native-test
  (testing "keyword variable with native solver"
    (let [x (i/fresh-keyword [:red :green :blue])
          sol (i/satisfy (i/= x :red) native)]
      (is (= :red (get sol x))))))

(deftest keyword-not-equals-native-test
  (testing "keyword not-equals"
    (let [x (i/fresh-keyword [:red :green :blue])
          sol (i/satisfy (i/and (i/not= x :red) (i/not= x :green)) native)]
      (is (= :blue (get sol x))))))

(deftest keyword-all-different-native-test
  (testing "keyword all-different"
    (let [x (i/fresh-keyword [:red :green :blue])
          y (i/fresh-keyword [:red :green :blue])
          z (i/fresh-keyword [:red :green :blue])
          sols (i/satisfy-all (i/all-different x y z) native)]
      ;; 3! = 6 permutations
      (is (= 6 (count sols)))
      (doseq [sol sols]
        (is (= 3 (count (distinct [(get sol x) (get sol y) (get sol z)]))))))))

;; ============================================================
;; Table constraint (Phase 4)
;; ============================================================

;; ============================================================
;; Set operations (Phase 5)
;; ============================================================

(deftest set-intersection-native-test
  (let [res (i/fresh-set (range 12))
        a (i/fresh-set (range 12))
        b (i/fresh-set (range 12))
        sol (i/satisfy (i/and (i/= a #{1 2 3 4 5 6}) (i/= b #{4 5 6 7 8 9})
                             (i/= res (i/intersection a b))) native)]
    (is (= #{4 5 6} (get sol res)))))

(deftest set-difference-native-test
  (let [res (i/fresh-set (range 12))
        a (i/fresh-set (range 12))
        b (i/fresh-set (range 12))
        sol (i/satisfy (i/and (i/= a #{1 2 3 4 5 6}) (i/= b #{4 5 6 7 8 9})
                             (i/= res (i/difference a b))) native)]
    (is (= #{1 2 3} (get sol res)))))

(deftest set-union-native-test
  (let [res (i/fresh-set (range 12))
        a (i/fresh-set (range 12))
        b (i/fresh-set (range 12))
        sol (i/satisfy (i/and (i/= a #{1 2 3 4 5 6}) (i/= b #{4 5 6 7 8 9})
                             (i/= res (i/union a b))) native)]
    (is (= #{1 2 3 4 5 6 7 8 9} (get sol res)))))

(deftest set-partition-native-test
  (let [domain (range 6)
        a (i/fresh-set domain)
        b (i/fresh-set domain)
        sol (i/satisfy (i/and (i/= (i/union a b) (set domain))
                             (i/= (i/count (i/intersection a b)) 0)
                             (i/= (i/count a) 3) (i/= (i/count b) 3)) native)]
    (is (some? sol))
    (is (= (set domain) (clojure.set/union (get sol a) (get sol b))))
    (is (empty? (clojure.set/intersection (get sol a) (get sol b))))))

(deftest set-subset-chain-native-test
  (let [domain (range 8)
        a (i/fresh-set domain)
        b (i/fresh-set domain)
        c (i/fresh-set domain)
        sol (i/satisfy (i/and (i/= (i/count a) 2) (i/= (i/count b) 4) (i/= (i/count c) 6)
                             (i/subset? a b) (i/subset? b c)) native)]
    (is (some? sol))
    (is (clojure.set/subset? (get sol a) (get sol b)))
    (is (clojure.set/subset? (get sol b) (get sol c)))))

(deftest set-covering-native-test
  (let [domain (range 10)
        s (i/fresh-set domain)
        sol (i/satisfy (i/and (i/contains? s 1) (i/contains? s 3)
                             (i/contains? s 5) (i/contains? s 7)
                             (i/= (i/count s) 4)) native)]
    (is (= 4 (count (get sol s))))
    (is (every? (get sol s) [1 3 5 7]))))

;; ============================================================
;; Table constraint (Phase 4)
;; ============================================================

(deftest table-basic-native-test
  (testing "table constrains vars to one of the allowed tuples"
    (let [x1 (i/fresh-int (range 10))
          x2 (i/fresh-int (range 10))
          x3 (i/fresh-int (range 10))
          allowed [[1 2 3] [4 5 6] [7 8 9]]
          result (i/solve (i/table [x1 x2 x3] allowed) [x1 x2 x3])]
      (is (some? result))
      (is (some #{result} allowed)))))

(deftest table-all-solutions-native-test
  (testing "table finds all matching tuples"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          allowed [[1 2] [3 4] [5 6]]
          sols (i/satisfy-all (i/table [x y] allowed) native)]
      (is (= 3 (count sols)))
      (is (= (set allowed)
             (set (map (fn [sol] [(get sol x) (get sol y)]) sols)))))))

(deftest table-cross-validate-test
  (testing "table gives same results on both backends"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          z (i/fresh-int (range 10))
          allowed [[1 2 3] [4 5 6] [7 8 9]]]
      (verify-both-backends (i/table [x y z] allowed) :all? true))))
