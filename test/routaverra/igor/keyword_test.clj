(ns routaverra.igor.keyword-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.api :as api]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.utils.test :refer [throws?]]))

;; ============================================================
;; Ground behavior (constant folding)
;; ============================================================

(deftest keyword-ground-equality-test
  (testing "keyword equality folds"
    (is (true? (i/= :red :red)))
    (is (false? (i/= :red :blue)))))

(deftest keyword-ground-not-equals-test
  (testing "keyword inequality folds"
    (is (true? (i/not= :red :blue)))
    (is (false? (i/not= :red :red)))))

(deftest keyword-ground-set-contains-test
  (testing "keyword set membership folds"
    (is (true? (i/contains? #{:red :blue :green} :red)))
    (is (false? (i/contains? #{:red :blue :green} :yellow)))))

;; ============================================================
;; Solver tests
;; ============================================================

(deftest keyword-basic-solver-test
  (testing "solver assigns keyword from domain"
    (let [x (i/fresh-keyword #{:red :blue :green})
          sol (i/satisfy (i/= x :red))]
      (is (= :red (get sol x))))))

(deftest keyword-not-equals-solver-test
  (testing "solver respects keyword inequality"
    (let [x (i/fresh-keyword #{:red :blue})
          sol (i/satisfy (i/not= x :red))]
      (is (= :blue (get sol x))))))

(deftest keyword-multiple-domains-test
  (testing "two decisions with different keyword subsets share one enum"
    (let [x (i/fresh-keyword #{:red :blue :green})
          y (i/fresh-keyword #{:blue :green :yellow})
          sol (i/satisfy (i/and (i/= x :blue) (i/= y :green)))]
      (is (= :blue (get sol x)))
      (is (= :green (get sol y))))))

(deftest keyword-all-different-test
  (testing "all-different works with keyword decisions"
    (let [x (i/fresh-keyword #{:a :b :c})
          y (i/fresh-keyword #{:a :b :c})
          z (i/fresh-keyword #{:a :b :c})
          sol (i/satisfy (i/all-different x y z))]
      (is (= #{:a :b :c} (set [(get sol x) (get sol y) (get sol z)]))))))

;; ============================================================
;; Namespaced keywords
;; ============================================================

(deftest namespaced-keyword-test
  (testing "namespaced keywords are distinct from non-namespaced"
    (let [x (i/fresh-keyword #{:red :my-ns/red})
          sol (i/satisfy (i/= x :my-ns/red))]
      (is (= :my-ns/red (get sol x))))))

(deftest different-namespaces-test
  (testing "keywords from different namespaces are distinct"
    (let [x (i/fresh-keyword #{:ns-a/x :ns-b/x})
          sol (i/satisfy (i/= x :ns-a/x))]
      (is (= :ns-a/x (get sol x))))))

(deftest namespaced-keyword-ground-test
  (testing "ground equality distinguishes namespaces"
    (is (false? (i/= :ns-a/x :ns-b/x)))
    (is (true? (i/= :ns-a/x :ns-a/x)))))

;; ============================================================
;; Set of keywords
;; ============================================================

(deftest keyword-set-test
  (testing "fresh-set with keyword domain"
    (let [s (i/fresh-set #{:a :b :c})
          sol (i/satisfy (i/and (i/contains? s :a)
                                (i/contains? s :b)))]
      (is (contains? (get sol s) :a))
      (is (contains? (get sol s) :b)))))

;; ============================================================
;; Validation errors
;; ============================================================

(deftest mixed-domain-error-test
  (testing "mixed keyword/integer domain throws"
    (is (= true (throws? (i/fresh-set #{:red 3}))))
    (is (= true (throws? (i/fresh-keyword #{:red 3}))))))

(deftest type-mismatch-error-test
  (testing "comparing keyword to integer throws at validation"
    (is (= true (throws?
                 (i/satisfy (i/= (i/fresh-keyword #{:a :b})
                                 (i/fresh-int (range 10)))))))))

;; ============================================================
;; README examples
;; ============================================================

(deftest readme-graph-coloring-test
  (testing "graph coloring with keywords"
    (let [edges [[0 1] [0 2] [1 2] [1 3] [2 4] [3 4]]
          colors (vec (repeatedly 5 #(i/fresh-keyword #{:red :green :blue})))
          sol (i/satisfy
               (->> edges
                    (map (fn [[u v]] (i/not= (nth colors u) (nth colors v))))
                    (apply i/and)))
          result (mapv sol colors)]
      ;; All colors are valid keywords
      (is (every? #{:red :green :blue} result))
      ;; No adjacent pair shares a color
      (is (every? (fn [[u v]] (not= (nth result u) (nth result v))) edges)))))

(deftest readme-feature-selection-test
  (testing "feature selection with keyword sets"
    (let [features (i/fresh-set #{:wifi :bluetooth :nfc :gps :lte})
          sol (i/satisfy (i/and (i/contains? features :wifi)
                                (i/implies (i/contains? features :lte)
                                           (i/contains? features :gps))
                                (i/<= (i/count features) 3)
                                (i/contains? features :lte)))
          result (sol features)]
      (is (contains? result :wifi))
      (is (contains? result :lte))
      (is (contains? result :gps))
      (is (<= (count result) 3)))))

(deftest readme-configuration-test
  (testing "configuration with multiple keyword domains"
    (let [color (i/fresh-keyword #{:red :blue :black})
          trim  (i/fresh-keyword #{:sport :luxury :base})
          sol (i/satisfy (i/and (i/implies (i/= trim :sport) (i/not= color :blue))
                                (i/implies (i/= trim :luxury) (i/= color :black))
                                (i/= trim :sport)))]
      (is (= :sport (sol trim)))
      (is (not= :blue (sol color))))))

;; ============================================================
;; Naming scheme
;; ============================================================

(deftest keyword-mzn-naming-test
  (testing "keyword->mzn-name produces valid identifiers"
    (is (= "kw_red" (api/keyword->mzn-name :red)))
    (is (= "kw_my_color" (api/keyword->mzn-name :my-color)))
    (is (= "kw_ns__foo" (api/keyword->mzn-name :ns/foo)))
    (is (= "kw_my_ns__my_val" (api/keyword->mzn-name :my-ns/my-val)))))
