(ns routaverra.igor.cache-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.core.async :as async]
            [routaverra.igor :as i]
            [routaverra.igor.cache :as cache]
            [clojure.java.io :as io]))

(defn delete-recursively [f]
  (when (.exists (io/file f))
    (doseq [child (reverse (file-seq (io/file f)))]
      (.delete child))))

(defn with-temp-cache [f]
  (let [dir (str "target/test-cache-" (System/nanoTime))]
    (binding [cache/*enabled* true
              cache/*cache-dir* dir]
      (try
        (f)
        (finally
          (delete-recursively dir))))))

(use-fixtures :each with-temp-cache)

;; --- canonical-form: structural identity ---

(deftest canonical-form-stable-across-evals-test
  (testing "structurally identical problems produce the same canonical form"
    (let [form-a (let [x (i/fresh-int (range 10))
                       y (i/fresh-int (range 10))]
                   (cache/canonical-form (i/= (i/+ x y) 10)))
          form-b (let [x (i/fresh-int (range 10))
                       y (i/fresh-int (range 10))]
                   (cache/canonical-form (i/= (i/+ x y) 10)))]
      (is (= form-a form-b)))))

(deftest canonical-form-different-for-different-problems-test
  (testing "structurally different problems produce different canonical forms"
    (let [form-a (let [x (i/fresh-int (range 10))
                       y (i/fresh-int (range 10))]
                   (cache/canonical-form (i/= (i/+ x y) 10)))
          form-b (let [x (i/fresh-int (range 10))
                       y (i/fresh-int (range 10))]
                   (cache/canonical-form (i/= (i/- x y) 10)))]
      (is (not= form-a form-b)))))

(deftest canonical-form-shared-variable-test
  (testing "shared variable references map to the same canonical index"
    (let [x (i/fresh-int (range 10))
          form (cache/canonical-form (i/and (i/>= x 0) (i/<= x 9)))]
      (is (= form
             (let [y (i/fresh-int (range 10))]
               (cache/canonical-form (i/and (i/>= y 0) (i/<= y 9)))))))))

;; --- canonical-form: decision types and domains ---

(deftest canonical-form-int-different-domains-test
  (let [form-a (let [x (i/fresh-int (range 10))]
                 (cache/canonical-form (i/>= x 0)))
        form-b (let [x (i/fresh-int (range 20))]
                 (cache/canonical-form (i/>= x 0)))]
    (is (not= form-a form-b))))

(deftest canonical-form-int-same-domain-test
  (let [form-a (let [x (i/fresh-int #{1 2 3})]
                 (cache/canonical-form (i/>= x 0)))
        form-b (let [x (i/fresh-int #{1 2 3})]
                 (cache/canonical-form (i/>= x 0)))]
    (is (= form-a form-b))))

(deftest canonical-form-bool-test
  (let [form-a (let [b (i/fresh-bool)]
                 (cache/canonical-form (i/= b true)))
        form-b (let [b (i/fresh-bool)]
                 (cache/canonical-form (i/= b true)))]
    (is (= form-a form-b))))

(deftest canonical-form-bool-vs-int-test
  (let [form-bool (let [b (i/fresh-bool)]
                    (cache/canonical-form (i/= b true)))
        form-int (let [x (i/fresh-int (range 2))]
                   (cache/canonical-form (i/= x 1)))]
    (is (not= form-bool form-int))))

(deftest canonical-form-set-different-universe-test
  (let [form-a (let [s (i/fresh-set #{1 2 3})]
                 (cache/canonical-form (i/subset? s #{1 2 3})))
        form-b (let [s (i/fresh-set #{1 2 3 4 5})]
                 (cache/canonical-form (i/subset? s #{1 2 3 4 5})))]
    (is (not= form-a form-b))))

(deftest canonical-form-set-same-universe-test
  (let [form-a (let [s (i/fresh-set #{1 2 3})]
                 (cache/canonical-form (i/subset? s #{1 2 3})))
        form-b (let [s (i/fresh-set #{1 2 3})]
                 (cache/canonical-form (i/subset? s #{1 2 3})))]
    (is (= form-a form-b))))

(deftest canonical-form-keyword-different-domain-test
  (let [form-a (let [k (i/fresh-keyword [:red :green :blue])]
                 (cache/canonical-form (i/= k :red)))
        form-b (let [k (i/fresh-keyword [:red :green])]
                 (cache/canonical-form (i/= k :red)))]
    (is (not= form-a form-b))))

(deftest canonical-form-keyword-same-domain-test
  (let [form-a (let [k (i/fresh-keyword [:red :green :blue])]
                 (cache/canonical-form (i/= k :red)))
        form-b (let [k (i/fresh-keyword [:red :green :blue])]
                 (cache/canonical-form (i/= k :red)))]
    (is (= form-a form-b))))

(deftest canonical-form-int-vs-keyword-test
  (let [form-int (let [x (i/fresh-int #{1 2 3})]
                   (cache/canonical-form (i/= x 1)))
        form-kw (let [k (i/fresh-keyword [:a :b :c])]
                  (cache/canonical-form (i/= k :a)))]
    (is (not= form-int form-kw))))

;; --- cache key tests ---

(deftest cache-key-stable-test
  (let [key-a (let [x (i/fresh-int (range 10))
                    y (i/fresh-int (range 10))]
                (cache/cache-key (i/= (i/+ x y) 10) nil {}))
        key-b (let [x (i/fresh-int (range 10))
                    y (i/fresh-int (range 10))]
                (cache/cache-key (i/= (i/+ x y) 10) nil {}))]
    (is (= key-a key-b))))

(deftest cache-key-direction-matters-test
  (let [x (i/fresh-int (range 10))
        constraint (i/>= x 0)
        key-max (cache/cache-key constraint x {:direction :maximize})
        key-min (cache/cache-key constraint x {:direction :minimize})]
    (is (not= key-max key-min))))

(deftest cache-key-all-does-not-matter-test
  (let [x (i/fresh-int (range 10))
        constraint (i/>= x 0)
        key-single (cache/cache-key constraint nil {})
        key-all (cache/cache-key constraint nil {:all? true})]
    (is (= key-single key-all))))

;; --- encounter-ordered-decisions tests ---

(deftest encounter-order-matches-tree-structure-test
  (let [x (i/fresh-int (range 10))
        y (i/fresh-int (range 10))
        z (i/fresh-int (range 10))
        constraint (i/and (i/= z 1) (i/= x 2) (i/= y 3))
        ordered (cache/encounter-ordered-decisions constraint)]
    (is (= [z x y] ordered))))

(deftest encounter-order-deduplicates-test
  (let [x (i/fresh-int (range 10))
        constraint (i/and (i/>= x 0) (i/<= x 9))
        ordered (cache/encounter-ordered-decisions constraint)]
    (is (= [x] ordered))))

;; --- solution set semantics ---

(deftest satisfy-populates-cache-for-satisfy-all-test
  (testing "satisfy adds one solution; satisfy-all merges solver results with cache"
    (let [x (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/>= x 1))]
      (is (#{1 2 3} (get sol x))))
    (let [x (i/fresh-int #{1 2 3})
          sols (i/satisfy-all (i/>= x 1))]
      (is (= #{1 2 3} (set (map #(get % x) sols)))))))

(deftest satisfy-all-populates-cache-for-satisfy-test
  (testing "satisfy-all populates cache; subsequent satisfy returns from cache"
    (let [x (i/fresh-int #{1 2 3})]
      (i/satisfy-all (i/>= x 1)))
    (let [x (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/>= x 1))]
      (is (#{1 2 3} (get sol x))))))

;; --- open/closed semantics ---

(deftest satisfy-all-marks-complete-test
  (testing "satisfy-all without timeout marks cache as complete"
    (let [x (i/fresh-int #{1 2 3})]
      (i/satisfy-all (i/>= x 1)))
    ;; Check raw cache entry
    (let [x (i/fresh-int #{1 2 3})
          key (cache/cache-key (i/>= x 1) nil {})
          entry (cache/cache-read key)]
      (is (:complete? entry))
      (is (= 3 (clojure.core/count (:solutions entry)))))))

(deftest satisfy-does-not-mark-complete-test
  (testing "satisfy leaves cache incomplete — one solution found, not all"
    (let [x (i/fresh-int #{1 2 3})]
      (i/satisfy (i/>= x 1)))
    (let [x (i/fresh-int #{1 2 3})
          key (cache/cache-key (i/>= x 1) nil {})
          entry (cache/cache-read key)]
      (is (not (:complete? entry)))
      (is (= 1 (clojure.core/count (:solutions entry)))))))

(deftest complete-satisfy-all-skips-solver-test
  (testing "satisfy-all with complete cache returns cached set without re-solving"
    ;; First call: find all solutions, marks complete
    (let [x (i/fresh-int #{1 2 3})
          sols1 (i/satisfy-all (i/>= x 1))]
      (is (= 3 (clojure.core/count sols1))))
    ;; Second call: should return from cache (same result)
    (let [x (i/fresh-int #{1 2 3})
          sols2 (i/satisfy-all (i/>= x 1))]
      (is (= 3 (clojure.core/count sols2)))
      (is (= #{1 2 3} (set (map #(get % x) sols2)))))))

(deftest incomplete-cache-triggers-re-solve-test
  (testing "satisfy-all with incomplete cache (from satisfy) re-solves and completes"
    ;; satisfy adds one solution, incomplete
    (let [x (i/fresh-int #{1 2 3})]
      (i/satisfy (i/>= x 1)))
    ;; satisfy-all should re-solve because cache is not complete
    (let [x (i/fresh-int #{1 2 3})
          sols (i/satisfy-all (i/>= x 1))]
      (is (= #{1 2 3} (set (map #(get % x) sols))))
      ;; Now cache should be marked complete
      (let [key (cache/cache-key (i/>= x 1) nil {})
            entry (cache/cache-read key)]
        (is (:complete? entry))))))

(deftest timeout-satisfy-all-accumulates-test
  (testing "satisfy-all with timeout collects partial results; subsequent call merges"
    ;; We can't deterministically test timing, but we can verify that
    ;; calling satisfy-all twice produces at least as many solutions as once
    (let [make (fn [] (let [x (i/fresh-int #{1 2 3 4 5})] [x (i/>= x 1)]))
          [x1 c1] (make)
          sols1 (i/satisfy-all c1)
          count1 (clojure.core/count sols1)
          ;; Second call merges with cache
          [x2 c2] (make)
          sols2 (i/satisfy-all c2)
          count2 (clojure.core/count sols2)]
      (is (>= count2 count1))
      (is (= 5 count2)))))

;; --- end-to-end: satisfy (sync) ---

(deftest satisfy-cache-hit-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int (range 10))
                                y (i/fresh-int (range 10))
                                constraint (i/and (i/= x 3) (i/= y 7))
                                solution (i/satisfy constraint)]
                            [(get solution x) (get solution y)]))]
    (is (= [3 7] (solve-and-check)))
    (is (= [3 7] (solve-and-check)))))

(deftest satisfy-cache-miss-on-different-problem-test
  (let [x (i/fresh-int (range 10))
        sol-a (i/satisfy (i/= x 3))
        sol-b (i/satisfy (i/= x 7))]
    (is (= 3 (get sol-a x)))
    (is (= 7 (get sol-b x)))))

(deftest cache-disabled-test
  (let [x (i/fresh-int (range 10))
        constraint (i/= x 5)]
    (i/satisfy constraint)
    (binding [cache/*enabled* false]
      (is (= 5 (get (i/satisfy constraint) x))))))

(deftest clear-cache-test
  (testing "clear! removes all cached entries; subsequent solve re-runs solver"
    ;; Populate cache
    (let [x (i/fresh-int #{1 2 3})]
      (i/satisfy-all (i/>= x 1)))
    ;; Verify cache is populated
    (let [x (i/fresh-int #{1 2 3})
          key (cache/cache-key (i/>= x 1) nil {})]
      (is (some? (cache/cache-read key))))
    ;; Clear
    (cache/clear!)
    ;; Verify cache is empty
    (let [x (i/fresh-int #{1 2 3})
          key (cache/cache-key (i/>= x 1) nil {})]
      (is (nil? (cache/cache-read key))))
    ;; Solve still works after clear
    (let [x (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/>= x 1))]
      (is (#{1 2 3} (get sol x))))))

;; --- end-to-end: decision types ---

(deftest cache-bool-test
  (let [solve-and-check (fn []
                          (let [b (i/fresh-bool)
                                sol (i/satisfy (i/= b true))]
                            (get sol b)))]
    (is (= true (solve-and-check)))
    (is (= true (solve-and-check)))))

(deftest cache-set-test
  (let [solve-and-check (fn []
                          (let [s (i/fresh-set #{1 2 3})
                                sol (i/satisfy (i/= s #{1 2}))]
                            (get sol s)))]
    (is (= #{1 2} (solve-and-check)))
    (is (= #{1 2} (solve-and-check)))))

(deftest cache-keyword-test
  (let [solve-and-check (fn []
                          (let [k (i/fresh-keyword [:red :green :blue])
                                sol (i/satisfy (i/= k :red))]
                            (get sol k)))]
    (is (= :red (solve-and-check)))
    (is (= :red (solve-and-check)))))

;; --- end-to-end: maximize / minimize ---

(deftest cache-maximize-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int (range 1 11))
                                solution (i/maximize x (i/>= x 0))]
                            (get solution x)))]
    (is (= 10 (solve-and-check)))
    (is (= 10 (solve-and-check)))))

(deftest cache-minimize-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int (range 1 11))
                                solution (i/minimize x (i/>= x 0))]
                            (get solution x)))]
    (is (= 1 (solve-and-check)))
    (is (= 1 (solve-and-check)))))

(deftest maximize-vs-minimize-no-cross-contamination-test
  (let [make-constraint (fn [] (let [x (i/fresh-int (range 1 11))] [x (i/>= x 0)]))
        [x1 c1] (make-constraint)
        max-sol (i/maximize x1 c1)
        [x2 c2] (make-constraint)
        min-sol (i/minimize x2 c2)]
    (is (= 10 (get max-sol x1)))
    (is (= 1 (get min-sol x2)))))

;; --- end-to-end: satisfy-all (sync) ---

(deftest satisfy-all-cached-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int #{1 2 3})
                                sols (i/satisfy-all (i/>= x 1))]
                            (sort (map #(get % x) sols))))]
    (is (= [1 2 3] (solve-and-check)))
    ;; Second call: complete cache → returns from cache
    (is (= [1 2 3] (solve-and-check)))))

;; --- end-to-end: async ---

(deftest async-satisfy-cached-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int #{1 2 3})
                                ch (i/satisfy (i/= x 2) {:async? true})
                                sol (async/alt!! ch ([v] v)
                                                 (async/timeout 10000) :timeout)]
                            (is (not= :timeout sol))
                            (get sol x)))]
    (is (= 2 (solve-and-check)))
    (is (= 2 (solve-and-check)))))

(deftest async-satisfy-populates-cache-for-sync-test
  (let [x (i/fresh-int (range 10))
        ch (i/satisfy (i/= x 5) {:async? true})
        sol (async/alt!! ch ([v] v)
                         (async/timeout 10000) :timeout)]
    (is (= 5 (get sol x))))
  (let [x (i/fresh-int (range 10))
        sol (i/satisfy (i/= x 5))]
    (is (= 5 (get sol x)))))

(deftest async-satisfy-all-emits-cached-then-new-test
  (testing "async satisfy-all emits cached solutions first, then new ones"
    (let [x (i/fresh-int #{1 2 3})]
      (i/satisfy (i/= x 2)))
    (let [x (i/fresh-int #{1 2 3})
          ch (i/satisfy-all (i/>= x 1) {:async? true})
          sols (async/<!! (async/go-loop [acc []]
                            (let [[v _] (async/alts! [ch (async/timeout 10000)])]
                              (if v
                                (recur (conj acc v))
                                acc))))]
      (is (= #{1 2 3} (set (map #(get % x) sols)))))))

(deftest async-satisfy-all-cached-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int #{1 2 3})
                                ch (i/satisfy-all (i/>= x 1) {:async? true})
                                sols (async/<!! (async/go-loop [acc []]
                                                  (let [[v _] (async/alts! [ch (async/timeout 10000)])]
                                                    (if v
                                                      (recur (conj acc v))
                                                      acc))))]
                            (sort (map #(get % x) sols))))]
    (is (= [1 2 3] (solve-and-check)))
    (is (= [1 2 3] (solve-and-check)))))

(deftest async-minimize-cached-test
  (let [solve-and-check (fn []
                          (let [x (i/fresh-int (range 1 11))
                                ch (i/minimize x (i/>= x 0) {:async? true})
                                sol (async/<!! (async/go-loop [last-sol nil]
                                                 (let [[v _] (async/alts! [ch (async/timeout 10000)])]
                                                   (if v
                                                     (recur v)
                                                     last-sol))))]
                            (get sol x)))]
    (is (= 1 (solve-and-check)))
    (is (= 1 (solve-and-check)))))
