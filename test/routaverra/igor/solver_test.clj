(ns routaverra.igor.solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async]
            [routaverra.igor :as i]))

(deftest satisfy-sync-test
  (testing "satisfy returns a single valid solution"
    (let [x (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/>= x 1))]
      (is (map? sol))
      (is (#{1 2 3} (get sol x))))))

(deftest satisfy-all-sync-test
  (testing "satisfy-all returns all solutions"
    (let [x (i/fresh-int #{1 2 3})
          sols (i/satisfy-all (i/>= x 1))]
      (is (vector? sols))
      (is (= 3 (clojure.core/count sols)))
      (is (= #{1 2 3} (set (map #(get % x) sols)))))))

(deftest satisfy-async-test
  (testing "satisfy async returns a channel with one solution"
    (let [x (i/fresh-int #{1 2 3})
          ch (i/satisfy (i/>= x 1) {:async? true})
          sol (async/alt!! ch ([v] v)
                           (async/timeout 5000) :timeout)]
      (is (not= :timeout sol))
      (is (map? sol))
      (is (#{1 2 3} (get sol x))))))

(deftest satisfy-all-async-test
  (testing "satisfy-all async streams all solutions then closes"
    (let [x (i/fresh-int #{1 2 3})
          ch (i/satisfy-all (i/>= x 1) {:async? true})
          sols (async/<!! (async/go-loop [acc []]
                            (let [[v _] (async/alts! [ch (async/timeout 5000)])]
                              (if v
                                (recur (conj acc v))
                                acc))))]
      (is (= 3 (clojure.core/count sols)))
      (is (= #{1 2 3} (set (map #(get % x) sols)))))))

(deftest minimize-async-test
  (testing "minimize async returns a channel with optimal solution"
    (let [x (i/fresh-int (range 1 11))
          ch (i/minimize x (i/>= x 0) {:async? true})
          sol (async/<!! (async/go-loop [last-sol nil]
                           (let [[v _] (async/alts! [ch (async/timeout 5000)])]
                             (if v
                               (recur v)
                               last-sol))))]
      (is (map? sol))
      (is (= 1 (get sol x))))))
