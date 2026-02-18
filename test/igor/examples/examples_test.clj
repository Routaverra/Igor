(ns igor.examples.examples-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]))

(deftest meso-chain-test
  (testing "meso chain constraint integration test"
    (let [mesos (take 5 (repeatedly i/fresh))
          cluster-free (fn [set-decision]
                         (i/forall (i/bind (range 12) set-decision)
                           (fn [a]
                             (i/when (i/contains? set-decision (i/mod (i/+ a 1) 12))
                               (i/not (i/contains? set-decision (i/mod (i/+ a 2) 12)))))))
          constraint (apply
                      i/conjunction
                      (concat
                       (->> mesos
                            (partition 2 1)
                            (map
                             (fn [[a b]]
                               (i/and
                                (i/not= a b)
                                (i/= (i/count (i/intersection a b)) 3)))))
                       (map cluster-free mesos)
                       (map (comp (partial i/= 4) i/count) mesos)))
          solution (i/satisfy constraint)
          mesos* (map solution mesos)]
      (is (= true
             (and (every?
                   clojure.core/true?
                   (map
                    (fn [x]
                      (clojure.core/= 4 (clojure.core/count x)))
                    mesos*))
                  (every?
                   clojure.core/true?
                   (map
                    (fn [[a b]]
                      (and
                       (clojure.core/not= a b)
                       (clojure.core/= 3 (clojure.core/count (clojure.set/intersection a b)))))
                    (partition 2 1 mesos*)))))))))
