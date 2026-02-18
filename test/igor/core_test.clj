(ns igor.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.utils.test :refer [throws?]]))

(deftest conjunction-does-not-stack-overflow-test
  (is (= false
         (throws?
          (let [n 15]
            (->> (for [x (range n)
                       y (range n)
                       :let [a (i/fresh)
                             b (i/fresh)]]
                   (i/and
                    (i/= a x)
                    (i/= b y)
                    (i/= (i/+ a b) (clojure.core/+ x y))))
                 (apply i/conjunction)))))))
