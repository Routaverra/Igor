(ns igor.set
  (:require [clojure.set]))

(defn sym-diff [set-a set-b]
  (clojure.set/union
   (clojure.set/difference (set set-a) (set set-b))
   (clojure.set/difference (set set-b) (set set-a))))
