(ns igor.core
  (:refer-clojure :exclude [+ - * / = > < >= <= and or not when if cond
                             mod rem inc dec even? odd? pos? neg? zero?
                             true? false? not= contains? count max min nth abs])
  (:require [igor.api :as api]
            [igor.solver :as solver]
            [igor.terms.core :as terms]
            [igor.terms.set :as terms.set]
            [igor.terms.introduced :as terms.introduced]
            [igor.graph :as graph]
            [igor.extensional :as extensional]))

(def fresh api/fresh)
(def fresh-set api/fresh-set)
(def fresh-int api/fresh-int)
(def fresh-bool api/fresh-bool)
(def bind api/bind)

(defn satisfy
  ([term]
   (satisfy term {}))
  ([term opts]
   (solver/solve opts term nil)))

(defn maximize
  ([obj constraint]
   (maximize obj constraint {}))
  ([obj constraint opts]
   (solver/solve opts constraint obj)))

(defn dithered? [x]
  (boolean (api/cacheing-decisions x)))

;; --- Re-exported term constructors ---

(def + terms/plus)
(def * terms/product)
(def - terms/minus)
(def / terms/divide)
(def inc terms/inc*)
(def dec terms/dec*)
(def = terms/equals)
(def not= terms/not-equals)
(def > terms/greater-than)
(def < terms/less-than)
(def >= terms/gte)
(def <= terms/lte)
(def and terms/and*)
(def or terms/or*)
(def not terms/not*)
(def when terms/when*)
(def if terms/iff)
(def cond terms/cond*)
(def even? terms/even?*)
(def odd? terms/odd?*)
(def pos? terms/pos?*)
(def neg? terms/neg?*)
(def zero? terms/zero?*)
(def true? terms/true?*)
(def false? terms/false?*)
(def mod terms/modulo)
(def rem terms/remainder)
(def max terms/max*)
(def min terms/min*)
(def count terms/count*)
(def nth terms/nth*)
(def abs terms/abs*)
(def all-different terms/all-different)
(def contains? terms/contains?*)
(def intersection terms.set/intersection)
(def difference terms.set/difference)
(def sym-diff terms.set/sym-diff)
(def union terms.set/union)
(def subset? terms.set/subset?)
(def superset? terms.set/superset?)
(def forall terms.introduced/forall)
(def for-set terms.introduced/for-set)
(def digraph graph/digraph)
(def active-nodes graph/active-nodes)
(def active-edges graph/active-edges)
(def circuit graph/circuit)
(def subcircuit graph/subcircuit)
(def subgraph graph/subgraph)
(def path graph/path)
(def dpath graph/dpath)
(def bounded-path graph/bounded-path)
(def bounded-dpath graph/bounded-dpath)
(def reachable graph/reachable)
(def dreachable graph/dreachable)
(def connected graph/connected)
(def dconnected graph/dconnected)
(def dag graph/dag)
(def tree graph/tree)
(def dtree graph/dtree)
(def weighted-spanning-tree graph/weighted-spanning-tree)
(def d-weighted-spanning-tree graph/d-weighted-spanning-tree)
(def table extensional/table)
(def regular extensional/regular)
(def cost-regular extensional/cost-regular)
