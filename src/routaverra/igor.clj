(ns routaverra.igor
  (:refer-clojure :exclude [+ - * / = > < >= <= and or not if cond some every?
                             mod rem inc dec even? odd? pos? neg? zero?
                             true? false? not= contains? count max min nth abs
                             resolve])
  (:require [routaverra.igor.api :as api]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.solver :as solver]
            [routaverra.igor.terms.core :as terms]
            [routaverra.igor.terms.set :as terms.set]
            [routaverra.igor.terms.introduced :as terms.introduced]
            [routaverra.igor.graph :as graph]
            [routaverra.igor.extensional :as extensional]
            [routaverra.igor.notation :as notation]
            [routaverra.igor.alternatives :as alternatives]
            [routaverra.igor.soft :as soft]
            [clojure.walk :as walk]))

(def fresh api/fresh)
(def fresh-set api/fresh-set)
(def fresh-int api/fresh-int)
(def fresh-bool api/fresh-bool)
(def fresh-keyword api/fresh-keyword)
(def bind api/bind)

(defn satisfy
  ([term]
   (satisfy term {}))
  ([term opts]
   (solver/solve opts term nil)))

(defn satisfy-all
  ([term]
   (satisfy-all term {}))
  ([term opts]
   (solver/solve (assoc opts :all? true) term nil)))

(defn resolve
  "Walks form, replacing decision variables with solved values and evaluating
   igor term expressions to concrete results.
   The 'second half' of solve — useful when you already have a solution."
  [solution form]
  (walk/postwalk
    (fn [x]
      (clojure.core/cond
        (api/decision? x) (get solution x x)
        (satisfies? protocols/IExpress x) (protocols/evaluate x solution)
        :else x))
    form))

(defn solve
  "Satisfies the constraint and walks form, replacing every decision
   variable with its solved value. Returns nil when unsatisfiable."
  [constraint form]
  (when-let [solution (satisfy constraint)]
    (resolve solution form)))

(defn maximize
  ([obj constraint]
   (maximize obj constraint {}))
  ([obj constraint opts]
   (solver/solve opts constraint obj)))

(defn minimize
  ([obj constraint]
   (minimize obj constraint {}))
  ([obj constraint opts]
   (solver/solve (assoc opts :direction :minimize) constraint obj)))

(def decision? api/decision?)

(defn unresolved? [x]
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
(def implies terms.introduced/implies*)
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
(def pow terms/pow*)
(def all-different terms/all-different)
(def contains? terms/contains?*)
(def intersection terms.set/intersection)
(def difference terms.set/difference)
(def sym-diff terms.set/sym-diff)
(def union terms.set/union)
(def subset? terms.set/subset?)
(def superset? terms.set/superset?)
(def set< terms.set/set<)
(def set<= terms.set/set<=)
(def every? terms/every?*)
(def some terms/some*)
(def image terms.introduced/image)
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
(def as notation/as)
(def render-notation notation/render-notation)
(def render-problem notation/render-problem)

(defn validate-solution
  "Evaluate a constraint against a solution map in pure Clojure.
   Returns true if the solution satisfies the constraint, false otherwise."
  [constraint solution]
  (protocols/evaluate constraint solution))

;; --- Constructive disjunction ---

(def alternatives alternatives/alternatives)
(def choice alternatives/choice)

;; --- Soft constraints ---

(def soft soft/soft)
(def violation soft/violation)
