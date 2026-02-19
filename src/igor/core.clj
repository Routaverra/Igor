(ns igor.core
  (:refer-clojure :exclude [+ - * / = > < >= <= and or not when if cond
                             mod rem inc dec even? odd? pos? neg? zero?
                             true? false? not= contains? count max min nth])
  (:require [igor.api :as api]
            [igor.protocols :as protocols]
            [igor.solver :as solver]
            [igor.terms.core :as terms]
            [igor.terms.set :as terms.set]
            [igor.terms.introduced :as terms.introduced]
            [igor.types :as types]
            [igor.utils.string :refer [>>]]))

(defn fresh
  "Mint a fresh decision."
  ([]
   (fresh (str (gensym))))
  ([id]
   {:pre [(string? id)]}
   (if (re-matches #"[A-Za-z][A-Za-z0-9_]*" id)
     (api/->Decision id)
     (throw (ex-info
             (>> {:id id}
                 "Invalid identifier: {{id}}. Identifiers should start with a letter and consist only of letters, numbers, and underscores.")
             {})))))

(defn fresh-set [super]
  (api/bind super (fresh)))

(defn fresh-int
  "Mint a fresh integer decision bounded to the given domain."
  ([domain]
   (api/force-type (api/bind domain (fresh)) types/Numeric))
  ([domain id]
   (api/force-type (api/bind domain (fresh id)) types/Numeric)))

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
(def contains? terms/contains?*)
(def intersection terms.set/intersection)
(def difference terms.set/difference)
(def sym-diff terms.set/sym-diff)
(def union terms.set/union)
(def subset? terms.set/subset?)
(def superset? terms.set/superset?)
(def forall terms.introduced/forall)
(def for-set terms.introduced/for-set)
