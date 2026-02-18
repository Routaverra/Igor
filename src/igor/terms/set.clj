(ns igor.terms.set
  (:require [igor.protocols :as protocols]
            [igor.types :as types]
            [igor.api :as api]
            [igor.set :as i.set]
            [clojure.set :as set]))

(defrecord TermIntersection [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/intersection (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "intersect" (map protocols/translate (:argv self)))))

(defrecord TermDifference [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/difference (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "diff" (map protocols/translate (:argv self)))))

(defrecord TermSymDiff [argv]
  protocols/IExpress
  (write [_self] (apply list 'igor.set/sym-diff (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "symdiff" (map protocols/translate (:argv self)))))

(defrecord TermUnion [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/union (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "union" (map protocols/translate (:argv self)))))

(defrecord TermSubset [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/subset? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "subset" (map protocols/translate (:argv self)))))

(defrecord TermSuperset [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/superset? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "superset" (map protocols/translate (:argv self)))))

;; --- Constructor functions ---

(defn intersection [& args] (api/cacheing-validate (->TermIntersection (vec args))))
(defn difference [& args] (api/cacheing-validate (->TermDifference (vec args))))
(defn sym-diff [& args] (api/cacheing-validate (->TermSymDiff (vec args))))
(defn union [& args] (api/cacheing-validate (->TermUnion (vec args))))
(defn subset? [& args] (api/cacheing-validate (->TermSubset (vec args))))
(defn superset? [& args] (api/cacheing-validate (->TermSuperset (vec args))))
