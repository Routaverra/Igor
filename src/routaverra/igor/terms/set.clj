(ns routaverra.igor.terms.set
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.types :as types]
            [routaverra.igor.api :as api]
            [routaverra.igor.set :as i.set]
            [clojure.set :as set]))

;; --- Helpers ---

(defn- all-ground-sets? [args] (every? set? args))

(defn- ground-or-validate [term ground?]
  (if ground?
    (protocols/evaluate term {})
    (api/cacheing-validate term)))

(defn set-lex-compare
  "Lexicographic comparison on sorted element sequences.
   Returns neg/zero/pos like compare."
  [a b]
  (let [sa (sort a) sb (sort b)]
    (loop [[x & xs] sa [y & ys] sb]
      (cond
        (and (nil? x) (nil? y)) 0
        (nil? x) -1
        (nil? y)  1
        (< x y)  -1
        (> x y)   1
        :else (recur xs ys)))))

;; --- Records ---

(defrecord TermIntersection [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/intersection (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "intersect" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply set/intersection (api/eval-argv self solution))))

(defrecord TermDifference [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/difference (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "diff" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply set/difference (api/eval-argv self solution))))

(defrecord TermSymDiff [argv]
  protocols/IExpress
  (write [_self] (apply list 'routaverra.igor.set/sym-diff (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "symdiff" (map protocols/translate (:argv self))))
  (evaluate [self solution] (reduce i.set/sym-diff (api/eval-argv self solution))))

(defrecord TermUnion [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/union (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "union" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply set/union (api/eval-argv self solution))))

(defrecord TermSubset [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/subset? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "subset" (map protocols/translate (:argv self))))
  (evaluate [self solution]
    (let [vals (api/eval-argv self solution)]
      (every? true? (map (fn [[a b]] (set/subset? a b)) (partition 2 1 vals))))))

(defrecord TermSuperset [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/superset? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "superset" (map protocols/translate (:argv self))))
  (evaluate [self solution]
    (let [vals (api/eval-argv self solution)]
      (every? true? (map (fn [[a b]] (set/superset? a b)) (partition 2 1 vals))))))

(defrecord TermSetLt [argv]
  protocols/IExpress
  (write [_self] (apply list 'set-lt (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "set_lt(" (protocols/translate (first (:argv self)))
                         ", " (protocols/translate (second (:argv self))) ")"))
  (evaluate [self solution]
    (let [[a b] (api/eval-argv self solution)]
      (neg? (set-lex-compare a b)))))

(defrecord TermSetLe [argv]
  protocols/IExpress
  (write [_self] (apply list 'set-le (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "set_le(" (protocols/translate (first (:argv self)))
                         ", " (protocols/translate (second (:argv self))) ")"))
  (evaluate [self solution]
    (let [[a b] (api/eval-argv self solution)]
      (<= (set-lex-compare a b) 0))))

;; --- Constructor functions ---

(defn intersection [& args]
  (ground-or-validate (->TermIntersection (vec args)) (all-ground-sets? args)))
(defn difference [& args]
  (ground-or-validate (->TermDifference (vec args)) (all-ground-sets? args)))
(defn sym-diff [& args]
  (ground-or-validate (->TermSymDiff (vec args)) (all-ground-sets? args)))
(defn union [& args]
  (ground-or-validate (->TermUnion (vec args)) (all-ground-sets? args)))
(defn subset? [& args]
  (ground-or-validate (->TermSubset (vec args)) (all-ground-sets? args)))
(defn superset? [& args]
  (ground-or-validate (->TermSuperset (vec args)) (all-ground-sets? args)))

(defn set< [a b]
  (ground-or-validate (->TermSetLt [a b]) (all-ground-sets? [a b])))

(defn set<= [a b]
  (ground-or-validate (->TermSetLe [a b]) (all-ground-sets? [a b])))
