(ns routaverra.igor.terms.core
  (:refer-clojure :exclude [+ - * / = > < >= <= and or not when
                             mod rem inc dec even? odd? pos? neg? zero?
                             true? false? not= contains? count max min])
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.types :as types]
            [routaverra.igor.utils.string :refer [>>]]))

(declare plus product minus divide inc* dec* equals not-equals
         greater-than less-than gte lte and* or* not* when*
         iff cond* contains?* count* max* min* nth*
         even?* odd* pos?* neg?* zero?* true?* false?*
         modulo remainder abs* all-different
         translate-comparator)

(defn ground?
  "Returns true if x is a ground value (literal number, boolean, or set of ground values)
   with no decision variables — safe to evaluate in pure Clojure."
  [x]
  (clojure.core/or (number? x)
                   (instance? Boolean x)
                   (clojure.core/and (set? x) (every? ground? x))))

(defn translation-error! [self]
  (throw
   (ex-info
    "This isn't expected to happen because expansion should always be applied before translation."
    {:self self})))

(defrecord TermPlus [argv]
  protocols/IExpand
  (expand [self] (reduce
                  (fn [acc curr]
                    (plus acc curr))
                  (:argv self)))
  protocols/IExpress
  (write [_self] (apply list '+ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "+" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply clojure.core/+ (api/eval-argv self solution))))

(defrecord TermProduct [argv]
  protocols/IExpress
  (write [_self] (apply list '* (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "*" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply clojure.core/* (api/eval-argv self solution))))

(defrecord TermMinus [argv]
  protocols/IExpress
  (write [_self] (apply list '- (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "-" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply clojure.core/- (api/eval-argv self solution))))

(defrecord TermDivide [argv]
  protocols/IExpress
  (write [_self] (apply list '/ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "div" (map protocols/translate (:argv self))))
  (evaluate [self solution] (apply quot (api/eval-argv self solution))))

(defrecord TermInc [argv]
  protocols/IExpress
  (write [_self] (apply list 'inc (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (plus (first argv) 1)))
  (evaluate [self solution] (clojure.core/inc (api/eval-arg (first argv) solution))))

(defrecord TermDec [argv]
  protocols/IExpress
  (write [_self] (apply list 'dec (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (minus (first argv) 1)))
  (evaluate [self solution] (clojure.core/dec (api/eval-arg (first argv) solution))))

(defrecord TermEven? [argv]
  protocols/IExpand
  (expand [_self] (equals (modulo (first argv) 2) 0))
  protocols/IExpress
  (write [_self] (apply list 'even? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translation-error! self))
  (evaluate [self solution] (clojure.core/even? (api/eval-arg (first argv) solution))))

(defrecord TermOdd? [argv]
  protocols/IExpand
  (expand [_self] (equals (modulo (first argv) 2) 1))
  protocols/IExpress
  (write [_self] (apply list 'odd? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translation-error! self))
  (evaluate [self solution] (clojure.core/odd? (api/eval-arg (first argv) solution))))

(defn to-literal-array [elements]
  (apply str (concat ["["] (interpose ", " elements) ["]"])))

(defrecord TermMax [argv]
  protocols/IExpress
  (write [_self] (apply list 'max (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "max(" (to-literal-array (map protocols/translate (:argv self))) ")"))
  (evaluate [self solution] (apply clojure.core/max (api/eval-argv self solution))))

(defrecord TermMin [argv]
  protocols/IExpress
  (write [_self] (apply list 'min (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "min(" (to-literal-array (map protocols/translate (:argv self))) ")"))
  (evaluate [self solution] (apply clojure.core/min (api/eval-argv self solution))))

(defrecord TermAbs [argv]
  protocols/IExpress
  (write [_self] (apply list 'abs (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "abs(" (protocols/translate (first (:argv self))) ")"))
  (evaluate [self solution] (Math/abs (long (api/eval-arg (first argv) solution)))))

(defrecord TermPow [argv]
  protocols/IExpress
  (write [_self] (apply list 'pow (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "pow(" (protocols/translate (first (:argv self))) ", " (protocols/translate (second (:argv self))) ")"))
  (evaluate [self solution] (long (Math/pow (api/eval-arg (first argv) solution) (api/eval-arg (second argv) solution)))))

(defrecord TermAllDifferent [argv]
  protocols/IInclude
  (mzn-includes [_self] #{"alldifferent.mzn"})
  protocols/IExpress
  (write [_self] (apply list 'all-different (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "alldifferent(" (to-literal-array (map protocols/translate (:argv self))) ")"))
  (evaluate [self solution]
    (let [vals (api/eval-argv self solution)]
      (clojure.core/= (clojure.core/count vals) (clojure.core/count (distinct vals))))))

(defrecord TermTrue? [argv]
  protocols/IExpress
  (write [_self] (apply list 'true? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (equals (first argv) true)))
  (evaluate [self solution] (clojure.core/true? (api/eval-arg (first argv) solution))))

(defrecord TermFalse? [argv]
  protocols/IExpress
  (write [_self] (apply list 'false? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (equals (first argv) false)))
  (evaluate [self solution] (clojure.core/false? (api/eval-arg (first argv) solution))))

(defrecord TermAnd [argv]
  protocols/IExpress
  (write [_self] (apply list 'and (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "/\\" (map protocols/translate (:argv self))))
  (evaluate [self solution] (every? identity (api/eval-argv self solution))))

(defn conjunctive? [x] (clojure.core/= (type x) TermAnd))

(defrecord TermOr [argv]
  protocols/IExpress
  (write [_self] (apply list 'or (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "\\/" (map protocols/translate (:argv self))))
  (evaluate [self solution] (boolean (some identity (api/eval-argv self solution)))))

(defrecord TermWhen [argv]
  protocols/IExpress
  (write [_self] (apply list 'when (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take 2 (repeat {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply
                     api/translate-binary-operation
                     "->"
                     (map protocols/translate (:argv self))))
  (evaluate [self solution]
    (let [[test body] (api/eval-argv self solution)]
      (clojure.core/or (clojure.core/not test) body))))

(defrecord TermGreaterThan [argv]
  protocols/IExpress
  (write [_self] (apply list '> (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self ">" greater-than))
  (evaluate [self solution] (apply clojure.core/> (api/eval-argv self solution))))

(defrecord TermLessThan [argv]
  protocols/IExpress
  (write [_self] (apply list '< (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self "<" less-than))
  (evaluate [self solution] (apply clojure.core/< (api/eval-argv self solution))))

(defrecord TermGreaterThanOrEqualTo [argv]
  protocols/IExpand
  (expand [self] (case (clojure.core/count argv)
                   1 true
                   2 self
                   (reduce
                    (fn [acc [x y]]
                      (and* acc (gte x y)))
                    true
                    (partition 2 1 argv))))
  protocols/IExpress
  (write [_self] (apply list '>= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self ">=" gte))
  (evaluate [self solution] (apply clojure.core/>= (api/eval-argv self solution))))

(defrecord TermLessThanOrEqualTo [argv]
  protocols/IExpress
  (write [_self] (apply list '<= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self "<=" lte))
  (evaluate [self solution] (apply clojure.core/<= (api/eval-argv self solution))))

(defrecord TermNot [argv]
  protocols/IExpress
  (write [_self] (apply list 'not (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [_self] (api/cacheing-decisions (first argv)))
  (bindings [_self] (protocols/bindings (first argv)))
  (validate [self] (api/validate-domains self))
  (translate [_self] (>> {:arg (protocols/translate (first argv))} "(not {{arg}})"))
  (evaluate [self solution] (clojure.core/not (api/eval-arg (first argv) solution))))

(defrecord TermEquals [argv]
  protocols/IExpress
  (write [_self] (apply list '= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take
                   (clojure.core/count argv)
                   (repeat
                    (zipmap
                     (->> argv
                          (map protocols/codomain)
                          (sort-by clojure.core/count)
                          first
                          keys)
                     (repeat self)))))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
    (clojure.core/when (empty? (->> (:argv self)
                                    (map (comp set keys protocols/codomain))
                                    (apply clojure.set/intersection)))
      (throw (ex-info "equality testing requires consistent types" {})))
    self)
  (translate [self] (translate-comparator self "=" equals))
  (evaluate [self solution] (apply clojure.core/= (api/eval-argv self solution))))

(defn condititonal-return-exprs [self]
  (->> (:argv self)
       (partition-all 2)
       (map (fn [test-expr-pair]
              (case (clojure.core/count test-expr-pair)
                2 (last test-expr-pair)
                1 (first test-expr-pair))))))

(defn conditional-codomain [self]
  {:post [(clojure.spec.alpha/valid? ::api/domain %)]}
  (zipmap (->> (condititonal-return-exprs self)
               (map protocols/codomain)
               (sort-by clojure.core/count)
               first
               keys)
          (repeat self)))

(defn conditional-domainv [self]
  (let [return-domain (conditional-codomain self)]
    (->> (:argv self)
         (partition-all 2)
         (mapcat (fn [test-expr-pair]
                   (case (clojure.core/count test-expr-pair)
                     2 [{types/Bool self} return-domain]
                     1 [return-domain]))))))

(defn translate-conditional [self]
  (apply
   str
   (concat
    ["("]
    (->> (:argv self)
         (partition-all 2)
         (interleave (range))
         (partition 2)
         (mapcat (fn [[i [test-or-expr expr :as test-expr-pair]]]
                   (case (clojure.core/count test-expr-pair)
                     2 [(if (clojure.core/zero? i) "if " " elseif ")
                        (protocols/translate test-or-expr)
                        " then "
                        (protocols/translate expr)]
                     1 [" else " (protocols/translate test-or-expr)]))))
    [" endif)"])))

(defrecord TermIf [argv]
  protocols/IExpress
  (write [_self] (apply list 'if (map protocols/write argv)))
  (codomain [self] (conditional-codomain self))
  (domainv [self] (conditional-domainv self))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
            (clojure.core/when (empty? (->> (condititonal-return-exprs self)
                                            (map (comp set keys protocols/codomain))
                                            (apply clojure.set/intersection)))
              (throw (ex-info "if requires consistent types in its return expressions" {})))
            (api/validate-domains self))
  (translate [self] (translate-conditional self))
  (evaluate [self solution]
    (let [[test then else] argv]
      (if (api/eval-arg test solution)
        (api/eval-arg then solution)
        (api/eval-arg else solution)))))

(defrecord TermCond [argv]
  protocols/IExpress
  (write [_self] (apply list 'cond (map protocols/write argv)))
  (codomain [self] (conditional-codomain self))
  (domainv [self] (conditional-domainv self))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
            (clojure.core/when (empty? (->> (condititonal-return-exprs self)
                                            (map (comp set keys protocols/codomain))
                                            (apply clojure.set/intersection)))
              (throw (ex-info "cond requires consistent types in its return expressions" {})))
            (api/validate-domains self))
  (translate [self] (translate-conditional self))
  (evaluate [self solution]
    (let [pairs (partition-all 2 argv)]
      (loop [[[test-or-val expr :as pair] & rest] pairs]
        (case (clojure.core/count pair)
          2 (if (api/eval-arg test-or-val solution)
              (api/eval-arg expr solution)
              (recur rest))
          1 (api/eval-arg test-or-val solution))))))

(defrecord TermContains [argv]
  protocols/IExpress
  (write [_self] (apply list 'contains? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Set self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-binary-operation
                     "in"
                     (protocols/translate (second (:argv self)))
                     (protocols/translate (first (:argv self)))))
  (evaluate [self solution]
    (let [[set-expr elem] (api/eval-argv self solution)]
      (clojure.core/contains? set-expr elem))))

(defrecord TermPos? [argv]
  protocols/IExpress
  (write [_self] (apply list 'pos? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (protocols/translate
     (greater-than (first argv) 0)))
  (evaluate [self solution] (clojure.core/pos? (api/eval-arg (first argv) solution))))

(defrecord TermNeg? [argv]
  protocols/IExpress
  (write [_self] (apply list 'neg? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (protocols/translate
     (less-than (first argv) 0)))
  (evaluate [self solution] (clojure.core/neg? (api/eval-arg (first argv) solution))))

(defrecord TermZero? [argv]
  protocols/IExpress
  (write [_self] (apply list 'zero? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (protocols/translate
     (equals (first argv) 0)))
  (evaluate [self solution] (clojure.core/zero? (api/eval-arg (first argv) solution))))

(defrecord TermMod [argv]
  protocols/IExpand
  (expand [_self]
    (let [n (first argv)
          d (second argv)
          m (remainder n d)]
      (iff (or* (zero?* m) (equals (pos?* n) (pos?* d)))
           m
           (plus m d))))
  protocols/IExpress
  (write [_self] (apply list 'mod (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translation-error! self))
  (evaluate [self solution] (apply clojure.core/mod (api/eval-argv self solution))))

(defrecord TermRem [argv]
  protocols/IExpress
  (write [_self] (apply list 'rem (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply api/translate-binary-operation "mod" (map protocols/translate argv)))
  (evaluate [self solution] (apply clojure.core/rem (api/eval-argv self solution))))

(defrecord TermCount [argv]
  protocols/IExpress
  (write [_self] (apply list 'count (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Set self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>> {:set (protocols/translate (first (:argv self)))} "(card({{set}}))"))
  (evaluate [self solution] (clojure.core/count (api/eval-arg (first argv) solution))))

(defrecord TermNth [argv n]
  ;; argv = [elem0 elem1 ... elemN-1 idx], n = number of elements (not counting idx)
  protocols/IExpand
  (expand [_self]
    (let [elems (subvec argv 0 n)
          idx (get argv n)]
      (if (clojure.core/= n 1)
        (first elems)
        (apply cond*
          (concat
            (mapcat (fn [i elem] [(equals idx i) elem])
                    (range (clojure.core/dec n))
                    (butlast elems))
            [:else (last elems)])))))
  protocols/IExpress
  (write [_self] (list 'nth (mapv protocols/write (subvec argv 0 n)) (protocols/write (get argv n))))
  (codomain [self]
    (let [elem-types (->> (subvec argv 0 n)
                          (map (comp set keys protocols/codomain)))]
      (zipmap (apply clojure.set/intersection elem-types) (repeat self))))
  (domainv [self]
    ;; elements can be any type (consistent with each other), index is Numeric
    (let [elem-domain (protocols/codomain self)]
      (concat (repeat n elem-domain) [{types/Numeric self}])))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self]
    (clojure.core/when (empty? (->> (subvec argv 0 n)
                                     (map (comp set keys protocols/codomain))
                                     (apply clojure.set/intersection)))
      (throw (ex-info "nth requires consistent types across elements" {})))
    (api/validate-domains self))
  (translate [self] (translation-error! self))
  (evaluate [self solution]
    (let [vals (api/eval-argv self solution)
          elems (subvec vals 0 n)
          idx (get vals n)]
      (clojure.core/nth elems idx))))

;; --- Constructor functions ---

(defn- ground-or-validate
  "Construct the term once. If all args are ground, evaluate directly; otherwise validate."
  [term ground?-check]
  (if ground?-check
    (protocols/evaluate term {})
    (api/cacheing-validate term)))

(defn plus [& args]
  (ground-or-validate (->TermPlus (vec args)) (every? ground? args)))
(defn product [& args]
  (ground-or-validate (->TermProduct (vec args)) (every? ground? args)))
(defn minus [& args]
  (ground-or-validate (->TermMinus (vec args)) (every? ground? args)))
(defn divide [& args]
  (ground-or-validate (->TermDivide (vec args)) (every? ground? args)))
(defn inc* [x]
  (ground-or-validate (->TermInc [x]) (ground? x)))
(defn dec* [x]
  (ground-or-validate (->TermDec [x]) (ground? x)))
(defn even?* [x]
  (ground-or-validate (->TermEven? [x]) (ground? x)))
(defn odd?* [x]
  (ground-or-validate (->TermOdd? [x]) (ground? x)))
(defn max* [& args]
  (ground-or-validate (->TermMax (vec args)) (every? ground? args)))
(defn min* [& args]
  (ground-or-validate (->TermMin (vec args)) (every? ground? args)))
(defn true?* [x]
  (ground-or-validate (->TermTrue? [x]) (ground? x)))
(defn false?* [x]
  (ground-or-validate (->TermFalse? [x]) (ground? x)))
(defn and* [& args]
  (ground-or-validate (->TermAnd (vec args)) (every? ground? args)))
(defn or* [& args]
  (ground-or-validate (->TermOr (vec args)) (every? ground? args)))
(defn when* [test body]
  (ground-or-validate (->TermWhen [test body]) (every? ground? [test body])))
(defn not* [x]
  (ground-or-validate (->TermNot [x]) (ground? x)))
(defn greater-than [& args]
  (ground-or-validate (->TermGreaterThan (vec args)) (every? ground? args)))
(defn less-than [& args]
  (ground-or-validate (->TermLessThan (vec args)) (every? ground? args)))
(defn gte [& args]
  (ground-or-validate (->TermGreaterThanOrEqualTo (vec args)) (every? ground? args)))
(defn lte [& args]
  (ground-or-validate (->TermLessThanOrEqualTo (vec args)) (every? ground? args)))
(defn equals [& args]
  (ground-or-validate (->TermEquals (vec args)) (every? ground? args)))
(defn not-equals [& args]
  (if (every? ground? args)
    (clojure.core/not (protocols/evaluate (->TermEquals (vec args)) {}))
    (not* (apply equals args))))
(defn iff [test then else]
  (ground-or-validate (->TermIf [test then else]) (every? ground? [test then else])))
(defn cond* [& args]
  (let [penultimate (get (vec args) (clojure.core/- (clojure.core/count args) 2))]
    (clojure.core/when-not (clojure.core/contains? #{:else :default} penultimate)
      (throw (ex-info "cond requires :else or :default" {})))
    (let [normalized (-> (drop-last 2 args) vec (conj (last args)))]
      (ground-or-validate (->TermCond normalized)
                          (every? ground? normalized)))))
(defn contains?* [set-expr elem]
  (ground-or-validate (->TermContains [set-expr elem]) (every? ground? [set-expr elem])))
(defn pos?* [x]
  (ground-or-validate (->TermPos? [x]) (ground? x)))
(defn neg?* [x]
  (ground-or-validate (->TermNeg? [x]) (ground? x)))
(defn zero?* [x]
  (ground-or-validate (->TermZero? [x]) (ground? x)))
(defn modulo [& args]
  (ground-or-validate (->TermMod (vec args)) (every? ground? args)))
(defn remainder [& args]
  (ground-or-validate (->TermRem (vec args)) (every? ground? args)))
(defn abs* [x]
  (ground-or-validate (->TermAbs [x]) (ground? x)))
(defn pow* [base exp]
  (ground-or-validate (->TermPow [base exp]) (every? ground? [base exp])))
(defn all-different [& args]
  (ground-or-validate (->TermAllDifferent (vec args)) (every? ground? args)))
(defn count* [x]
  (ground-or-validate (->TermCount [x]) (ground? x)))
(defn nth* [elems idx]
  {:pre [(vector? elems) (clojure.core/>= (clojure.core/count elems) 1)]}
  (let [term (->TermNth (conj elems idx) (clojure.core/count elems))]
    (ground-or-validate term (clojure.core/and (every? ground? elems) (ground? idx)))))

;; --- translate-comparator (moved from api) ---

(defn translate-comparator [self op constructor-fn]
  (case (clojure.core/count (:argv self))
    1 (protocols/translate true)
    2 (apply api/translate-binary-operation op (map protocols/translate (:argv self)))
    (->> (:argv self)
         (partition 2 1)
         (map (fn [[a b]] (constructor-fn a b)))
         (apply and*)
         protocols/translate)))

