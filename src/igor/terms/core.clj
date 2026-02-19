(ns igor.terms.core
  (:refer-clojure :exclude [+ - * / = > < >= <= and or not when
                             mod rem inc dec even? odd? pos? neg? zero?
                             true? false? not= contains? count max min])
  (:require [igor.protocols :as protocols]
            [igor.api :as api]
            [igor.types :as types]
            [igor.utils.string :refer [>>]]))

(declare plus product minus divide inc* dec* equals not-equals
         greater-than less-than gte lte and* or* not* when*
         iff cond* contains?* count* max* min* nth*
         even?* odd* pos?* neg?* zero?* true?* false?*
         modulo remainder conjunction disjunction
         translate-comparator)

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
  (translate [self] (api/translate-nary-operation "+" (map protocols/translate (:argv self)))))

(defrecord TermProduct [argv]
  protocols/IExpress
  (write [_self] (apply list '* (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "*" (map protocols/translate (:argv self)))))

(defrecord TermMinus [argv]
  protocols/IExpress
  (write [_self] (apply list '- (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "-" (map protocols/translate (:argv self)))))

(defrecord TermDivide [argv]
  protocols/IExpress
  (write [_self] (apply list '/ (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (repeat {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "div" (map protocols/translate (:argv self)))))

(defrecord TermInc [argv]
  protocols/IExpress
  (write [_self] (apply list 'inc (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (plus (first argv) 1))))

(defrecord TermDec [argv]
  protocols/IExpress
  (write [_self] (apply list 'dec (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (minus (first argv) 1))))

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
  (translate [self] (translation-error! self)))

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
  (translate [self] (translation-error! self)))

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
  (translate [self] (str "max(" (to-literal-array (map protocols/translate (:argv self))) ")")))

(defrecord TermMin [argv]
  protocols/IExpress
  (write [_self] (apply list 'min (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "min(" (to-literal-array (map protocols/translate (:argv self))) ")")))

(defrecord TermTrue? [argv]
  protocols/IExpress
  (write [_self] (apply list 'true? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (equals (first argv) true))))

(defrecord TermFalse? [argv]
  protocols/IExpress
  (write [_self] (apply list 'false? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (protocols/translate
                     (equals (first argv) false))))

(defrecord TermAnd [argv]
  protocols/IExpand
  (expand [self] (apply conjunction (:argv self)))
  protocols/IExpress
  (write [_self] (apply list 'and (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "/\\" (map protocols/translate (:argv self)))))

(defn conjunctive? [x] (clojure.core/= (type x) TermAnd))

(defrecord TermOr [argv]
  protocols/IExpand
  (expand [self] (apply disjunction (:argv self)))
  protocols/IExpress
  (write [_self] (apply list 'or (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "\\/" (map protocols/translate (:argv self)))))

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
                     (map protocols/translate (:argv self)))))

(defrecord TermGreaterThan [argv]
  protocols/IExpress
  (write [_self] (apply list '> (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self ">" greater-than)))

(defrecord TermLessThan [argv]
  protocols/IExpress
  (write [_self] (apply list '< (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self "<" less-than)))

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
  (translate [self] (translate-comparator self ">=" gte)))

(defrecord TermLessThanOrEqualTo [argv]
  protocols/IExpress
  (write [_self] (apply list '<= (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (clojure.core/count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (translate-comparator self "<=" lte)))

(defrecord TermNot [argv]
  protocols/IExpress
  (write [_self] (apply list 'not (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Bool self}])
  (decisions [_self] (api/cacheing-decisions (first argv)))
  (bindings [_self] (protocols/bindings (first argv)))
  (validate [self] (api/validate-domains self))
  (translate [_self] (>> {:arg (protocols/translate (first argv))} "(not {{arg}})")))

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
  (translate [self] (translate-comparator self "=" equals)))

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
  (translate [self] (translate-conditional self)))

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
  (translate [self] (translate-conditional self)))

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
                     (protocols/translate (first (:argv self))))))

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
     (greater-than (first argv) 0))))

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
     (less-than (first argv) 0))))

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
     (equals (first argv) 0))))

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
  (translate [self] (translation-error! self)))

(defrecord TermRem [argv]
  protocols/IExpress
  (write [_self] (apply list 'rem (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Numeric self} {types/Numeric self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply api/translate-binary-operation "mod" (map protocols/translate argv))))

(defrecord TermCount [argv]
  protocols/IExpress
  (write [_self] (apply list 'count (map protocols/write argv)))
  (codomain [self] {types/Numeric self})
  (domainv [self] [{types/Set self}])
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>> {:set (protocols/translate (first (:argv self)))} "(card({{set}}))")))

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
  (translate [self] (translation-error! self)))

;; --- Constructor functions ---

(defn plus [& args] (api/cacheing-validate (->TermPlus (vec args))))
(defn product [& args] (api/cacheing-validate (->TermProduct (vec args))))
(defn minus [& args] (api/cacheing-validate (->TermMinus (vec args))))
(defn divide [& args] (api/cacheing-validate (->TermDivide (vec args))))
(defn inc* [x] (api/cacheing-validate (->TermInc [x])))
(defn dec* [x] (api/cacheing-validate (->TermDec [x])))
(defn even?* [x] (api/cacheing-validate (->TermEven? [x])))
(defn odd?* [x] (api/cacheing-validate (->TermOdd? [x])))
(defn max* [& args] (api/cacheing-validate (->TermMax (vec args))))
(defn min* [& args] (api/cacheing-validate (->TermMin (vec args))))
(defn true?* [x] (api/cacheing-validate (->TermTrue? [x])))
(defn false?* [x] (api/cacheing-validate (->TermFalse? [x])))
(defn and* [& args] (api/cacheing-validate (->TermAnd (vec args))))
(defn or* [& args] (api/cacheing-validate (->TermOr (vec args))))
(defn when* [test body] (api/cacheing-validate (->TermWhen [test body])))
(defn not* [x] (api/cacheing-validate (->TermNot [x])))
(defn greater-than [& args] (api/cacheing-validate (->TermGreaterThan (vec args))))
(defn less-than [& args] (api/cacheing-validate (->TermLessThan (vec args))))
(defn gte [& args] (api/cacheing-validate (->TermGreaterThanOrEqualTo (vec args))))
(defn lte [& args] (api/cacheing-validate (->TermLessThanOrEqualTo (vec args))))
(defn equals [& args] (api/cacheing-validate (->TermEquals (vec args))))
(defn not-equals [& args] (not* (apply equals args)))
(defn iff [test then else] (api/cacheing-validate (->TermIf [test then else])))
(defn cond* [& args]
  (let [penultimate (get (vec args) (clojure.core/- (clojure.core/count args) 2))]
    (clojure.core/when-not (clojure.core/contains? #{:else :default} penultimate)
      (throw (ex-info "cond requires :else or :default" {})))
    (api/cacheing-validate (->TermCond (-> (drop-last 2 args) vec (conj (last args)))))))
(defn contains?* [set-expr elem] (api/cacheing-validate (->TermContains [set-expr elem])))
(defn pos?* [x] (api/cacheing-validate (->TermPos? [x])))
(defn neg?* [x] (api/cacheing-validate (->TermNeg? [x])))
(defn zero?* [x] (api/cacheing-validate (->TermZero? [x])))
(defn modulo [& args] (api/cacheing-validate (->TermMod (vec args))))
(defn remainder [& args] (api/cacheing-validate (->TermRem (vec args))))
(defn count* [x] (api/cacheing-validate (->TermCount [x])))
(defn nth* [elems idx]
  {:pre [(vector? elems) (clojure.core/>= (clojure.core/count elems) 1)]}
  (api/cacheing-validate (->TermNth (conj elems idx) (clojure.core/count elems))))

;; --- translate-comparator (moved from api) ---

(defn translate-comparator [self op constructor-fn]
  (case (clojure.core/count (:argv self))
    1 (protocols/translate true)
    2 (apply api/translate-binary-operation op (map protocols/translate (:argv self)))
    (->> (:argv self)
         (partition 2 1)
         (map (fn [[a b]] (constructor-fn a b)))
         (apply conjunction)
         protocols/translate)))

;; --- conjunction/disjunction (moved from api) ---

(defn conjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur (and* expr (first more)) (rest more))
      expr)))

(defn disjunction [& args]
  (loop [expr (first args)
         more (rest args)]
    (if (seq more)
      (recur (or* expr (first more)) (rest more))
      expr)))
