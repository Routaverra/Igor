(ns routaverra.igor.native.propagators
  (:require [routaverra.igor.native.domains :as domains]
            [routaverra.igor.native.globals :as globals]
            [routaverra.igor.api :as api]
            [routaverra.igor.types :as types]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.terms.core :as terms]
            [routaverra.igor.extensional :as extensional])
  (:import [routaverra.igor.terms.core
            TermPlus TermProduct TermMinus TermDivide
            TermInc TermDec TermMax TermMin TermAbs TermPow
            TermAnd TermOr TermNot TermIf TermCond
            TermGreaterThan TermLessThan
            TermGreaterThanOrEqualTo TermLessThanOrEqualTo
            TermEquals TermAllDifferent
            TermRem TermMod
            TermTrue? TermFalse? TermPos? TermNeg? TermZero?
            TermContains TermCount TermNth
            TermEven? TermOdd?]
           [routaverra.igor.extensional TermTable]))

;; ---- Helpers ----

(defn decision? [x] (api/decision? x))

(defn to-int
  "Convert a ground value to an integer for domain operations."
  [v kw-map]
  (cond
    (integer? v) v
    (instance? Boolean v) (if v 1 0)
    (keyword? v) (get-in kw-map [:kw->int v])
    (number? v) (long v)
    :else (throw (ex-info (str "Cannot convert to int: " v) {:v v}))))

(defn resolve-bounds
  "Get [lo hi] for an arg, which may be a Decision (looked up in store) or a constant."
  [store arg kw-map]
  (if (decision? arg)
    (let [d (get store arg)]
      [(domains/domain-min d) (domains/domain-max d)])
    (let [v (to-int arg kw-map)]
      [v v])))

(defn safe-restrict-min
  "Restrict decision's domain min in store. Returns store or ::failed."
  [store decision new-min]
  (if (= store ::domains/failed)
    ::domains/failed
    (let [d (get store decision)
          result (domains/restrict-min d new-min)]
      (if (= result ::domains/failed)
        ::domains/failed
        (let [[new-d _] result]
          (assoc store decision new-d))))))

(defn safe-restrict-max
  "Restrict decision's domain max in store. Returns store or ::failed."
  [store decision new-max]
  (if (= store ::domains/failed)
    ::domains/failed
    (let [d (get store decision)
          result (domains/restrict-max d new-max)]
      (if (= result ::domains/failed)
        ::domains/failed
        (let [[new-d _] result]
          (assoc store decision new-d))))))

(defn safe-remove-value
  "Remove a value from decision's domain. Returns store or ::failed."
  [store decision val]
  (if (= store ::domains/failed)
    ::domains/failed
    (let [d (get store decision)
          result (domains/remove-value d val)]
      (if (= result ::domains/failed)
        ::domains/failed
        (let [[new-d _] result]
          (assoc store decision new-d))))))

(defn safe-intersect
  "Intersect decision's domain with another domain. Returns store or ::failed."
  [store decision other-domain]
  (if (= store ::domains/failed)
    ::domains/failed
    (let [d (get store decision)
          result (domains/intersect-domain d other-domain)]
      (if (= result ::domains/failed)
        ::domains/failed
        (let [[new-d _] result]
          (assoc store decision new-d))))))

(defn decision-args
  "Return only the Decision args from argv."
  [argv]
  (filter decision? argv))

(defn make-events-map
  "Create an events subscription map for the given decisions with the given event types."
  [decisions event-types]
  (into {} (map (fn [d] [d event-types]) decisions)))

;; ---- Compile constraint multimethod ----

(defmulti compile-constraint
  "Compile a constraint term into a seq of propagator maps."
  (fn [term kw-map] (type term)))

;; ---- Compile defining equality (= result-var term) ----

(defmulti compile-defining-equality
  "Compile (= result-var term-record) into propagator(s)."
  (fn [result-var term kw-map] (type term)))

;; ---- TermAnd (root level: all children must hold) ----

(defmethod compile-constraint TermAnd [term kw-map]
  (let [children (:argv term)]
    (mapcat
     (fn [child]
       (if (decision? child)
         ;; Boolean decision must be true (= 1)
         [{:id (gensym "force-true-")
           :vars #{child}
           :events {child #{:bounds}}
           :priority 0
           :propagate-fn (fn [store]
                           (safe-restrict-min store child 1))}]
         ;; A term — compile it as a constraint
         (compile-constraint child kw-map)))
     children)))

;; ---- TermOr (root level: at least one child must hold) ----

(defmethod compile-constraint TermOr [term kw-map]
  ;; If a root-level OR, we need to check if any child can still be true
  ;; and force truth when only one option remains
  (let [children (:argv term)
        dec-children (filter decision? children)
        term-children (remove decision? children)]
    (if (and (every? decision? children) (seq children))
      ;; All children are boolean decisions — at least one must be 1
      [{:id (gensym "or-root-")
        :vars (set children)
        :events (make-events-map children #{:assigned :bounds})
        :priority 1
        :propagate-fn (fn [store]
                        (let [vals (map #(get store %) children)
                              ;; Check if any is already true
                              any-true (some #(and (domains/assigned? %) (= 1 (domains/domain-min %))) vals)
                              ;; Check which can still be true
                              can-be-true (filter #(>= (domains/domain-max (get store %)) 1) children)]
                          (cond
                            any-true store ;; already satisfied
                            (empty? can-be-true) ::domains/failed
                            (= 1 (count can-be-true)) (safe-restrict-min store (first can-be-true) 1)
                            :else store)))}]
      ;; Mixed or complex OR — compile all sub-constraints
      ;; This is a simplification; true OR propagation is handled via reification
      (mapcat #(compile-constraint % kw-map) children))))

;; ---- TermEquals ----

(defmethod compile-constraint TermEquals [term kw-map]
  (let [argv (:argv term)
        n (count argv)]
    (cond
      ;; Binary case (most common after flattening)
      (= n 2)
      (let [[a b] argv]
        (cond
          ;; (= Decision Decision) — equality
          ;; Must come before IRecord check since Decisions are IRecords
          (and (decision? a) (decision? b))
          [{:id (gensym "eq-dd-")
            :vars #{a b}
            :events {a #{:bounds :domain} b #{:bounds :domain}}
            :priority 1
            :propagate-fn (fn [store]
                            (let [da (get store a) db (get store b)
                                  result (domains/intersect-domain da db)]
                              (if (= result ::domains/failed)
                                ::domains/failed
                                (let [[new-d _] result]
                                  (-> store
                                      (assoc a new-d)
                                      (assoc b new-d))))))}]

          ;; (= Decision TermRecord) — defining equality
          (and (decision? a) (instance? clojure.lang.IRecord b)
               (satisfies? protocols/IExpress b))
          (compile-defining-equality a b kw-map)

          (and (decision? b) (instance? clojure.lang.IRecord a)
               (satisfies? protocols/IExpress a))
          (compile-defining-equality b a kw-map)

          ;; (= Decision constant)
          (and (decision? a) (not (decision? b)))
          (let [v (to-int b kw-map)]
            [{:id (gensym "eq-dc-")
              :vars #{a}
              :events {}
              :priority 0
              :propagate-fn (fn [store]
                              (safe-intersect store a (domains/make-singleton v)))}])

          (and (decision? b) (not (decision? a)))
          (let [v (to-int a kw-map)]
            [{:id (gensym "eq-cd-")
              :vars #{b}
              :events {}
              :priority 0
              :propagate-fn (fn [store]
                              (safe-intersect store b (domains/make-singleton v)))}])

          ;; (= constant constant) — static check
          :else []))

      ;; N-ary: decompose to pairwise
      (> n 2)
      (let [pairs (partition 2 1 argv)]
        (mapcat (fn [[a b]]
                  (compile-constraint (terms/->TermEquals [a b]) kw-map))
                pairs))

      :else [])))

;; ---- TermNot ----

(defmethod compile-constraint TermNot [term kw-map]
  (let [[child] (:argv term)]
    (cond
      ;; (not decision) — decision must be false (0)
      (decision? child)
      [{:id (gensym "force-false-")
        :vars #{child}
        :events {}
        :priority 0
        :propagate-fn (fn [store]
                        (safe-restrict-max store child 0))}]

      ;; (not (= x y)) — not-equals at top level
      (instance? TermEquals child)
      (let [[a b] (:argv child)]
        (if (and (decision? a) (decision? b))
          [{:id (gensym "neq-")
            :vars #{a b}
            :events {a #{:assigned} b #{:assigned}}
            :priority 1
            :propagate-fn (fn [store]
                            (let [da (get store a) db (get store b)]
                              (cond
                                (and (domains/assigned? da) (domains/assigned? db))
                                (if (= (domains/domain-min da) (domains/domain-min db))
                                  ::domains/failed
                                  store)

                                (domains/assigned? da)
                                (safe-remove-value store b (domains/domain-min da))

                                (domains/assigned? db)
                                (safe-remove-value store a (domains/domain-min db))

                                :else store)))}]
          ;; (not (= decision constant))
          (let [[d c] (if (decision? a) [a b] [b a])]
            (if (decision? d)
              (let [v (to-int c kw-map)]
                [{:id (gensym "neq-dc-")
                  :vars #{d}
                  :events {d #{:assigned}}
                  :priority 1
                  :propagate-fn (fn [store]
                                  (safe-remove-value store d v))}])
              []))))

      ;; (not some-other-term) — compile the term, treat as negation
      ;; This shouldn't normally happen after flattening
      :else (compile-constraint child kw-map))))

;; ---- Comparison constraints (top-level) ----

(defmethod compile-constraint TermGreaterThan [term kw-map]
  (let [[a b] (:argv term)
        [alo ahi] (if (decision? a) nil (let [v (to-int a kw-map)] [v v]))
        [blo bhi] (if (decision? b) nil (let [v (to-int b kw-map)] [v v]))
        vars (set (filter decision? [a b]))]
    [{:id (gensym "gt-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[alo ahi] (resolve-bounds store a kw-map)
                            [blo bhi] (resolve-bounds store b kw-map)]
                        (cond-> store
                          (decision? a) (safe-restrict-min a (inc blo))
                          (decision? b) (safe-restrict-max b (dec ahi)))))}]))

(defmethod compile-constraint TermLessThan [term kw-map]
  (let [[a b] (:argv term)
        vars (set (filter decision? [a b]))]
    [{:id (gensym "lt-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[alo ahi] (resolve-bounds store a kw-map)
                            [blo bhi] (resolve-bounds store b kw-map)]
                        (cond-> store
                          (decision? a) (safe-restrict-max a (dec blo))
                          (decision? b) (safe-restrict-min b (inc ahi)))))}]))

(defmethod compile-constraint TermGreaterThanOrEqualTo [term kw-map]
  (let [[a b] (:argv term)
        vars (set (filter decision? [a b]))]
    [{:id (gensym "gte-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[alo ahi] (resolve-bounds store a kw-map)
                            [blo bhi] (resolve-bounds store b kw-map)]
                        (cond-> store
                          (decision? a) (safe-restrict-min a blo)
                          (decision? b) (safe-restrict-max b ahi))))}]))

(defmethod compile-constraint TermLessThanOrEqualTo [term kw-map]
  (let [[a b] (:argv term)
        vars (set (filter decision? [a b]))]
    [{:id (gensym "lte-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[alo ahi] (resolve-bounds store a kw-map)
                            [blo bhi] (resolve-bounds store b kw-map)]
                        (cond-> store
                          (decision? a) (safe-restrict-max a bhi)
                          (decision? b) (safe-restrict-min b alo))))}]))

;; ---- AllDifferent (bounds consistency via Hall intervals) ----

(defmethod compile-constraint TermAllDifferent [term kw-map]
  (let [decisions (vec (filter decision? (:argv term)))]
    [(globals/alldifferent-propagator decisions)]))

;; ---- Boolean literal at root level ----

(defmethod compile-constraint Boolean [term kw-map]
  (if term
    [] ;; true is trivially satisfied
    [{:id (gensym "fail-")
      :vars #{}
      :events {}
      :priority 0
      :propagate-fn (fn [_] ::domains/failed)}]))

;; ---- Defining equality dispatchers ----

;; Helper to make a propagator for z = f(args) where f does bounds propagation
(defn make-arith-propagator [id z args events-map priority propagate-fn]
  {:id id
   :vars (into #{z} (filter decision? args))
   :events events-map
   :priority priority
   :propagate-fn propagate-fn})

;; TermPlus: z = x + y
(defmethod compile-defining-equality TermPlus [z term kw-map]
  (let [[x y] (:argv term)
        vars (set (filter decision? [z x y]))]
    [{:id (gensym "plus-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          ;; Forward: z = x + y
                          (decision? z) (safe-restrict-min z (+ xlo ylo))
                          (decision? z) (safe-restrict-max z (+ xhi yhi))
                          ;; Backward: x = z - y
                          (decision? x) (safe-restrict-min x (- zlo yhi))
                          (decision? x) (safe-restrict-max x (- zhi ylo))
                          ;; Backward: y = z - x
                          (decision? y) (safe-restrict-min y (- zlo xhi))
                          (decision? y) (safe-restrict-max y (- zhi xlo)))))}]))

;; TermMinus: z = x - y  =>  z + y = x
(defmethod compile-defining-equality TermMinus [z term kw-map]
  (let [[x y] (:argv term)
        vars (set (filter decision? [z x y]))]
    [{:id (gensym "minus-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          ;; z = x - y
                          (decision? z) (safe-restrict-min z (- xlo yhi))
                          (decision? z) (safe-restrict-max z (- xhi ylo))
                          ;; x = z + y
                          (decision? x) (safe-restrict-min x (+ zlo ylo))
                          (decision? x) (safe-restrict-max x (+ zhi yhi))
                          ;; y = x - z
                          (decision? y) (safe-restrict-min y (- xlo zhi))
                          (decision? y) (safe-restrict-max y (- xhi zlo)))))}]))

;; TermProduct: z = x * y
(defmethod compile-defining-equality TermProduct [z term kw-map]
  (let [[x y] (:argv term)
        vars (set (filter decision? [z x y]))]
    [{:id (gensym "product-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 3
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)
                            products [(* xlo ylo) (* xlo yhi) (* xhi ylo) (* xhi yhi)]
                            pmin (apply min products)
                            pmax (apply max products)]
                        (as-> store s
                          ;; Forward
                          (if (decision? z) (safe-restrict-min s z pmin) s)
                          (if (decision? z) (safe-restrict-max s z pmax) s)
                          ;; Backward for x: z / y
                          (if (and (decision? x) (not= 0 ylo) (not= 0 yhi)
                                   (or (pos? ylo) (neg? yhi)))
                            (let [divs [(quot zlo ylo) (quot zlo yhi) (quot zhi ylo) (quot zhi yhi)]
                                  ;; Account for rounding
                                  candidates (mapcat (fn [d] [(long (Math/floor (double d)))
                                                              (long (Math/ceil (double d)))]) divs)]
                              ;; Use conservative bounds
                              (-> s
                                  (safe-restrict-min x (apply min candidates))
                                  (safe-restrict-max x (apply max candidates))))
                            s)
                          ;; Backward for y: z / x
                          (if (and (decision? y) (not= 0 xlo) (not= 0 xhi)
                                   (or (pos? xlo) (neg? xhi)))
                            (let [divs [(quot zlo xlo) (quot zlo xhi) (quot zhi xlo) (quot zhi xhi)]
                                  candidates (mapcat (fn [d] [(long (Math/floor (double d)))
                                                              (long (Math/ceil (double d)))]) divs)]
                              (-> s
                                  (safe-restrict-min y (apply min candidates))
                                  (safe-restrict-max y (apply max candidates))))
                            s))))}]))

;; TermDivide: z = x / y (integer division)
(defmethod compile-defining-equality TermDivide [z term kw-map]
  (let [[x y] (:argv term)
        vars (set (filter decision? [z x y]))]
    [{:id (gensym "divide-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 3
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (if (and (<= ylo 0) (>= yhi 0))
                          ;; y range includes 0 — limited propagation
                          store
                          (let [divs [(quot xlo ylo) (quot xlo yhi) (quot xhi ylo) (quot xhi yhi)]
                                dmin (apply min divs)
                                dmax (apply max divs)]
                            (cond-> store
                              (decision? z) (safe-restrict-min z dmin)
                              (decision? z) (safe-restrict-max z dmax))))))}]))

;; TermRem: z = rem(x, y)
(defmethod compile-defining-equality TermRem [z term kw-map]
  (let [[x y] (:argv term)
        vars (set (filter decision? [z x y]))]
    [{:id (gensym "rem-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 3
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)
                            yabs-max (max (Math/abs (long ylo)) (Math/abs (long yhi)))
                            rmax (dec yabs-max)
                            rmin (- rmax)]
                        (cond-> store
                          (decision? z) (safe-restrict-min z (max rmin xlo))
                          (decision? z) (safe-restrict-max z (min rmax xhi)))))}]))

;; TermMod: expands via IExpand, shouldn't appear after expansion
;; But handle it defensively
(defmethod compile-defining-equality TermMod [z term kw-map]
  (compile-defining-equality z (protocols/expand term) kw-map))

;; TermMax: z = max(args...)
(defmethod compile-defining-equality TermMax [z term kw-map]
  (let [args (:argv term)
        vars (into #{z} (filter decision? args))]
    [{:id (gensym "max-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [bounds (map #(resolve-bounds store % kw-map) args)
                            all-lo (map first bounds)
                            all-hi (map second bounds)
                            z-lo (apply max all-lo)
                            z-hi (apply max all-hi)]
                        (as-> store s
                          (if (decision? z)
                            (-> s (safe-restrict-min z z-lo)
                                  (safe-restrict-max z z-hi))
                            s)
                          ;; Each arg's max <= z's max
                          (reduce (fn [s arg]
                                    (if (decision? arg)
                                      (safe-restrict-max s arg (domains/domain-max (get s z)))
                                      s))
                                  s args))))}]))

;; TermMin: z = min(args...)
(defmethod compile-defining-equality TermMin [z term kw-map]
  (let [args (:argv term)
        vars (into #{z} (filter decision? args))]
    [{:id (gensym "min-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [bounds (map #(resolve-bounds store % kw-map) args)
                            all-lo (map first bounds)
                            all-hi (map second bounds)
                            z-lo (apply min all-lo)
                            z-hi (apply min all-hi)]
                        (as-> store s
                          (if (decision? z)
                            (-> s (safe-restrict-min z z-lo)
                                  (safe-restrict-max z z-hi))
                            s)
                          ;; Each arg's min >= z's min
                          (reduce (fn [s arg]
                                    (if (decision? arg)
                                      (safe-restrict-min s arg (domains/domain-min (get s z)))
                                      s))
                                  s args))))}]))

;; TermAbs: z = |x|
(defmethod compile-defining-equality TermAbs [z term kw-map]
  (let [[x] (:argv term)
        vars (set (filter decision? [z x]))]
    [{:id (gensym "abs-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            zlo (cond
                                  (>= xlo 0) xlo
                                  (<= xhi 0) (- xhi)
                                  :else 0)
                            zhi (max (Math/abs (long xlo)) (Math/abs (long xhi)))]
                        (cond-> store
                          (decision? z) (safe-restrict-min z zlo)
                          (decision? z) (safe-restrict-max z zhi)
                          (decision? z) (safe-restrict-min z 0))))}]))

;; TermPow: z = x^n (n is always ground)
(defmethod compile-defining-equality TermPow [z term kw-map]
  (let [[x n-expr] (:argv term)
        n (if (decision? n-expr) nil (to-int n-expr kw-map))
        vars (set (filter decision? [z x]))]
    [{:id (gensym "pow-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 3
      :propagate-fn (fn [store]
                      (if (nil? n)
                        store ;; Can't propagate if n is variable
                        (let [[xlo xhi] (resolve-bounds store x kw-map)
                              pvals [(long (Math/pow xlo n)) (long (Math/pow xhi n))]
                              ;; For even powers, 0 might be min
                              pvals (if (and (even? n) (neg? xlo) (pos? xhi))
                                      (conj pvals 0)
                                      pvals)
                              pmin (apply min pvals)
                              pmax (apply max pvals)]
                          (cond-> store
                            (decision? z) (safe-restrict-min z pmin)
                            (decision? z) (safe-restrict-max z pmax)))))}]))

;; TermInc: z = x + 1
(defmethod compile-defining-equality TermInc [z term kw-map]
  (let [[x] (:argv term)
        vars (set (filter decision? [z x]))]
    [{:id (gensym "inc-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          (decision? z) (safe-restrict-min z (inc xlo))
                          (decision? z) (safe-restrict-max z (inc xhi))
                          (decision? x) (safe-restrict-min x (dec zlo))
                          (decision? x) (safe-restrict-max x (dec zhi)))))}]))

;; TermDec: z = x - 1
(defmethod compile-defining-equality TermDec [z term kw-map]
  (let [[x] (:argv term)
        vars (set (filter decision? [z x]))]
    [{:id (gensym "dec-")
      :vars vars
      :events (make-events-map (seq vars) #{:bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          (decision? z) (safe-restrict-min z (dec xlo))
                          (decision? z) (safe-restrict-max z (dec xhi))
                          (decision? x) (safe-restrict-min x (inc zlo))
                          (decision? x) (safe-restrict-max x (inc zhi)))))}]))

;; ---- Logical propagators (defining equality) ----

;; TermAnd: z = x AND y (boolean, 0/1)
(defmethod compile-defining-equality TermAnd [z term kw-map]
  (let [args (:argv term)
        vars (into #{z} (filter decision? args))]
    [{:id (gensym "and-def-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [dz (get store z)
                            arg-domains (map #(if (decision? %) (get store %) (domains/make-singleton (to-int % kw-map))) args)]
                        (cond
                          ;; z assigned to 1 → all args must be 1
                          (and (domains/assigned? dz) (= 1 (domains/domain-min dz)))
                          (reduce (fn [s arg]
                                    (if (decision? arg)
                                      (safe-restrict-min s arg 1)
                                      s))
                                  store args)

                          ;; Any arg assigned to 0 → z must be 0
                          (some #(and (domains/assigned? %) (= 0 (domains/domain-min %))) arg-domains)
                          (safe-restrict-max store z 0)

                          ;; z assigned to 0 and all args but one are 1 → that one must be 0
                          (and (domains/assigned? dz) (= 0 (domains/domain-max dz)))
                          (let [unassigned-args (filter #(and (decision? %) (not (domains/assigned? (get store %))))
                                                       args)
                                assigned-true-args (filter #(and (domains/assigned? (if (decision? %)
                                                                                     (get store %)
                                                                                     (domains/make-singleton (to-int % kw-map))))
                                                                (= 1 (domains/domain-min (if (decision? %)
                                                                                           (get store %)
                                                                                           (domains/make-singleton (to-int % kw-map))))))
                                                          args)]
                            (if (and (= 1 (count unassigned-args))
                                     (= (dec (count args)) (count assigned-true-args)))
                              (safe-restrict-max store (first unassigned-args) 0)
                              store))

                          ;; All args are 1 → z = 1
                          (every? #(and (domains/assigned? %) (= 1 (domains/domain-min %))) arg-domains)
                          (safe-restrict-min store z 1)

                          :else store)))}]))

;; TermOr: z = x OR y (boolean, 0/1)
(defmethod compile-defining-equality TermOr [z term kw-map]
  (let [args (:argv term)
        vars (into #{z} (filter decision? args))]
    [{:id (gensym "or-def-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [dz (get store z)
                            arg-domains (map #(if (decision? %) (get store %) (domains/make-singleton (to-int % kw-map))) args)]
                        (cond
                          ;; z assigned to 0 → all args must be 0
                          (and (domains/assigned? dz) (= 0 (domains/domain-max dz)))
                          (reduce (fn [s arg]
                                    (if (decision? arg)
                                      (safe-restrict-max s arg 0)
                                      s))
                                  store args)

                          ;; Any arg assigned to 1 → z must be 1
                          (some #(and (domains/assigned? %) (= 1 (domains/domain-min %))) arg-domains)
                          (safe-restrict-min store z 1)

                          ;; z assigned to 1 and all args but one are 0 → that one must be 1
                          (and (domains/assigned? dz) (= 1 (domains/domain-min dz)))
                          (let [unassigned-args (filter #(and (decision? %) (not (domains/assigned? (get store %))))
                                                       args)
                                assigned-false-args (filter #(and (domains/assigned? (if (decision? %)
                                                                                      (get store %)
                                                                                      (domains/make-singleton (to-int % kw-map))))
                                                                 (= 0 (domains/domain-max (if (decision? %)
                                                                                            (get store %)
                                                                                            (domains/make-singleton (to-int % kw-map))))))
                                                           args)]
                            (if (and (= 1 (count unassigned-args))
                                     (= (dec (count args)) (count assigned-false-args)))
                              (safe-restrict-min store (first unassigned-args) 1)
                              store))

                          ;; All args are 0 → z = 0
                          (every? #(and (domains/assigned? %) (= 0 (domains/domain-max %))) arg-domains)
                          (safe-restrict-max store z 0)

                          :else store)))}]))

;; TermNot: z = NOT x
(defmethod compile-defining-equality TermNot [z term kw-map]
  (let [[x] (:argv term)
        vars (set (filter decision? [z x]))]
    [{:id (gensym "not-def-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          ;; z = 1 - x
                          (decision? z) (safe-restrict-min z (- 1 xhi))
                          (decision? z) (safe-restrict-max z (- 1 xlo))
                          ;; x = 1 - z
                          (decision? x) (safe-restrict-min x (- 1 zhi))
                          (decision? x) (safe-restrict-max x (- 1 zlo)))))}]))

;; ---- Conditional propagators ----

;; TermIf: z = if(b, then, else)
(defmethod compile-defining-equality TermIf [z term kw-map]
  (let [[b then-expr else-expr] (:argv term)
        vars (into #{z} (filter decision? [b then-expr else-expr]))]
    [{:id (gensym "if-def-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 2
      :propagate-fn (fn [store]
                      (let [[blo bhi] (resolve-bounds store b kw-map)
                            [tlo thi] (resolve-bounds store then-expr kw-map)
                            [elo ehi] (resolve-bounds store else-expr kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond
                          ;; b = 1 → z = then
                          (and (= blo 1) (= bhi 1))
                          (cond-> store
                            (decision? z) (safe-restrict-min z tlo)
                            (decision? z) (safe-restrict-max z thi)
                            (decision? then-expr) (safe-restrict-min then-expr zlo)
                            (decision? then-expr) (safe-restrict-max then-expr zhi))

                          ;; b = 0 → z = else
                          (and (= blo 0) (= bhi 0))
                          (cond-> store
                            (decision? z) (safe-restrict-min z elo)
                            (decision? z) (safe-restrict-max z ehi)
                            (decision? else-expr) (safe-restrict-min else-expr zlo)
                            (decision? else-expr) (safe-restrict-max else-expr zhi))

                          ;; b unknown → z in union of then/else ranges
                          :else
                          (let [union-lo (min tlo elo)
                                union-hi (max thi ehi)]
                            (cond-> store
                              (decision? z) (safe-restrict-min z union-lo)
                              (decision? z) (safe-restrict-max z union-hi)
                              ;; If z outside then's range → b can't be 1
                              (and (decision? b) (> zlo thi))
                              (safe-restrict-max b 0)
                              (and (decision? b) (< zhi tlo))
                              (safe-restrict-max b 0)
                              ;; If z outside else's range → b can't be 0
                              (and (decision? b) (> zlo ehi))
                              (safe-restrict-min b 1)
                              (and (decision? b) (< zhi elo))
                              (safe-restrict-min b 1))))))}]))

;; TermCond: should be expanded to nested TermIf via IExpand before reaching here
;; But handle defensively
(defmethod compile-defining-equality TermCond [z term kw-map]
  (compile-defining-equality z (protocols/expand term) kw-map))

;; ---- Reified comparison propagators ----

;; (= bool_var (> x y))
(defmethod compile-defining-equality TermGreaterThan [b term kw-map]
  (let [[x y] (:argv term)
        vars (into #{b} (filter decision? [x y]))]
    [{:id (gensym "reif-gt-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[blo bhi] (resolve-bounds store b kw-map)
                            [xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)]
                        (cond
                          ;; b = 1 → x > y
                          (= blo bhi 1)
                          (cond-> store
                            (decision? x) (safe-restrict-min x (inc ylo))
                            (decision? y) (safe-restrict-max y (dec xhi)))

                          ;; b = 0 → x <= y
                          (= blo bhi 0)
                          (cond-> store
                            (decision? x) (safe-restrict-max x yhi)
                            (decision? y) (safe-restrict-min y xlo))

                          ;; Determine b from x, y
                          (> xlo yhi) (safe-restrict-min store b 1)
                          (<= xhi ylo) (safe-restrict-max store b 0)
                          :else store)))}]))

;; (= bool_var (< x y))
(defmethod compile-defining-equality TermLessThan [b term kw-map]
  (let [[x y] (:argv term)
        vars (into #{b} (filter decision? [x y]))]
    [{:id (gensym "reif-lt-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[blo bhi] (resolve-bounds store b kw-map)
                            [xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)]
                        (cond
                          (= blo bhi 1)
                          (cond-> store
                            (decision? x) (safe-restrict-max x (dec yhi))
                            (decision? y) (safe-restrict-min y (inc xlo)))

                          (= blo bhi 0)
                          (cond-> store
                            (decision? x) (safe-restrict-min x ylo)
                            (decision? y) (safe-restrict-max y xhi))

                          (< xhi ylo) (safe-restrict-min store b 1)
                          (>= xlo yhi) (safe-restrict-max store b 0)
                          :else store)))}]))

;; (= bool_var (>= x y))
(defmethod compile-defining-equality TermGreaterThanOrEqualTo [b term kw-map]
  (let [[x y] (:argv term)
        vars (into #{b} (filter decision? [x y]))]
    [{:id (gensym "reif-gte-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[blo bhi] (resolve-bounds store b kw-map)
                            [xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)]
                        (cond
                          (= blo bhi 1)
                          (cond-> store
                            (decision? x) (safe-restrict-min x ylo)
                            (decision? y) (safe-restrict-max y xhi))

                          (= blo bhi 0)
                          (cond-> store
                            (decision? x) (safe-restrict-max x (dec yhi))
                            (decision? y) (safe-restrict-min y (inc xlo)))

                          (>= xlo yhi) (safe-restrict-min store b 1)
                          (< xhi ylo) (safe-restrict-max store b 0)
                          :else store)))}]))

;; (= bool_var (<= x y))
(defmethod compile-defining-equality TermLessThanOrEqualTo [b term kw-map]
  (let [[x y] (:argv term)
        vars (into #{b} (filter decision? [x y]))]
    [{:id (gensym "reif-lte-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[blo bhi] (resolve-bounds store b kw-map)
                            [xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)]
                        (cond
                          (= blo bhi 1)
                          (cond-> store
                            (decision? x) (safe-restrict-max x yhi)
                            (decision? y) (safe-restrict-min y xlo))

                          (= blo bhi 0)
                          (cond-> store
                            (decision? x) (safe-restrict-min x (inc ylo))
                            (decision? y) (safe-restrict-max y (dec xhi)))

                          (<= xhi ylo) (safe-restrict-min store b 1)
                          (> xlo yhi) (safe-restrict-max store b 0)
                          :else store)))}]))

;; (= bool_var (= x y)) — reified equality
(defmethod compile-defining-equality TermEquals [b term kw-map]
  (let [[x y] (:argv term)
        vars (into #{b} (filter decision? [x y]))]
    [{:id (gensym "reif-eq-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[blo bhi] (resolve-bounds store b kw-map)
                            [xlo xhi] (resolve-bounds store x kw-map)
                            [ylo yhi] (resolve-bounds store y kw-map)]
                        (cond
                          ;; b = 1 → x = y (intersect)
                          (= blo bhi 1)
                          (let [new-lo (max xlo ylo)
                                new-hi (min xhi yhi)]
                            (if (> new-lo new-hi)
                              ::domains/failed
                              (cond-> store
                                (decision? x) (safe-restrict-min x new-lo)
                                (decision? x) (safe-restrict-max x new-hi)
                                (decision? y) (safe-restrict-min y new-lo)
                                (decision? y) (safe-restrict-max y new-hi))))

                          ;; b = 0 → x != y
                          (= blo bhi 0)
                          (cond
                            (and (= xlo xhi) (decision? y))
                            (safe-remove-value store y xlo)
                            (and (= ylo yhi) (decision? x))
                            (safe-remove-value store x ylo)
                            :else store)

                          ;; Both assigned and equal → b = 1
                          (and (= xlo xhi) (= ylo yhi) (= xlo ylo))
                          (safe-restrict-min store b 1)

                          ;; Domains don't overlap → b = 0
                          (or (> xlo yhi) (> ylo xhi))
                          (safe-restrict-max store b 0)

                          :else store)))}]))

;; ---- Reified logical inside defining equality ----

;; (= bool_var (and x y)) — handled by the TermAnd defining equality above
;; (= bool_var (or x y)) — handled by the TermOr defining equality above
;; (= bool_var (not x)) — handled by the TermNot defining equality above

;; ---- Predicate terms ----

;; TermTrue?: z = true?(x) → z = (x == true) → z = x (for booleans)
(defmethod compile-defining-equality TermTrue? [z term kw-map]
  (let [[x] (:argv term)
        vars (set (filter decision? [z x]))]
    [{:id (gensym "true?-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          (decision? z) (safe-restrict-min z xlo)
                          (decision? z) (safe-restrict-max z xhi)
                          (decision? x) (safe-restrict-min x zlo)
                          (decision? x) (safe-restrict-max x zhi))))}]))

;; TermFalse?: z = false?(x) → z = NOT x
(defmethod compile-defining-equality TermFalse? [z term kw-map]
  (let [[x] (:argv term)
        vars (set (filter decision? [z x]))]
    [{:id (gensym "false?-")
      :vars vars
      :events (make-events-map (seq vars) #{:assigned :bounds})
      :priority 1
      :propagate-fn (fn [store]
                      (let [[xlo xhi] (resolve-bounds store x kw-map)
                            [zlo zhi] (resolve-bounds store z kw-map)]
                        (cond-> store
                          (decision? z) (safe-restrict-min z (- 1 xhi))
                          (decision? z) (safe-restrict-max z (- 1 xlo))
                          (decision? x) (safe-restrict-min x (- 1 zhi))
                          (decision? x) (safe-restrict-max x (- 1 zlo)))))}]))

;; TermPos?: z = (x > 0)
(defmethod compile-defining-equality TermPos? [z term kw-map]
  (compile-defining-equality z (terms/->TermGreaterThan [(first (:argv term)) 0]) kw-map))

;; TermNeg?: z = (x < 0)
(defmethod compile-defining-equality TermNeg? [z term kw-map]
  (compile-defining-equality z (terms/->TermLessThan [(first (:argv term)) 0]) kw-map))

;; TermZero?: z = (x == 0)
(defmethod compile-defining-equality TermZero? [z term kw-map]
  (compile-defining-equality z (terms/->TermEquals [(first (:argv term)) 0]) kw-map))

;; TermEven?: should be expanded via IExpand
(defmethod compile-defining-equality TermEven? [z term kw-map]
  (compile-defining-equality z (protocols/expand term) kw-map))

;; TermOdd?: should be expanded via IExpand
(defmethod compile-defining-equality TermOdd? [z term kw-map]
  (compile-defining-equality z (protocols/expand term) kw-map))

;; ---- AllDifferent in defining equality ----

(defmethod compile-defining-equality TermAllDifferent [z term kw-map]
  ;; z = alldifferent(args) — z is a boolean
  ;; This is unusual but could happen. Treat as reified.
  ;; For now, compile the alldifferent and force z = 1
  (into
   [{:id (gensym "alldiff-force-")
     :vars #{z}
     :events {}
     :priority 0
     :propagate-fn (fn [store] (safe-restrict-min store z 1))}]
   (compile-constraint term kw-map)))

;; ---- Table constraint ----

(defmethod compile-constraint TermTable [term kw-map]
  (let [decisions (vec (filter decision? (:argv term)))]
    [(globals/table-propagator decisions (:tuples term) kw-map)]))

;; ---- Fallback for unsupported terms ----

(defmethod compile-constraint :default [term kw-map]
  ;; For terms we don't handle (e.g., set operations, regular/cost-regular),
  ;; return empty — they won't propagate but won't crash either.
  ;; The search will need to find solutions by enumeration.
  [])

(defmethod compile-defining-equality :default [z term kw-map]
  ;; Unknown defining equality — no propagation
  [])
