(ns igor.extensional
  (:require [igor.protocols :as protocols]
            [igor.api :as api]
            [igor.types :as types]
            [igor.terms.core :as terms]))

;; ============================================================
;; Helpers
;; ============================================================

(defn- mzn-set-str
  "Format a set of ints as a MiniZinc set literal: {-2,-1,0,1,2}"
  [s]
  (str "{" (apply str (interpose "," (sort s))) "}"))

(defn- array2d-str
  "MiniZinc array2d(1..rows, 1..cols, [flat...])"
  [rows cols flat-elements]
  (str "array2d(1.." rows ", 1.." cols ", "
       (terms/to-literal-array flat-elements) ")"))

(defn- flat-transition-table
  "Flatten transitions vec-of-maps into row-major order by sorted alphabet.
   Missing keys map to 0 (MiniZinc dead state)."
  [transitions alphabet-sorted]
  (for [state-map transitions
        sym alphabet-sorted]
    (inc (get state-map sym -1))))

(defn- flat-cost-table
  "Flatten costs vec-of-maps into row-major order by sorted alphabet.
   Missing keys map to 0."
  [costs alphabet-sorted]
  (for [cost-map costs
        sym alphabet-sorted]
    (get cost-map sym 0)))

;; ============================================================
;; DFA simulation (ground pass-through)
;; ============================================================

(defn simulate-dfa
  "Run a DFA on a ground sequence. Returns true if the DFA accepts."
  [{:keys [transitions start accept]} sequence]
  (loop [state start
         [x & xs] sequence]
    (if (nil? x)
      (contains? accept state)
      (let [next-state (get (nth transitions state) x)]
        (if (nil? next-state)
          false
          (recur next-state xs))))))

;; ============================================================
;; TermTable
;; ============================================================

(defrecord TermTable [argv tuples]
  protocols/IInclude
  (mzn-includes [_self] #{"table.mzn"})
  protocols/IExpress
  (write [_self] (list 'table (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat (count argv) {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [n-tuples (count (:tuples self))
          n-vars (count (:argv self))
          flat (apply concat (:tuples self))]
      (str "table("
           (terms/to-literal-array (map protocols/translate (:argv self)))
           ", "
           (array2d-str n-tuples n-vars flat)
           ")")))
  (evaluate [self solution]
    (let [vals (api/eval-argv self solution)]
      (boolean (some #(= (vec vals) (vec %)) tuples)))))

;; ============================================================
;; TermRegular (classic 1..S overload with offset remapping)
;; ============================================================

(defrecord TermRegular [argv dfa]
  protocols/IInclude
  (mzn-includes [_self] #{"regular.mzn"})
  protocols/IExpress
  (write [_self] (list 'regular (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat (count argv) {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [{:keys [states alphabet transitions start accept]} (:dfa self)
          alphabet-sorted (sort alphabet)
          offset (- 1 (first alphabet-sorted))
          s (count alphabet)
          q (long states)
          q0 (inc start)
          f-set (mzn-set-str (map inc accept))
          x-strs (map (fn [v]
                        (if (zero? offset)
                          (protocols/translate v)
                          (str "(" (protocols/translate v) " + " offset ")")))
                      (:argv self))
          flat (flat-transition-table transitions alphabet-sorted)]
      (str "regular("
           (terms/to-literal-array x-strs)
           ", " q ", " s ", "
           (array2d-str q s flat)
           ", " q0 ", " f-set ")")))
  (evaluate [self solution]
    (simulate-dfa dfa (api/eval-argv self solution))))

;; ============================================================
;; TermCostRegular (classic int overload, requires alphabet remapping)
;; ============================================================

(defn simulate-dfa-cost
  "Run a DFA on a ground sequence, accumulating cost. Returns [accepted? total-cost]."
  [{:keys [transitions start accept costs]} sequence]
  (loop [state start
         total-cost 0
         [x & xs] sequence]
    (if (nil? x)
      [(contains? accept state) total-cost]
      (let [next-state (get (nth transitions state) x)
            step-cost (get (nth costs state) x 0)]
        (if (nil? next-state)
          [false total-cost]
          (recur next-state (+ total-cost step-cost) xs))))))

(defrecord TermCostRegular [argv cost dfa]
  protocols/IInclude
  (mzn-includes [_self] #{"cost_regular.mzn"})
  protocols/IExpress
  (write [_self] (list 'cost-regular (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self}]
            (repeat (dec (count argv)) {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [{:keys [states alphabet transitions start accept costs]} (:dfa self)
          alphabet-sorted (sort alphabet)
          offset (- 1 (first alphabet-sorted))
          s (count alphabet)
          q (long states)
          q0 (inc start)
          f-set (mzn-set-str (map inc accept))
          x-vars (rest (:argv self))
          x-strs (map (fn [v]
                        (if (zero? offset)
                          (protocols/translate v)
                          (str "(" (protocols/translate v) " + " offset ")")))
                      x-vars)
          flat-d (flat-transition-table transitions alphabet-sorted)
          flat-c (flat-cost-table costs alphabet-sorted)]
      (str "cost_regular("
           (terms/to-literal-array x-strs)
           ", " q ", " s ", "
           (array2d-str q s flat-d)
           ", " q0 ", " f-set ", "
           (array2d-str q s flat-c)
           ", " (protocols/translate (:cost self)) ")")))
  (evaluate [self solution]
    (let [cost-val (api/eval-arg cost solution)
          seq-vals (mapv #(api/eval-arg % solution) (rest argv))
          [accepted? total-cost] (simulate-dfa-cost dfa seq-vals)]
      (and accepted? (= cost-val total-cost)))))

;; ============================================================
;; Constructor functions
;; ============================================================

(defn- ground-or-validate [term ground?]
  (if ground?
    (protocols/evaluate term {})
    (api/cacheing-validate term)))

(defn table
  "Constrain that the tuple of vars x is one of the allowed tuples.
   x is a vec of decision vars, tuples is a vec of vec of ints."
  [x tuples]
  {:pre [(vector? x) (seq tuples) (every? #(= (count %) (count x)) tuples)]}
  (ground-or-validate (->TermTable x tuples) (every? terms/ground? x)))

(defn regular
  "Constrain that the sequence x is accepted by the given DFA.
   x is a vec of decision vars.
   dfa is a map with keys :states :alphabet :transitions :start :accept.
   States are 0-indexed. Transitions is a vec of maps {symbol -> next-state}."
  [x dfa]
  {:pre [(vector? x) (seq x)
         (integer? (:states dfa))
         (set? (:alphabet dfa))
         (vector? (:transitions dfa))
         (= (:states dfa) (count (:transitions dfa)))
         (integer? (:start dfa))
         (set? (:accept dfa))]}
  (ground-or-validate (->TermRegular x dfa) (every? terms/ground? x)))

(defn cost-regular
  "Constrain that the sequence x is accepted by the given DFA, with cost.
   x is a vec of decision vars, cost is a decision var.
   dfa is like regular's but also has :costs (vec of maps {symbol -> cost})."
  [x cost dfa]
  {:pre [(vector? x) (seq x)
         (integer? (:states dfa))
         (set? (:alphabet dfa))
         (vector? (:transitions dfa))
         (= (:states dfa) (count (:transitions dfa)))
         (integer? (:start dfa))
         (set? (:accept dfa))
         (vector? (:costs dfa))
         (= (:states dfa) (count (:costs dfa)))]}
  (let [argv (vec (concat [cost] x))]
    (api/cacheing-validate (->TermCostRegular argv cost dfa))))
