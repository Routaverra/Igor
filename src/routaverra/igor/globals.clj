(ns routaverra.igor.globals
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.types :as types]
            [routaverra.igor.terms.core :as terms]))

;; ============================================================
;; Graph constraints
;; ============================================================

(defrecord TermCircuit [argv]
  protocols/IInclude
  (mzn-includes [_self] #{"circuit.mzn"})
  protocols/IExpress
  (write [_self] (apply list 'circuit (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (str "circuit("
         (terms/to-literal-array (map #(str (protocols/translate %) " + 1") (:argv self)))
         ")")))

(defrecord TermSubCircuit [argv]
  protocols/IInclude
  (mzn-includes [_self] #{"subcircuit.mzn"})
  protocols/IExpress
  (write [_self] (apply list 'subcircuit (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take (count argv) (repeat {types/Numeric self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (str "subcircuit("
         (terms/to-literal-array (map #(str (protocols/translate %) " + 1") (:argv self)))
         ")")))

(defrecord TermDPath [argv n e from-arr to-arr]
  protocols/IInclude
  (mzn-includes [_self] #{"path.mzn"})
  protocols/IExpress
  (write [_self] (list 'dpath n e from-arr to-arr (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self}]
            (repeat (+ n e) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [s-str (protocols/translate (nth argv 0))
          t-str (protocols/translate (nth argv 1))
          ns-vars (subvec argv 2 (+ 2 n))
          es-vars (subvec argv (+ 2 n))
          from-str (terms/to-literal-array (map #(str (inc %)) from-arr))
          to-str (terms/to-literal-array (map #(str (inc %)) to-arr))
          ns-str (terms/to-literal-array (map protocols/translate ns-vars))
          es-str (terms/to-literal-array (map protocols/translate es-vars))]
      (str "dpath(" n ", " e ", " from-str ", " to-str ", "
           "(" s-str " + 1), (" t-str " + 1), " ns-str ", " es-str ")"))))

(defrecord TermBoundedDPath [argv n e from-arr to-arr weight-arr]
  protocols/IInclude
  (mzn-includes [_self] #{"path.mzn"})
  protocols/IExpress
  (write [_self] (list 'bounded-dpath n e from-arr to-arr weight-arr (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self} {types/Numeric self}]
            (repeat (+ n e) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [s-str (protocols/translate (nth argv 0))
          t-str (protocols/translate (nth argv 1))
          k-str (protocols/translate (nth argv 2))
          ns-vars (subvec argv 3 (+ 3 n))
          es-vars (subvec argv (+ 3 n))
          from-str (terms/to-literal-array (map #(str (inc %)) from-arr))
          to-str (terms/to-literal-array (map #(str (inc %)) to-arr))
          w-str (terms/to-literal-array (map str weight-arr))
          ns-str (terms/to-literal-array (map protocols/translate ns-vars))
          es-str (terms/to-literal-array (map protocols/translate es-vars))]
      (str "bounded_dpath(" n ", " e ", " from-str ", " to-str ", " w-str ", "
           "(" s-str " + 1), (" t-str " + 1), " ns-str ", " es-str ", " k-str ")"))))

;; --- Constructor functions ---

(defn circuit [& args]
  (if (every? terms/ground? args)
    (let [vals (vec args)
          n (count vals)]
      (and (every? #(and (integer? %) (>= % 0) (< % n)) vals)
           (= n (count (distinct vals)))
           (not-any? #(= % (nth vals %)) (range n))
           (loop [node 0 steps 0]
             (if (= steps n) (zero? node)
                 (recur (nth vals node) (inc steps))))))
    (api/cacheing-validate (->TermCircuit (vec args)))))

(defn subcircuit [& args]
  (if (every? terms/ground? args)
    (let [vals (vec args)
          n (count vals)]
      (and (every? #(and (integer? %) (>= % 0) (< % n)) vals)
           (loop [unvisited (set (filter #(not= % (nth vals %)) (range n)))]
             (if (empty? unvisited)
               true
               (let [start (first unvisited)
                     cycle-nodes (loop [node start seen #{}]
                                   (cond
                                     (seen node) (when (= node start) seen)
                                     (not (unvisited node)) nil
                                     :else (recur (nth vals node) (conj seen node))))]
                 (when cycle-nodes
                   (recur (clojure.set/difference unvisited cycle-nodes))))))))
    (api/cacheing-validate (->TermSubCircuit (vec args)))))

(defn dpath [n e from-vec to-vec source target ns-vec es-vec]
  {:pre [(integer? n) (integer? e)
         (vector? from-vec) (vector? to-vec)
         (= e (count from-vec)) (= e (count to-vec))
         (vector? ns-vec) (= n (count ns-vec))
         (vector? es-vec) (= e (count es-vec))]}
  (let [argv (vec (concat [source target] ns-vec es-vec))]
    (api/cacheing-validate (->TermDPath argv n e (vec from-vec) (vec to-vec)))))

(defn bounded-dpath [n e from-vec to-vec weight-vec source target ns-vec es-vec cost-var]
  {:pre [(integer? n) (integer? e)
         (vector? from-vec) (vector? to-vec) (vector? weight-vec)
         (= e (count from-vec)) (= e (count to-vec)) (= e (count weight-vec))
         (vector? ns-vec) (= n (count ns-vec))
         (vector? es-vec) (= e (count es-vec))]}
  (let [argv (vec (concat [source target cost-var] ns-vec es-vec))]
    (api/cacheing-validate (->TermBoundedDPath argv n e (vec from-vec) (vec to-vec) (vec weight-vec)))))
