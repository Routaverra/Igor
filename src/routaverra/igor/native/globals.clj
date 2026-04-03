(ns routaverra.igor.native.globals
  "Global constraint propagators: AllDifferent (value removal + bounds check)
   and Table (simple filter-and-intersect)."
  (:require [routaverra.igor.native.domains :as domains]
            [routaverra.igor.api :as api]))

;; ============================================================
;; AllDifferent — Value Consistency + Global Bounds Check
;;
;; Current implementation uses two-phase propagation:
;; 1. Value removal: when a variable is assigned, remove its value
;;    from all other variables.
;; 2. Bounds check: if the number of variables exceeds the range
;;    [min(mins)..max(maxes)], fail immediately.
;;
;; ## Consistency levels (from weakest to strongest)
;;
;; Value consistency (what we do):
;;   - O(n) per propagation
;;   - Only prunes when a variable is assigned
;;   - Global bounds check catches obvious infeasibility
;;
;; Bounds consistency (Hall intervals, Lopez-Ortiz et al. 2003):
;;   - O(n log n) per propagation
;;   - Finds groups of k variables whose combined range spans exactly
;;     k values, and prunes those values from all other variables
;;   - Standard in Gecode and Chuffed
;;
;; Domain consistency (maximum bipartite matching):
;;   - O(n * sqrt(n)) per propagation
;;   - Strongest pruning — removes any value that doesn't participate
;;     in a maximum matching
;;   - Used optionally in Gecode
;;
;; ## When to upgrade
;;
;; The current level is adequate when alldifferent is one constraint
;; among many (SEND+MORE=MONEY, graph coloring). The gap becomes
;; noticeable when alldifferent is the primary constraint and domains
;; overlap heavily — large N-queens (N>12), latin squares, scheduling
;; with many workers. If solve times on such problems become a
;; bottleneck, implement bounds consistency via the sweep-line Hall
;; interval algorithm.
;; ============================================================

(defn- alldifferent-propagate [decisions store]
  (let [n (count decisions)
        ;; Phase 1: Remove assigned values from other variables
        store (reduce
               (fn [store vi]
                 (if (= store ::domains/failed)
                   ::domains/failed
                   (let [di (get store vi)]
                     (if (domains/assigned? di)
                       (let [val (domains/domain-min di)]
                         (reduce
                          (fn [store vj]
                            (if (or (= store ::domains/failed) (= vi vj))
                              store
                              (let [dj (get store vj)
                                    result (domains/remove-value dj val)]
                                (if (= result ::domains/failed)
                                  ::domains/failed
                                  (let [[new-dj _] result]
                                    (assoc store vj new-dj))))))
                          store
                          decisions))
                       store))))
               store
               decisions)]
    (if (= store ::domains/failed)
      ::domains/failed
      ;; Phase 2: Bounds check — n variables need n distinct values,
      ;; so the range must be at least n wide
      (let [global-min (apply min (map #(domains/domain-min (get store %)) decisions))
            global-max (apply max (map #(domains/domain-max (get store %)) decisions))
            range-size (inc (- global-max global-min))]
        (if (< range-size n)
          ::domains/failed
          store)))))

(defn alldifferent-propagator
  "Create an AllDifferent propagator."
  [decisions]
  (let [dec-set (set decisions)]
    {:id (gensym "alldiff-")
     :vars dec-set
     :events (into {} (map (fn [d] [d #{:assigned :bounds}]) decisions))
     :priority 3
     :propagate-fn (fn [store]
                     (alldifferent-propagate decisions store))}))

;; ============================================================
;; Table — Simple filter-and-intersect
;;
;; For each propagation:
;; 1. Filter tuples to those consistent with current domains.
;; 2. For each variable position, collect the set of values
;;    appearing in surviving tuples.
;; 3. Intersect each variable's domain with that set.
;; ============================================================

(defn- tuple-consistent?
  "Check if a tuple is consistent with current variable domains."
  [store decisions tuple kw-map]
  (every? (fn [[decision value]]
            (let [d (get store decision)
                  v (if (keyword? value)
                      (get-in kw-map [:kw->int value] value)
                      value)]
              (domains/contains-value? d v)))
          (map vector decisions tuple)))

(defn table-propagator
  "Create a Table propagator. Filters tuples against current domains
   and restricts each variable to values that appear in surviving tuples."
  [decisions tuples kw-map]
  (let [dec-set (set decisions)
        n (count decisions)]
    {:id (gensym "table-")
     :vars dec-set
     :events (into {} (map (fn [d] [d #{:bounds :domain}]) decisions))
     :priority 4
     :propagate-fn
     (fn [store]
       (let [surviving (filter #(tuple-consistent? store decisions % kw-map) tuples)]
         (if (empty? surviving)
           ::domains/failed
           (reduce
            (fn [store i]
              (if (= store ::domains/failed)
                ::domains/failed
                (let [decision (nth decisions i)
                      d (get store decision)
                      allowed-values (into (sorted-set)
                                          (map (fn [tuple]
                                                 (let [v (nth tuple i)]
                                                   (if (keyword? v)
                                                     (get-in kw-map [:kw->int v] v)
                                                     v)))
                                               surviving))
                      allowed-domain (domains/->EnumeratedDomain allowed-values)
                      result (domains/intersect-domain d allowed-domain)]
                  (if (= result ::domains/failed)
                    ::domains/failed
                    (let [[new-d _] result]
                      (assoc store decision new-d))))))
            store
            (range n)))))}))
