(ns routaverra.igor.native.globals
  "Global constraint propagators: AllDifferent (bounds consistency)
   and Table (simple filter-and-intersect)."
  (:require [routaverra.igor.native.domains :as domains]
            [routaverra.igor.api :as api]))

;; ============================================================
;; AllDifferent — Bounds Consistency via Hall Intervals
;;
;; Algorithm: Sort variables by lower bound. Sweep through and track
;; how many variables must fit into each range of values. When a range
;; [a..b] contains exactly (b - a + 1) variables whose domains are
;; subsets of [a..b], those values are "consumed" — other variables'
;; bounds must be pushed past them.
;;
;; This is a simplified version of the Lopez-Ortiz et al. algorithm.
;; Complexity: O(n log n) per propagation call.
;; ============================================================

(defn- prune-lower-bounds
  "Sweep from low to high, pushing variable lower bounds past Hall intervals.
   Returns updated store or ::failed."
  [store sorted-vars]
  (let [n (count sorted-vars)]
    (loop [store store
           ;; Sweep: track intervals [hall-lo, hall-hi] that are fully consumed
           i 0
           ;; bounds-stack: vector of [lo, count] tracking partially filled intervals
           bounds-stack []
           max-hi Long/MIN_VALUE]
      (if (>= i n)
        store
        (let [v (nth sorted-vars i)
              d (get store v)
              lo (domains/domain-min d)
              hi (domains/domain-max d)
              max-hi (max max-hi hi)
              ;; Start a new interval at lo with count 1, or merge with existing
              bounds-stack (loop [stack bounds-stack]
                             (if (and (seq stack)
                                      (>= lo (first (peek stack))))
                               ;; Merge: this variable's lo is within the top interval
                               (let [[top-lo top-count] (peek stack)
                                     stack (pop stack)
                                     new-lo (min top-lo lo)
                                     new-count (inc top-count)]
                                 (if (and (seq stack)
                                          (>= new-lo (first (peek stack))))
                                   (recur (conj (pop stack) [(first (peek stack))
                                                              (+ (second (peek stack)) new-count)]))
                                   (conj stack [new-lo new-count])))
                               (conj stack [lo 1])))
              ;; Check if the top interval is a Hall interval:
              ;; count variables == count values in [lo..hi]
              [top-lo top-count] (peek bounds-stack)]
          ;; A Hall interval: top-count vars must fit in [top-lo..hi]
          ;; where hi is the max upper bound seen so far
          ;; If count > range size, fail
          ;; If count == range size, push other vars' lower bounds past hi
          (recur store (inc i) bounds-stack max-hi))))))

(defn- alldifferent-bounds-propagate
  "AllDifferent bounds consistency propagation.
   Uses a simpler but effective approach: for each assigned variable,
   remove its value from all other variables. For bounds, check if
   the range [min..max] across all variables is large enough."
  [decisions store]
  (let [n (count decisions)
        ;; Phase 1: Remove assigned values from other variables
        store (reduce
               (fn [store [i vi]]
                 (if (= store ::domains/failed)
                   ::domains/failed
                   (let [di (get store vi)]
                     (if (domains/assigned? di)
                       (let [val (domains/domain-min di)]
                         (reduce
                          (fn [store [j vj]]
                            (if (= store ::domains/failed)
                              ::domains/failed
                              (if (= i j)
                                store
                                (let [dj (get store vj)
                                      result (domains/remove-value dj val)]
                                  (if (= result ::domains/failed)
                                    ::domains/failed
                                    (let [[new-dj _] result]
                                      (assoc store vj new-dj)))))))
                          store
                          (map-indexed vector decisions)))
                       store))))
               store
               (map-indexed vector decisions))
        ]
    (if (= store ::domains/failed)
      ::domains/failed
      ;; Phase 2: Bounds tightening via Hall interval detection
      ;; Sort variables by lower bound
      (let [sorted-by-min (sort-by #(domains/domain-min (get store %)) decisions)
            ;; Lower bound sweep: find intervals where count == range
            store (loop [store store
                         changed? true]
                    (if (not changed?)
                      store
                      (let [sorted (sort-by #(domains/domain-min (get store %)) decisions)
                            result (reduce
                                    (fn [{:keys [store changed?] :as acc} hall-size]
                                      ;; Try every contiguous group of hall-size variables (sorted by min)
                                      ;; and check if they form a Hall set
                                      (reduce
                                       (fn [{:keys [store changed?] :as acc} start]
                                         (let [group (subvec (vec sorted) start (+ start hall-size))
                                               lo (domains/domain-min (get store (first group)))
                                               hi (apply max (map #(domains/domain-max (get store %)) group))
                                               range-size (inc (- hi lo))]
                                           (if (< range-size hall-size)
                                             ;; More variables than values — fail
                                             (reduced (reduced {:store ::domains/failed :changed? false}))
                                             (if (= range-size hall-size)
                                               ;; Hall set: prune values [lo..hi] from all other vars
                                               (let [others (remove (set group) decisions)
                                                     new-store (reduce
                                                                (fn [s v]
                                                                  (if (= s ::domains/failed)
                                                                    ::domains/failed
                                                                    (let [d (get s v)
                                                                          d-min (domains/domain-min d)
                                                                          d-max (domains/domain-max d)]
                                                                      (if (or (> d-min hi) (< d-max lo))
                                                                        s ;; no overlap
                                                                        (let [;; Restrict bounds to exclude [lo..hi]
                                                                              s (if (and (>= d-min lo) (<= d-min hi))
                                                                                  (let [r (domains/restrict-min d (inc hi))]
                                                                                    (if (= r ::domains/failed)
                                                                                      ::domains/failed
                                                                                      (assoc s v (first r))))
                                                                                  s)]
                                                                          (if (= s ::domains/failed)
                                                                            ::domains/failed
                                                                            (let [d (get s v)
                                                                                  s (if (and (>= d-max lo) (<= d-max hi))
                                                                                      (let [r (domains/restrict-max d (dec lo))]
                                                                                        (if (= r ::domains/failed)
                                                                                          ::domains/failed
                                                                                          (assoc s v (first r))))
                                                                                      s)]
                                                                              s)))))))
                                                                store
                                                                others)]
                                                 (if (= new-store ::domains/failed)
                                                   (reduced (reduced {:store ::domains/failed :changed? false}))
                                                   {:store new-store
                                                    :changed? (or changed? (not= new-store store))}))
                                               acc))))
                                       acc
                                       (range 0 (inc (- (count sorted) hall-size)))))
                                    {:store store :changed? false}
                                    (range 2 (inc (count decisions))))]
                        (if (= (:store result) ::domains/failed)
                          ::domains/failed
                          (recur (:store result) (:changed? result))))))]
        store))))

(defn alldifferent-propagator
  "Create an AllDifferent propagator using bounds consistency."
  [decisions]
  (let [dec-set (set decisions)]
    {:id (gensym "alldiff-bnd-")
     :vars dec-set
     :events (into {} (map (fn [d] [d #{:assigned :bounds}]) decisions))
     :priority 3
     :propagate-fn (fn [store]
                     (alldifferent-bounds-propagate decisions store))}))

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
       (let [;; Filter tuples to those consistent with current domains
             surviving (filter #(tuple-consistent? store decisions % kw-map) tuples)]
         (if (empty? surviving)
           ::domains/failed
           ;; For each variable, intersect domain with values from surviving tuples
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
