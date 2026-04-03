(ns routaverra.igor.native.sets
  "Set variable propagators: intersection, union, difference,
   symmetric difference, subset, superset, contains, cardinality."
  (:require [routaverra.igor.native.domains :as domains]
            [routaverra.igor.api :as api]
            [clojure.set :as set]))

(defn- safe-require-all [store decision required]
  (if (= store ::domains/failed)
    ::domains/failed
    (let [d (get store decision)
          result (domains/set-require-all d required)]
      (if (= result ::domains/failed)
        ::domains/failed
        (let [[new-d _] result]
          (assoc store decision new-d))))))

(defn- safe-restrict-lub [store decision allowed]
  (if (= store ::domains/failed)
    ::domains/failed
    (let [d (get store decision)
          result (domains/set-restrict-lub d allowed)]
      (if (= result ::domains/failed)
        ::domains/failed
        (let [[new-d _] result]
          (assoc store decision new-d))))))

;; ---- Intersection: z = a ∩ b ----

(defn intersection-propagator [z a b]
  {:id (gensym "set-isect-")
   :vars #{z a b}
   :events {z #{:glb-change :lub-change}
            a #{:glb-change :lub-change}
            b #{:glb-change :lub-change}}
   :priority 3
   :propagate-fn
   (fn [store]
     (let [dz (get store z) da (get store a) db (get store b)]
       (-> store
           ;; z.GLB ⊇ a.GLB ∩ b.GLB
           (safe-require-all z (set/intersection (:glb da) (:glb db)))
           ;; z.LUB ⊆ a.LUB ∩ b.LUB
           (safe-restrict-lub z (set/intersection (:lub da) (:lub db)))
           ;; Backward: if v ∈ z.GLB and v ∈ b.GLB → v ∈ a.GLB
           (safe-require-all a (set/intersection (:glb (get store z)) (:glb db)))
           ;; Backward: if v ∈ z.GLB and v ∈ a.GLB → v ∈ b.GLB
           (safe-require-all b (set/intersection (:glb (get store z)) (:glb da))))))})

;; ---- Union: z = a ∪ b ----

(defn union-propagator [z a b]
  {:id (gensym "set-union-")
   :vars #{z a b}
   :events {z #{:glb-change :lub-change}
            a #{:glb-change :lub-change}
            b #{:glb-change :lub-change}}
   :priority 3
   :propagate-fn
   (fn [store]
     (let [dz (get store z) da (get store a) db (get store b)]
       (-> store
           ;; z.GLB ⊇ a.GLB ∪ b.GLB
           (safe-require-all z (set/union (:glb da) (:glb db)))
           ;; z.LUB ⊆ a.LUB ∪ b.LUB
           (safe-restrict-lub z (set/union (:lub da) (:lub db)))
           ;; Backward: if v ∈ z.GLB and v ∉ b.LUB → v ∈ a.GLB
           (safe-require-all a (set/difference (:glb (get store z)) (:lub db)))
           ;; Backward: if v ∈ z.GLB and v ∉ a.LUB → v ∈ b.GLB
           (safe-require-all b (set/difference (:glb (get store z)) (:lub da)))
           ;; Backward: if v ∉ z.LUB → v ∉ a.LUB and v ∉ b.LUB
           (safe-restrict-lub a (:lub (get store z)))
           (safe-restrict-lub b (:lub (get store z))))))})

;; ---- Difference: z = a \ b ----

(defn difference-propagator [z a b]
  {:id (gensym "set-diff-")
   :vars #{z a b}
   :events {z #{:glb-change :lub-change}
            a #{:glb-change :lub-change}
            b #{:glb-change :lub-change}}
   :priority 3
   :propagate-fn
   (fn [store]
     (let [dz (get store z) da (get store a) db (get store b)]
       (-> store
           ;; z.GLB ⊇ a.GLB \ b.LUB
           (safe-require-all z (set/difference (:glb da) (:lub db)))
           ;; z.LUB ⊆ a.LUB \ b.GLB
           (safe-restrict-lub z (set/difference (:lub da) (:glb db)))
           ;; Backward: if v ∈ z.GLB → v ∈ a.GLB
           (safe-require-all a (:glb (get store z)))
           ;; Backward: if v ∈ z.GLB → v ∉ b (exclude from b's LUB)
           (safe-restrict-lub b (set/difference (:lub (get store b)) (:glb (get store z)))))))})

;; ---- Symmetric Difference: z = a △ b ----

(defn symdiff-propagator [z a b]
  {:id (gensym "set-symdiff-")
   :vars #{z a b}
   :events {z #{:glb-change :lub-change}
            a #{:glb-change :lub-change}
            b #{:glb-change :lub-change}}
   :priority 3
   :propagate-fn
   (fn [store]
     (let [da (get store a) db (get store b)
           ;; z.LUB ⊆ (a.LUB ∪ b.LUB) \ (a.GLB ∩ b.GLB)
           possible (set/difference (set/union (:lub da) (:lub db))
                                    (set/intersection (:glb da) (:glb db)))]
       (-> store
           (safe-restrict-lub z possible))))})

;; ---- Subset: a ⊆ b (top-level constraint) ----

(defn subset-propagator [a b]
  {:id (gensym "set-subset-")
   :vars #{a b}
   :events {a #{:glb-change :lub-change}
            b #{:glb-change :lub-change}}
   :priority 2
   :propagate-fn
   (fn [store]
     (let [da (get store a) db (get store b)]
       (-> store
           ;; a.LUB ⊆ b.LUB
           (safe-restrict-lub a (:lub db))
           ;; b.GLB ⊇ a.GLB
           (safe-require-all b (:glb da)))))})

;; ---- Set Equality: a = b (for set variables) ----

(defn set-equality-propagator [a b]
  {:id (gensym "set-eq-")
   :vars #{a b}
   :events {a #{:glb-change :lub-change}
            b #{:glb-change :lub-change}}
   :priority 1
   :propagate-fn
   (fn [store]
     (let [da (get store a) db (get store b)
           ;; Unify GLBs and LUBs
           new-glb (set/union (:glb da) (:glb db))
           new-lub (set/intersection (:lub da) (:lub db))]
       (if (not (set/subset? new-glb new-lub))
         ::domains/failed
         (-> store
             (safe-require-all a new-glb)
             (safe-restrict-lub a new-lub)
             (safe-require-all b new-glb)
             (safe-restrict-lub b new-lub)))))})

;; ---- Set = literal set constant ----

(defn set-assign-propagator [decision set-value]
  {:id (gensym "set-assign-")
   :vars #{decision}
   :events {}
   :priority 0
   :propagate-fn
   (fn [store]
     (let [sv (into (sorted-set) set-value)]
       (-> store
           (safe-require-all decision sv)
           (safe-restrict-lub decision sv))))})

;; ---- Contains: v ∈ s ----

(defn contains-propagator [s v-decision-or-val kw-map]
  (if (api/decision? v-decision-or-val)
    ;; v is a variable — its domain must be within s.LUB
    {:id (gensym "set-contains-var-")
     :vars #{s v-decision-or-val}
     :events {s #{:lub-change :glb-change}
              v-decision-or-val #{:assigned :bounds}}
     :priority 2
     :propagate-fn
     (fn [store]
       (let [ds (get store s)
             dv (get store v-decision-or-val)]
         (if (domains/assigned? dv)
           ;; v is assigned → require in s
           (safe-require-all store s #{(domains/domain-min dv)})
           ;; v not assigned → restrict v to s.LUB
           (let [lub (:lub ds)
                 v-lo (apply min lub)
                 v-hi (apply max lub)]
             (cond-> store
               true (as-> s*
                          (let [d (get s* v-decision-or-val)
                                r (domains/restrict-min d v-lo)]
                            (if (= r ::domains/failed) ::domains/failed
                                (assoc s* v-decision-or-val (first r)))))
               (not= store ::domains/failed)
               (as-> s*
                     (let [d (get s* v-decision-or-val)
                           r (domains/restrict-max d v-hi)]
                       (if (= r ::domains/failed) ::domains/failed
                           (assoc s* v-decision-or-val (first r))))))))))}
    ;; v is a constant — require it in s.GLB
    (let [v (if (keyword? v-decision-or-val)
              (get-in kw-map [:kw->int v-decision-or-val] v-decision-or-val)
              v-decision-or-val)]
      {:id (gensym "set-contains-const-")
       :vars #{s}
       :events {}
       :priority 0
       :propagate-fn (fn [store]
                       (safe-require-all store s #{v}))})))

;; ---- Cardinality: n = |s| ----

(defn cardinality-propagator [n s]
  {:id (gensym "set-card-")
   :vars #{n s}
   :events {n #{:assigned :bounds}
            s #{:glb-change :lub-change}}
   :priority 2
   :propagate-fn
   (fn [store]
     (let [ds (get store s)
           dn (get store n)
           glb-size (count (:glb ds))
           lub-size (count (:lub ds))]
       (as-> store s*
         ;; n.min ≥ |s.GLB|
         (let [d (get s* n)
               r (domains/restrict-min d glb-size)]
           (if (= r ::domains/failed) ::domains/failed (assoc s* n (first r))))
         ;; n.max ≤ |s.LUB|
         (if (= s* ::domains/failed) ::domains/failed
             (let [d (get s* n)
                   r (domains/restrict-max d lub-size)]
               (if (= r ::domains/failed) ::domains/failed (assoc s* n (first r)))))
         ;; If n assigned to k:
         (if (= s* ::domains/failed) ::domains/failed
             (let [d (get s* n)]
               (if (domains/assigned? d)
                 (let [k (domains/domain-min d)
                       ds (get s* s)]
                   (cond
                     ;; |GLB| = k → close: LUB = GLB (no more elements)
                     (= (count (:glb ds)) k)
                     (safe-restrict-lub s* s (:glb ds))
                     ;; |LUB| = k → all elements required: GLB = LUB
                     (= (count (:lub ds)) k)
                     (safe-require-all s* s (:lub ds))
                     :else s*))
                 s*))))))})

;; ---- Reified subset: b ↔ (a ⊆ b-set) ----

(defn reified-subset-propagator [b a-var b-var]
  {:id (gensym "reif-subset-")
   :vars #{b a-var b-var}
   :events {b #{:assigned :bounds}
            a-var #{:glb-change :lub-change}
            b-var #{:glb-change :lub-change}}
   :priority 2
   :propagate-fn
   (fn [store]
     (let [db (get store b)
           da (get store a-var)
           dB (get store b-var)]
       (cond
         ;; b = 1 → enforce a ⊆ b-set
         (and (domains/assigned? db) (= 1 (domains/domain-min db)))
         (-> store
             (safe-restrict-lub a-var (:lub dB))
             (safe-require-all b-var (:glb da)))

         ;; b = 0 → a is NOT a subset of b-set
         ;; (weak: only if a.GLB has element outside b-set.LUB → already violated)
         (and (domains/assigned? db) (= 0 (domains/domain-min db)))
         store ;; hard to propagate negated subset

         ;; a.LUB ⊆ b-set.GLB → definitely subset → b = 1
         (set/subset? (:lub da) (:glb dB))
         (let [d (get store b)
               r (domains/restrict-min d 1)]
           (if (= r ::domains/failed) ::domains/failed (assoc store b (first r))))

         ;; a.GLB has element outside b-set.LUB → definitely NOT subset → b = 0
         (seq (set/difference (:glb da) (:lub dB)))
         (let [d (get store b)
               r (domains/restrict-max d 0)]
           (if (= r ::domains/failed) ::domains/failed (assoc store b (first r))))

         :else store)))})

;; ---- Reified superset: b ↔ (a ⊇ b-set) = b ↔ (b-set ⊆ a) ----

(defn reified-superset-propagator [b a-var b-var]
  ;; superset?(a, b) means b ⊆ a, so just swap args
  (reified-subset-propagator b b-var a-var))
