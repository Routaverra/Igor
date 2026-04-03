(ns routaverra.igor.native.search
  (:require [routaverra.igor.native.domains :as domains]
            [routaverra.igor.native.fixpoint :as fixpoint]
            [routaverra.igor.api :as api]))

(defn all-assigned?
  "Check if all decisions in the store are assigned (domain size 1)."
  [store merged-decisions]
  (every? (fn [decision]
            (domains/assigned? (get store decision)))
          (keys merged-decisions)))

(defn select-variable
  "First-fail heuristic: select unassigned variable with smallest domain.
   For integer variables, domain-size is the number of possible values.
   For set variables, domain-size is the number of undecided elements."
  [store merged-decisions]
  (->> (keys merged-decisions)
       (remove #(domains/assigned? (get store %)))
       (apply min-key #(let [d (get store %)]
                         (if (instance? routaverra.igor.native.domains.SetDomain d)
                           ;; For sets: number of undecided elements
                           (domains/domain-size d)
                           ;; For integers: domain size
                           (domains/domain-size d))))))

(defn- set-domain? [d] (instance? routaverra.igor.native.domains.SetDomain d))

(defn branch
  "Create two branches for a variable. For integer variables: assign min value
   vs remove it. For set variables: require an undecided element vs exclude it.
   Returns [[store1 events1] [store2 events2]] where each may be ::failed."
  [store var]
  (let [d (get store var)]
    (if (set-domain? d)
      ;; Set variable: pick first undecided element
      (let [undecided (first (clojure.set/difference (:lub d) (:glb d)))
            ;; Branch 1: require element (add to GLB)
            r1 (domains/set-require d undecided)
            b1 (if (= r1 ::domains/failed)
                 ::domains/failed
                 (let [[new-d events] r1]
                   [(assoc store var new-d) {var events}]))
            ;; Branch 2: exclude element (remove from LUB)
            r2 (domains/set-exclude d undecided)
            b2 (if (= r2 ::domains/failed)
                 ::domains/failed
                 (let [[new-d events] r2]
                   [(assoc store var new-d) {var events}]))]
        [b1 b2])
      ;; Integer variable: assign min vs remove min
      (let [val (domains/domain-min d)
            ;; Branch 1: assign var = val
            r1 (domains/intersect-domain d (domains/make-singleton val))
            b1 (if (= r1 ::domains/failed)
                 ::domains/failed
                 (let [[new-d events] r1]
                   [(assoc store var new-d) {var events}]))
            ;; Branch 2: remove val
            r2 (domains/remove-value d val)
            b2 (if (= r2 ::domains/failed)
                 ::domains/failed
                 (let [[new-d events] r2]
                   [(assoc store var new-d) {var events}]))]
        [b1 b2]))))

(defn- propagate-branch
  "Propagate a branch result. Returns store or ::failed."
  [propagators subscriptions branch-result]
  (if (= branch-result ::domains/failed)
    ::domains/failed
    (let [[store events] branch-result
          queue (fixpoint/events->queue events subscriptions propagators)]
      (fixpoint/propagate-fixpoint propagators store queue subscriptions))))

(defn solve-dfs
  "Depth-first search. Returns a fully-assigned store or nil."
  [propagators store subscriptions merged-decisions]
  (cond
    (= store ::domains/failed) nil
    (all-assigned? store merged-decisions) store
    :else
    (let [var (select-variable store merged-decisions)
          [b1 b2] (branch store var)]
      (or (let [store1 (propagate-branch propagators subscriptions b1)]
            (when-not (= store1 ::domains/failed)
              (solve-dfs propagators store1 subscriptions merged-decisions)))
          (let [store2 (propagate-branch propagators subscriptions b2)]
            (when-not (= store2 ::domains/failed)
              (solve-dfs propagators store2 subscriptions merged-decisions)))))))

(defn solve-all-dfs
  "Find all solutions via DFS. Returns vector of fully-assigned stores."
  [propagators store subscriptions merged-decisions]
  (cond
    (= store ::domains/failed) []
    (all-assigned? store merged-decisions) [store]
    :else
    (let [var (select-variable store merged-decisions)
          [b1 b2] (branch store var)
          store1 (propagate-branch propagators subscriptions b1)
          sols1 (if (= store1 ::domains/failed)
                  []
                  (solve-all-dfs propagators store1 subscriptions merged-decisions))
          store2 (propagate-branch propagators subscriptions b2)
          sols2 (if (= store2 ::domains/failed)
                  []
                  (solve-all-dfs propagators store2 subscriptions merged-decisions))]
      (into sols1 sols2))))

(defn solve-optimize
  "Branch-and-bound optimization. Returns best fully-assigned store or nil."
  [propagators store subscriptions objective-var direction merged-decisions]
  (loop [store store
         best nil]
    (let [sol (solve-dfs propagators store subscriptions merged-decisions)]
      (if (nil? sol)
        best
        (let [obj-val (domains/domain-min (get sol objective-var))
              bound-result (case direction
                             :minimize (domains/restrict-max (get store objective-var) (dec obj-val))
                             :maximize (domains/restrict-min (get store objective-var) (inc obj-val)))]
          (if (= bound-result ::domains/failed)
            sol
            (let [[new-obj-d obj-events] bound-result
                  bound-store (assoc store objective-var new-obj-d)
                  events {objective-var obj-events}
                  queue (fixpoint/events->queue events subscriptions propagators)
                  bound-store (fixpoint/propagate-fixpoint propagators bound-store queue subscriptions)]
              (if (= bound-store ::domains/failed)
                sol
                (recur bound-store sol)))))))))
