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
  "First-fail heuristic: select unassigned variable with smallest domain."
  [store merged-decisions]
  (->> (keys merged-decisions)
       (remove #(domains/assigned? (get store %)))
       (apply min-key #(domains/domain-size (get store %)))))

(defn select-value
  "Select minimum value from variable's domain."
  [store var]
  (domains/domain-min (get store var)))

(defn branch-assign
  "Assign var=val in store. Returns [new-store events-map] or ::failed."
  [store var val]
  (let [d (get store var)
        result (domains/intersect-domain d (domains/make-singleton val))]
    (if (= result ::domains/failed)
      ::domains/failed
      (let [[new-d events] result]
        [(assoc store var new-d) {var events}]))))

(defn branch-remove
  "Remove val from var's domain. Returns [new-store events-map] or ::failed."
  [store var val]
  (let [d (get store var)
        result (domains/remove-value d val)]
    (if (= result ::domains/failed)
      ::domains/failed
      (let [[new-d events] result]
        [(assoc store var new-d) {var events}]))))

(defn solve-dfs
  "Depth-first search. Returns a fully-assigned store or nil."
  [propagators store subscriptions merged-decisions]
  (cond
    (= store ::domains/failed) nil
    (all-assigned? store merged-decisions) store
    :else
    (let [var (select-variable store merged-decisions)
          val (select-value store var)
          ;; Branch 1: var = val
          branch1 (branch-assign store var val)]
      (or (when-not (= branch1 ::domains/failed)
            (let [[store1 events1] branch1
                  queue1 (fixpoint/events->queue events1 subscriptions propagators)
                  store1 (fixpoint/propagate-fixpoint propagators store1 queue1 subscriptions)]
              (solve-dfs propagators store1 subscriptions merged-decisions)))
          ;; Branch 2: var != val
          (let [branch2 (branch-remove store var val)]
            (when-not (= branch2 ::domains/failed)
              (let [[store2 events2] branch2
                    queue2 (fixpoint/events->queue events2 subscriptions propagators)
                    store2 (fixpoint/propagate-fixpoint propagators store2 queue2 subscriptions)]
                (solve-dfs propagators store2 subscriptions merged-decisions))))))))

(defn solve-all-dfs
  "Find all solutions via DFS. Returns vector of fully-assigned stores."
  [propagators store subscriptions merged-decisions]
  (cond
    (= store ::domains/failed) []
    (all-assigned? store merged-decisions) [store]
    :else
    (let [var (select-variable store merged-decisions)
          val (select-value store var)
          ;; Branch 1: var = val
          branch1 (branch-assign store var val)
          sols1 (if (= branch1 ::domains/failed)
                  []
                  (let [[store1 events1] branch1
                        queue1 (fixpoint/events->queue events1 subscriptions propagators)
                        store1 (fixpoint/propagate-fixpoint propagators store1 queue1 subscriptions)]
                    (solve-all-dfs propagators store1 subscriptions merged-decisions)))
          ;; Branch 2: var != val
          branch2 (branch-remove store var val)
          sols2 (if (= branch2 ::domains/failed)
                  []
                  (let [[store2 events2] branch2
                        queue2 (fixpoint/events->queue events2 subscriptions propagators)
                        store2 (fixpoint/propagate-fixpoint propagators store2 queue2 subscriptions)]
                    (solve-all-dfs propagators store2 subscriptions merged-decisions)))]
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
