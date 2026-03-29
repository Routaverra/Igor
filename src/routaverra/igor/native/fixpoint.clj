(ns routaverra.igor.native.fixpoint
  (:require [routaverra.igor.native.domains :as domains]))

(defn compute-events
  "Compare old and new domains for the given vars, return {Decision -> #{events}}."
  [old-store new-store vars]
  (persistent!
   (reduce
    (fn [acc v]
      (let [old-d (get old-store v)
            new-d (get new-store v)]
        (if (= old-d new-d)
          acc
          (let [events (cond-> #{}
                         (domains/assigned? new-d) (conj :assigned)
                         (or (not= (domains/domain-min old-d) (domains/domain-min new-d))
                             (not= (domains/domain-max old-d) (domains/domain-max new-d)))
                         (conj :bounds)
                         (not= (domains/domain-size old-d) (domains/domain-size new-d))
                         (conj :domain))]
            (if (seq events)
              (assoc! acc v events)
              acc)))))
    (transient {})
    vars)))

(defn build-subscriptions
  "Build reverse index: {Decision -> {event-type -> #{prop-indices}}}."
  [propagators]
  (reduce-kv
   (fn [subs idx prop]
     (reduce-kv
      (fn [subs decision events]
        (reduce
         (fn [subs evt]
           (update-in subs [decision evt] (fnil conj #{}) idx))
         subs
         events))
      subs
      (:events prop)))
   {}
   (vec propagators)))

(defn events->queue
  "Convert {Decision -> #{event-types}} into a priority queue of [priority prop-idx] entries."
  [events subscriptions propagators]
  (into (sorted-set)
        (for [[decision evt-set] events
              evt evt-set
              prop-idx (get-in subscriptions [decision evt] #{})]
          [(get-in propagators [prop-idx :priority] 0) prop-idx])))

(defn initial-queue
  "Enqueue all propagators for initial propagation."
  [propagators]
  (into (sorted-set)
        (map-indexed (fn [idx prop] [(:priority prop) idx]))
        propagators))

(defn propagate-fixpoint
  "Run propagators to fixpoint. Returns updated store or ::domains/failed."
  [propagators store queue subscriptions]
  (loop [store store
         queue queue]
    (if (empty? queue)
      store
      (let [entry (first queue)
            queue (disj queue entry)
            [_ prop-idx] entry
            prop (nth propagators prop-idx)
            old-store store
            result ((:propagate-fn prop) store)]
        (if (= result ::domains/failed)
          ::domains/failed
          (let [events (compute-events old-store result (:vars prop))
                affected (for [[decision evt-set] events
                              evt evt-set
                              pidx (get-in subscriptions [decision evt] #{})
                              :when (not= pidx prop-idx)]
                          [(:priority (nth propagators pidx)) pidx])
                queue (into queue affected)]
            (recur result queue)))))))
