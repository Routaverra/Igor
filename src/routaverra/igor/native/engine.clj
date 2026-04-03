(ns routaverra.igor.native.engine
  (:require [routaverra.igor.native.domains :as domains]
            [routaverra.igor.native.fixpoint :as fixpoint]
            [routaverra.igor.native.propagators :as propagators]
            [routaverra.igor.native.search :as search]
            [routaverra.igor.api :as api]
            [routaverra.igor.types :as types]
            [clojure.core.async :as async]))

;; ---- Store operations ----

(defn contiguous-range?
  "Returns true if the sorted-set forms a contiguous integer range."
  [s]
  (and (seq s)
       (every? integer? s)
       (= (count s) (inc (- (long (last (seq s))) (long (first s)))))))

(defn build-keyword-map
  "Build a bidirectional keyword<->integer mapping from the set of keywords."
  [all-keywords]
  (when (seq all-keywords)
    (let [sorted-kws (vec (sort all-keywords))]
      {:kw->int (into {} (map-indexed (fn [i kw] [kw i]) sorted-kws))
       :int->kw (into {} (map-indexed (fn [i kw] [i kw]) sorted-kws))})))

(defn make-store
  "Build the initial store from merged decisions and bindings.
   Returns {Decision -> domain}."
  [merged-decisions merged-bindings kw-map]
  (reduce-kv
   (fn [store decision domain-map]
     (let [type (types/domain->type domain-map)
           binding (get merged-bindings decision)
           binding-set (api/binding-set binding)]
       (assoc store decision
              (cond
                (= type types/Bool)
                (domains/->IntervalDomain 0 1)

                (= type types/Keyword)
                (if (and binding-set kw-map)
                  (let [kw->int (:kw->int kw-map)
                        int-vals (into (sorted-set) (map kw->int) binding-set)]
                    (domains/->EnumeratedDomain int-vals))
                  (let [n (count (:kw->int kw-map))]
                    (domains/->IntervalDomain 0 (dec n))))

                (= type types/Set)
                (if binding-set
                  ;; GLB starts empty, LUB starts with all possible elements
                  (let [elements (into (sorted-set) (filter integer?) binding-set)]
                    (domains/->SetDomain (sorted-set) elements))
                  ;; Unbound set (introduced by set flattening) — start with empty GLB
                  ;; LUB will be narrowed by propagation; use a placeholder that gets
                  ;; replaced after the full store is built
                  (if (api/impl-decision? decision)
                    (domains/->SetDomain (sorted-set) (sorted-set))
                    (throw (ex-info (str "Unbound set decision: " (:id decision))
                                   {:decision decision}))))

                (= type types/Numeric)
                (if binding-set
                  (let [nums (into (sorted-set) (filter integer?) binding-set)]
                    (if (contiguous-range? nums)
                      (domains/->IntervalDomain (first nums) (last (seq nums)))
                      (domains/->EnumeratedDomain nums)))
                  (if (or (api/impl-decision? decision) (api/lexical-decision? decision))
                    (domains/->IntervalDomain -10000000 10000000)
                    (throw (ex-info (str "Unbound numeric decision: " (:id decision))
                                   {:decision decision}))))))))
   {}
   merged-decisions))

;; ---- Solution extraction ----

(defn extract-solution
  "Extract a {Decision -> value} solution from a fully-assigned store."
  [store merged-decisions kw-map]
  (let [int->kw (when kw-map (:int->kw kw-map))]
    (reduce-kv
     (fn [sol decision domain-map]
       (if (api/impl-decision? decision)
         sol
         (let [type (types/domain->type domain-map)
               d (get store decision)]
           (assoc sol decision
                  (case type
                    ::types/numeric (domains/domain-min d)
                    ::types/boolean (not= 0 (domains/domain-min d))
                    ::types/keyword (get int->kw (domains/domain-min d))
                    ::types/set (domains/set-assigned-value d)
                    (domains/domain-min d))))))
     {}
     merged-decisions)))

;; ---- Entry point ----

(defn solve-native-sync
  "Synchronous native solve. Returns solution(s) depending on opts."
  [{:keys [all?] :as opts}
   {:keys [constraints merged-decisions merged-bindings
           objective-var direction all-keywords] :as model}]
  (let [kw-map (build-keyword-map all-keywords)
        store (make-store merged-decisions merged-bindings kw-map)
        ;; Introduced set decisions start with empty LUBs. Give them the
        ;; union of all bound set variables' LUBs so propagation can narrow.
        universal-lub (reduce-kv (fn [acc _ d]
                                   (if (and (instance? routaverra.igor.native.domains.SetDomain d)
                                            (seq (:lub d)))
                                     (into acc (:lub d))
                                     acc))
                                 (sorted-set) store)
        store (reduce-kv (fn [s dec d]
                           (if (and (instance? routaverra.igor.native.domains.SetDomain d)
                                    (empty? (:lub d)))
                             (assoc s dec (domains/->SetDomain (sorted-set) universal-lub))
                             s))
                         store store)
        propagators (vec (mapcat #(propagators/compile-constraint % kw-map) constraints))
        subscriptions (fixpoint/build-subscriptions propagators)
        store (fixpoint/propagate-fixpoint propagators store (fixpoint/initial-queue propagators) subscriptions)]
    (when-not (= store ::domains/failed)
      (cond
        all?
        (let [solutions (search/solve-all-dfs propagators store subscriptions merged-decisions)]
          (mapv #(extract-solution % merged-decisions kw-map) solutions))

        (and objective-var direction)
        (when-let [solution (search/solve-optimize propagators store subscriptions
                                                   objective-var direction merged-decisions)]
          (extract-solution solution merged-decisions kw-map))

        :else
        (when-let [solution (search/solve-dfs propagators store subscriptions merged-decisions)]
          (extract-solution solution merged-decisions kw-map))))))

(defn solve-native
  "Native constraint solver entry point. Handles sync and async modes."
  [{:keys [async? all?] :as opts} model]
  (if async?
    (let [ch (async/chan (if all? 100 1))]
      (future
        (try
          (let [result (solve-native-sync opts model)]
            (if all?
              (doseq [sol result]
                (async/>!! ch sol))
              (when result
                (async/>!! ch result))))
          (catch Exception _))
        (async/close! ch))
      ch)
    (solve-native-sync opts model)))
