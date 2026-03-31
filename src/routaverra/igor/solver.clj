(ns routaverra.igor.solver
  (:require [routaverra.igor.api :as api]
            [routaverra.igor.flattener :as flattener]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.types :as types]
            [routaverra.igor.cache :as cache]
            [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [clojure.core.async :as async]
            [routaverra.igor.utils.log :as log]
            [routaverra.igor.adapter :as adapter]
            [routaverra.igor.utils.string :refer [>>]]))

(def ^:dynamic *debug* false)
(def ^:dynamic *validate?* false)

(defn ->output [decisions]
  (let [var-string (->> (for [decision (sort-by :id (-> decisions keys))]
                          (>> {:x (protocols/translate decision)}
                              "\\\"\\({{x}})\\\""))
                        (interpose " ")
                        (apply str))]
    (>> {:x var-string}
        "output([\"[{{x}}]\"]);")))

(defn collect-keywords
  "Collect all keyword values from decision bindings and constraint argv vectors.
   Only collects keywords that participate as domain values, not structural record keys."
  [constraint decisions bindings]
  (let [;; Keywords from decision binding ranges (e.g. fresh-keyword, fresh-set with keywords)
        from-bindings (->> decisions
                           keys
                           (keep #(api/binding-set (get bindings %)))
                           (mapcat identity)
                           (filter keyword?)
                           set)
        ;; Keywords used as literal values in constraint expressions
        from-tree (reduce (fn [acc n]
                            (cond
                              (and (satisfies? protocols/IExpress n)
                                   (instance? clojure.lang.IRecord n)
                                   (:argv n))
                              (into acc (filter keyword? (:argv n)))

                              (set? n)
                              (into acc (filter keyword? n))

                              :else acc))
                          #{}
                          (tree-seq coll? seq constraint))]
    (into from-bindings from-tree)))

(defn keyword-enum-declaration
  "Generate MiniZinc enum declaration for all keywords in the problem."
  [keywords]
  (when (seq keywords)
    (let [sorted-kws (sort keywords)
          mzn-names (map api/keyword->mzn-name sorted-kws)]
      (str "enum Keyword = {" (string/join ", " mzn-names) "};"))))

(defn keyword-reverse-lookup
  "Build a map from MiniZinc enum name string to Clojure keyword."
  [keywords]
  (into {} (map (fn [kw] [(api/keyword->mzn-name kw) kw]) keywords)))

(defn decisions->var-declarations [decisions bindings]
  (->> decisions
       (map (fn [[decision domain]]
              (let [set (api/binding-set (get bindings decision))
                    type (types/domain->type domain)
                    env {:range (some-> set protocols/translate)
                         :decision (protocols/translate decision)}
                    >>* (partial >> env)]
                (cond
                  (= type types/Set)
                  (if set
                    (>>* "var set of {{range}}: {{decision}};")
                    (throw (ex-info (str "unbound set decision: " (protocols/write decision)) {})))

                  (= type types/Numeric)
                  (if set
                    (>>* "var {{range}}: {{decision}};")
                    (if (or (api/impl-decision? decision)
                            (api/lexical-decision? decision))
                      (>>* "var int: {{decision}};")
                      (throw (ex-info
                              (str "unbound numeric decision: " (protocols/write decision)
                                   ". Use (i/bind domain decision) or (i/fresh-int domain) to specify bounds.")
                              {}))))

                  (= type types/Bool)
                  (>>* "var bool: {{decision}};")

                  (= type types/Keyword)
                  (if set
                    (>>* "var {{range}}: {{decision}};")
                    (>>* "var Keyword: {{decision}};"))))))
       sort))

(def ^:dynamic *keyword-lookup* nil)

(defmulti detranspile*
  (fn [decisions [decision _out-str]]
    (types/domain->type (get decisions decision))))

(defmethod detranspile* types/Numeric [_ [_ out-str]]
  (Integer/parseInt out-str))

(defmethod detranspile* types/Bool [_ [_ out-str]]
  (Boolean/parseBoolean out-str))

(defmethod detranspile* types/Set [_ [_ out-str]]
  (if (re-matches #"[0-9]*\.\.[0-9]*" out-str)
    (let [[lower upper] (->> (string/split out-str #"\.\.")
                             (map #(Integer/parseInt %)))]
      (apply sorted-set (range lower (+ 1 upper))))
    ;; Set could contain enum values — check and convert
    (let [elements (-> out-str
                       (string/replace #"[{}]" "")
                       string/trim)]
      (if (string/blank? elements)
        (sorted-set)
        (let [parts (map string/trim (string/split elements #","))]
          (if (every? #(re-matches #"-?[0-9]+" %) parts)
            (apply sorted-set (map #(Integer/parseInt %) parts))
            ;; enum values — look up keywords
            (into #{} (map #(get *keyword-lookup* %) parts))))))))

(defmethod detranspile* types/Keyword [_ [_ out-str]]
  (get *keyword-lookup* out-str))

(defn detranspile-full
  "Parse MiniZinc output into a full solution map including impl decisions."
  [decisions out-str]
  (->> (string/split out-str #"\n")
       first
       read-string
       (interleave (sort-by :id (-> decisions keys)))
       (partition 2)
       (map (partial detranspile* decisions))
       (zipmap (sort-by :id (-> decisions keys)))))

(defn filter-impl-decisions
  "Remove impl decisions from a solution map."
  [solution]
  (into {} (filter (comp (complement api/impl-decision?) key) solution)))

(defn detranspile [& [decisions out-str :as args]]
  (filter-impl-decisions (detranspile-full decisions out-str)))

(defn expand-all [node]
  (clojure.walk/prewalk
   (fn [n]
     (if
      (satisfies? protocols/IExpand n)
       (protocols/expand n)
       n))
   node))

(defn collect-includes [node]
  (reduce (fn [acc n]
            (if (satisfies? protocols/IInclude n)
              (into acc (protocols/mzn-includes n))
              acc))
          #{}
          (tree-seq coll? seq node)))

(def ^:dynamic *flatten?* true)

(defn- solve-mzn
  "Generate MiniZinc and invoke the solver. Returns adapter result map:
   sync → {:result <solution-map | nil | vec-of-maps>, :complete? bool}
   async → {:chan <core.async channel>, :complete? <promise of bool>}"
  [{:keys [all? async?] :as opts} constraint objective]
  (let [model-decisions (api/merge-with-key
                         api/intersect-domains
                         (api/cacheing-decisions constraint)
                         (when objective (api/cacheing-decisions objective)))
        constraint-with-forced-decisions-and-expanded-terms
        (clojure.walk/postwalk
         (fn [x]
           (cond
             (and (api/decision? x)
                  (not (api/lexical-decision? x)))
             (api/force-type x (types/domain->type (get model-decisions x)))

             (satisfies? protocols/IExpand x)
             (expand-all x)

             :else x))
         constraint)
        [obj & obj-consts] (when objective
                             (if *flatten?*
                               (flattener/conjuctive-flattening objective)
                               [objective]))
        directive-str (if objective
                        (>> {:e (protocols/translate obj)
                             :d (name (or (:direction opts) :maximize))}
                            "solve {{d}} {{e}};")
                        "solve satisfy;")
        constraints (if *flatten?*
                      (concat (flattener/conjuctive-flattening
                               constraint-with-forced-decisions-and-expanded-terms)
                              obj-consts)
                      [constraint-with-forced-decisions-and-expanded-terms])
        constraint-str (->> constraints
                            (map (fn [constraint] (>> {:e (protocols/translate constraint)}
                                                      "constraint {{e}};")))
                            (interpose "\n")
                            (apply str))
        merged-decisions (apply
                          api/merge-with-key
                          api/intersect-domains
                          model-decisions
                          (map api/cacheing-decisions constraints))
        merged-bindings (apply
                         api/merge-with-key
                         (partial api/intersect-bindings "ignore")
                         (concat (map protocols/bindings constraints)
                                 (when objective [(protocols/bindings objective)])))
        var-declarations-str (decisions->var-declarations
                              merged-decisions
                              merged-bindings)
        output-str (->output merged-decisions)
        includes (into (collect-includes constraint-with-forced-decisions-and-expanded-terms)
                       (when objective (collect-includes objective)))
        include-strs (mapv #(str "include \"" % "\";") (sort includes))
        all-keywords (collect-keywords constraint merged-decisions merged-bindings)
        enum-decl (keyword-enum-declaration all-keywords)
        kw-lookup (keyword-reverse-lookup all-keywords)
        mzn-parts (cond-> (into include-strs
                                (if enum-decl
                                  (into [enum-decl] var-declarations-str)
                                  var-declarations-str))
                    constraint-str (conj constraint-str)
                    :always (conj output-str directive-str))
        mzn (apply str (interpose "\n" mzn-parts))]
    (if *debug*
      (do (spit "scratch/mzn" mzn) mzn)
      (let [detranspile-fn (if *validate?*
                             (fn [out-str]
                               (binding [*keyword-lookup* kw-lookup]
                                 (let [full-solution (detranspile-full merged-decisions out-str)
                                       valid? (protocols/evaluate constraint full-solution)]
                                   (when-not valid?
                                     (throw (ex-info "Solution validation failed: MiniZinc solution does not satisfy the original constraint"
                                                     {:solution (filter-impl-decisions full-solution)})))
                                   (filter-impl-decisions full-solution))))
                             (fn [out-str]
                               (binding [*keyword-lookup* kw-lookup]
                                 (detranspile merged-decisions out-str))))]
        ((if async? adapter/call-async adapter/call-sync)
         all? mzn detranspile-fn :timeout-ms (:timeout-ms opts))))))

(defn- cache-hit-chan
  "Return a channel that emits the given solutions then closes."
  [solutions]
  (let [ch (async/chan)]
    (async/go
      (doseq [sol solutions]
        (async/>! ch sol))
      (async/close! ch))
    ch))

(defn solve [{:keys [all? async? direction] :as opts}
             constraint
             objective]
  {:pre [(some? constraint)
         (contains? (protocols/codomain constraint) types/Bool)
         (or (nil? objective) (contains? (protocols/codomain objective) types/Numeric))
         (or (nil? direction) (#{:maximize :minimize} direction))]}
  (let [decisions  (when-not *debug* (cache/decisions-for constraint objective))
        cached     (when decisions (cache/lookup-solutions constraint objective opts))
        solutions  (:solutions cached)
        complete?  (:complete? cached)]
    (cond
      ;; Non-all with cached solution: return one immediately
      (and (seq solutions) (not all?))
      (let [sol (cache/values->solution decisions (first solutions))]
        (if async?
          (cache-hit-chan [sol])
          sol))

      ;; All with complete cache: return full set, no re-solve needed
      (and (seq solutions) all? complete?)
      (let [sols (mapv #(cache/values->solution decisions %) solutions)]
        (if async?
          (cache-hit-chan sols)
          sols))

      ;; Need to invoke the solver
      :else
      (let [solver-result (solve-mzn opts constraint objective)]
        (if *debug*
          solver-result
          (if async?
            ;; Async: emit cached solutions first, then new solver results (deduped)
            (let [{solver-chan :chan solver-complete? :complete?} solver-result
                  out-ch (async/chan)]
              (async/go
                (when all?
                  (doseq [vals solutions]
                    (async/>! out-ch (cache/values->solution decisions vals))))
                (loop [seen (or solutions #{})
                       errored? false]
                  (if-let [v (async/<! solver-chan)]
                    (if (and (map? v) (contains? v :routaverra.igor.adapter/error))
                      (do (async/>! out-ch v)
                          (recur seen true))
                      (let [vals (cache/solution->values decisions v)]
                        (when-not (contains? seen vals)
                          (async/>! out-ch v))
                        (recur (conj seen vals) errored?)))
                    (do
                      (when-not errored?
                        (cache/add-solutions! constraint objective opts
                          seen @solver-complete?))
                      (async/close! out-ch)))))
              out-ch)
            ;; Sync
            (let [{solver-output :result mzn-complete? :complete?} solver-result]
              (if all?
                (let [new-vals (set (map #(cache/solution->values decisions %) solver-output))
                      all-entry (cache/add-solutions! constraint objective opts
                                  new-vals mzn-complete?)]
                  (mapv #(cache/values->solution decisions %) (:solutions all-entry)))
                (do
                  (when solver-output
                    (cache/add-solutions! constraint objective opts
                      #{(cache/solution->values decisions solver-output)} mzn-complete?))
                  solver-output)))))))))
