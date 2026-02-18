(ns igor.solver
  (:require [igor.api :as api]
            [igor.flattener :as flattener]
            [igor.protocols :as protocols]
            [igor.types :as types]
            [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [igor.utils.log :as log]
            [igor.adapter :as adapter]
            [igor.utils.string :refer [>>]]))

(def ^:dynamic *debug* false)

(defn ->output [decisions]
  (let [var-string (->> (for [decision (sort-by :id (-> decisions keys))]
                          (>> {:x (protocols/translate decision)}
                              "\\\"\\({{x}})\\\""))
                        (interpose " ")
                        (apply str))]
    (>> {:x var-string}
        "output([\"[{{x}}]\"]);")))

(defn decisions->var-declarations [decisions bindings]
  (->> decisions
       (map (fn [[decision domain]]
              (let [set (api/binding-set (get bindings decision))
                    type (types/domain->type domain)
                    _ (when (and (= type types/Set) (nil? set))
                        (throw (ex-info (str "unbound set decision: " (protocols/write decision)) {})))
                    env {:range (some-> set protocols/translate)
                         :decision (protocols/translate decision)}
                    >>* (partial >> env)]
                (cond
                  (= type types/Set) (>>* "var set of {{range}}: {{decision}};")
                  (= type types/Numeric) (>>* "var int: {{decision}};")
                  (= type types/Bool) (>>* "var bool: {{decision}};")))))
       sort))

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
    (read-string (str "#" out-str))))

(defn detranspile [& [decisions out-str :as args]]
  (->> (string/split out-str #"\n")
       first
       read-string
       (interleave (sort-by :id (-> decisions keys)))
       (partition 2)
       (map (partial detranspile* decisions))
       (zipmap (sort-by :id (-> decisions keys)))
       (filter (comp (complement api/impl-decision?) key))
       (into {})))

(defn expand-all [node]
  (clojure.walk/prewalk
   (fn [n]
     (if
      (satisfies? protocols/IExpand n)
       (protocols/expand n)
       n))
   node))

(def ^:dynamic *flatten?* true)

(defn solve [{:keys [all? async?] :as opts}
             constraint
             objective]
  {:pre [(some? constraint)
         (contains? (protocols/codomain constraint) types/Bool)
         (or (nil? objective) (contains? (protocols/codomain objective) types/Numeric))]}
  (let [model-decisions (api/merge-with-key
                         api/intersect-domains
                         (api/cacheing-decisions constraint)
                         (when objective (api/cacheing-decisions objective)))
        constraint-with-forced-decisions-and-expanded-terms (clojure.walk/postwalk
                                                             (fn [x]
                                                               (cond
                                                                 (and (api/decision? x)
                                                                      (not (api/lexical-decision? x)))
                                                                 (api/force-type
                                                                  x
                                                                  (types/domain->type
                                                                   (get model-decisions x)))

                                                                 (satisfies? protocols/IExpand x)
                                                                 (expand-all x)

                                                                 :else x))
                                                             constraint)
        [obj & obj-consts] (when objective
                             (if *flatten?*
                               (flattener/conjuctive-flattening objective)
                               [objective]))
        directive-str (if objective
                        (>> {:e (protocols/translate obj)}
                            "solve maximize {{e}};")
                        "solve satisfy;")
        constraints (if *flatten?*
                      (concat (flattener/conjuctive-flattening constraint-with-forced-decisions-and-expanded-terms)
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
        var-declarations-str (decisions->var-declarations
                              merged-decisions
                              (apply
                               api/merge-with-key
                               (partial api/intersect-bindings "ignore")
                               (concat (map protocols/bindings constraints)
                                       (when objective [(protocols/bindings objective)]))))
        output-str (->output merged-decisions)
        mzn (apply str (interpose "\n" (cond-> var-declarations-str
                                               constraint-str (conj constraint-str)
                                               :always (conj output-str directive-str))))]
    (if *debug*
      (do (spit "scratch/mzn" mzn) mzn)
      ((if async?
         adapter/call-async
         adapter/call-sync)
       all?
       mzn
       (partial detranspile merged-decisions)))))
