(ns routaverra.igor.cache
  "Disk cache for solver results. Keyed on a canonical representation of the
   constraint tree where gensym-based Decision IDs are replaced with
   depth-first encounter indices, making the key stable across REPL
   re-evaluations and JVM restarts.

   The cache stores a *set* of known solutions per canonical model, plus a
   :complete? flag indicating whether the solver proved completion (all
   solutions found / optimality proved). Every solve call adds to this set
   (union semantics), so partial results from timeouts accumulate across
   invocations."
  (:require [routaverra.igor.api :as api]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

(def ^:dynamic *enabled* true)
(def ^:dynamic *cache-dir* ".igor/cache")

(defn encounter-ordered-decisions
  "Walks expression trees depth-first, returns decisions in order of
   first encounter. Each decision appears exactly once."
  [& exprs]
  (let [seen (volatile! #{})
        result (volatile! [])]
    (walk/prewalk
     (fn [node]
       (when (and (api/decision? node)
                  (not (contains? @seen (:id node))))
         (vswap! seen conj (:id node))
         (vswap! result conj node))
       node)
     (vec exprs))
    @result))

(defn canonical-form
  "Produces a canonical representation of constraint expressions where
   Decision variables are replaced with [::var idx domain type] vectors
   based on depth-first encounter order. Independent of gensym IDs."
  [& exprs]
  (let [id->idx (volatile! {})
        counter (volatile! -1)]
    (walk/prewalk
     (fn [node]
       (if (api/decision? node)
         (let [idx (or (get @id->idx (:id node))
                       (let [c (vswap! counter clojure.core/inc)]
                         (vswap! id->idx assoc (:id node) c)
                         c))]
           [::var idx
            (some-> (:routaverra.igor.api/range (meta node)) sort vec)
            (:routaverra.igor.api/type (meta node))])
         node))
     (vec exprs))))

(defn cache-key
  "Returns a hex string hash for a canonical model.
   The key captures the constraint structure, objective, and solve direction,
   but NOT all? or async? — those are execution modes, not model identity."
  [constraint objective opts]
  (let [form (if objective
               (canonical-form constraint objective)
               (canonical-form constraint))
        key-data [form (:direction opts)]]
    (format "%016x" (hash key-data))))

(defn- cache-file ^java.io.File [key]
  (io/file *cache-dir* (str key ".edn")))

(defn clear!
  "Delete all cached solutions. Safe to call at any time."
  []
  (let [dir (io/file *cache-dir*)]
    (when (.exists dir)
      (doseq [f (.listFiles dir)]
        (.delete f)))))

(defn cache-read
  "Returns cached entry {:solutions #{...} :complete? bool} or nil."
  [key]
  (let [f (cache-file key)]
    (when (.exists f)
      (read-string (slurp f)))))

(defn cache-write [key entry]
  (let [f (cache-file key)]
    (io/make-parents f)
    (spit f (pr-str entry))))

(defn decisions-for [constraint objective]
  (apply encounter-ordered-decisions
         (if objective [constraint objective] [constraint])))

(defn solution->values
  "Convert a solution map to a value-vector in encounter order."
  [decisions solution]
  (mapv #(get solution %) decisions))

(defn values->solution
  "Convert a value-vector to a solution map keyed by current decisions."
  [decisions values]
  (zipmap decisions values))

(defn lookup-solutions
  "Returns cached entry {:solutions #{...} :complete? bool}, or nil if empty/missing."
  [constraint objective opts]
  (when *enabled*
    (let [key (cache-key constraint objective opts)
          cached (cache-read key)]
      (when (and cached (seq (:solutions cached)))
        cached))))

(defn add-solutions!
  "Merge new value-vectors into the cached solution set.
   complete? is OR'd: once true, stays true.
   Returns the merged entry {:solutions #{...} :complete? bool}."
  [constraint objective opts new-value-vectors complete?]
  (if *enabled*
    (let [key (cache-key constraint objective opts)
          existing (or (cache-read key) {:solutions #{} :complete? false})
          merged {:solutions (into (:solutions existing) new-value-vectors)
                  :complete? (or (:complete? existing) complete?)}]
      (when (not= existing merged)
        (cache-write key merged))
      merged)
    {:solutions (set new-value-vectors) :complete? complete?}))
