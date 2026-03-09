(ns routaverra.igor.terms.introduced
  (:refer-clojure :exclude [every?])
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.types :as types]
            [routaverra.igor.utils.string :refer [>>]]
            [routaverra.igor.api :as api]))

(defrecord TermEvery? [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr constraint-expr] argv]
                   (list
                    'every?
                    [bind-sym
                     (protocols/write set-expr)]
                    (clojure.walk/postwalk
                     (fn [e]
                       (if
                        (= e (protocols/write local-decision))
                         bind-sym
                         e))
                     (protocols/write constraint-expr)))))
  (codomain [self] {types/Bool self})
  (domainv [self] [{types/Numeric self} {types/Set self} {types/Bool self}])
  (decisions [self] (dissoc
                     (api/unify-argv-decisions self)
                     (first argv)))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>>
                     {:local-decision (protocols/translate (first argv))
                      :set-expr (protocols/translate (second argv))
                      :constraint-expr (protocols/translate (last argv))}
                     "( forall ( {{local-decision}} in {{set-expr}} )( {{constraint-expr}} ) )"))
  (evaluate [self solution]
    (let [[local-decision set-expr constraint-expr] argv
          s (api/eval-arg set-expr solution)]
      (clojure.core/every? (fn [elem]
                (api/eval-arg constraint-expr (assoc solution local-decision elem)))
              s))))

(defrecord TermImage [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr generator-expr] argv]
                   (list
                    'image
                    [bind-sym
                     (protocols/write set-expr)]
                    (clojure.walk/postwalk
                     (fn [e]
                       (if
                        (= e (protocols/write local-decision))
                         bind-sym
                         e))
                     (protocols/write generator-expr)))))
  (codomain [self] {types/Set self})
  (domainv [self] [{types/Numeric self} {types/Set self} {types/Numeric self}])
  (decisions [self] (dissoc
                     (api/unify-argv-decisions self)
                     (first argv)))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>>
                     {:local-decision (protocols/translate (first argv))
                      :set-expr (protocols/translate (second argv))
                      :generator-expr (protocols/translate (last argv))}
                     "{ {{generator-expr}} | {{local-decision}} in {{set-expr}} }"))
  (evaluate [self solution]
    (let [[local-decision set-expr generator-expr] argv
          s (api/eval-arg set-expr solution)]
      (into (sorted-set)
            (map (fn [elem]
                   (api/eval-arg generator-expr (assoc solution local-decision elem)))
                 s)))))

;; --- Constructor functions ---

(defn fresh
  ([] (fresh (str (gensym))))
  ([id] (api/->Decision id)))

(defn every? [set-expr constraint-fn]
  (let [local-decision (api/lexical (fresh))]
    (api/cacheing-validate
     (->TermEvery? (:id local-decision)
                   [local-decision set-expr (constraint-fn local-decision)]))))

(defn image [set-expr generator-fn]
  (let [local-decision (api/lexical (fresh))]
    (api/cacheing-validate
     (->TermImage (:id local-decision)
                  [local-decision set-expr (generator-fn local-decision)]))))
