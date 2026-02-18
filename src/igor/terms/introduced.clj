(ns igor.terms.introduced
  (:require [igor.protocols :as protocols]
            [igor.types :as types]
            [igor.utils.string :refer [>>]]
            [igor.api :as api]))

(defrecord TermForAll [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr constraint-expr] argv]
                   (list
                    'forall
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
                     "( forall ( {{local-decision}} in {{set-expr}} )( {{constraint-expr}} ) )")))

(defrecord TermForSet [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr generator-expr] argv]
                   (list
                    'for-set
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
                     "{ {{generator-expr}} | {{local-decision}} in {{set-expr}} }")))

;; --- Constructor functions ---

(defn fresh
  ([] (fresh (str (gensym))))
  ([id] (api/->Decision id)))

(defn forall [set-expr constraint-fn]
  (let [local-decision (api/lexical (fresh))]
    (api/cacheing-validate
     (->TermForAll (:id local-decision)
                   [local-decision set-expr (constraint-fn local-decision)]))))

(defn for-set [set-expr generator-fn]
  (let [local-decision (api/lexical (fresh))]
    (api/cacheing-validate
     (->TermForSet (:id local-decision)
                   [local-decision set-expr (generator-fn local-decision)]))))
