(ns routaverra.igor.flattener
  (:require [routaverra.igor.terms.core :as terms.core]
            [routaverra.igor.terms.utils :as terms.utils]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.types :as types]))

(defn simple-term? [node]
  (not-any? terms.utils/decendents (terms.utils/decendents node)))

(defn replace-children-recursive [subs node]
  (clojure.walk/postwalk
   (fn [x]
     (if (coll? x)
       (get subs x x)
       x))
   node))

(defn update-sub-map [substitutions node]
  (let [preserve? (or
                   (api/decision? node)
                   ;; bare Decisions are leaf values — substituting them causes
                   ;; postwalk key mismatches when the same Decision appears in
                   ;; multiple sibling terms

                   (nil? (api/cacheing-decisions node))
                   ;; ground values (no decision variables) are constants —
                   ;; substituting them wastes an introduced variable

                   (some api/lexical-decision? (keys (api/cacheing-decisions node)))

                   (contains? substitutions node)
                   ;; an identical expression has been substituted elsewhere in the tree
                   ;; allowing the existing substitution to remain is a form of common subexpression elimination

                   (satisfies? protocols/IInclude node)
                   ;; global constraints (regular, cost_regular, table, graph constraints)
                   ;; must not be reified — keep them inline in conjunctions
                   )]
    (if preserve?
      substitutions
      (let [type (types/domain->type
                  (protocols/codomain node))]
        (assoc
         substitutions
         node
         (-> (api/->Decision (str "introduced" (gensym)))
             (api/force-type type)
             (api/impl)))))))

(defn post-order-traversal [root? substitutions node]
  (if (simple-term? node)
    (if root?
      [node {}]
      (update-sub-map
       substitutions
       node))
    (let [substitutions' (reduce
                          (partial post-order-traversal false)
                          substitutions
                          (terms.utils/decendents
                           node))
          node' (replace-children-recursive
                 substitutions'
                 node)]
      (if root?
        [node' substitutions']
        (update-sub-map
         substitutions'
         node')))))

(defn conjuctive-flattening [node]
  (let [[root subs] (post-order-traversal true {} node)]
    (conj
     (for [[k v] subs]
       (terms.core/->TermEquals [k v]))
     root)))
