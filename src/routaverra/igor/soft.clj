(ns routaverra.igor.soft
  "Soft constraints — constraints that can be violated at a cost.

   Returns a handle that IS the constraint (implements IExpress) and
   supports a violation cost extractor."
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.terms.core :as terms]
            [routaverra.igor.terms.introduced :as terms.introduced]))

(defrecord SoftConstraint [satisfied-var violation-var constraint-expr]
  protocols/IExpress
  (write [_self] (protocols/write constraint-expr))
  (codomain [_self] (protocols/codomain constraint-expr))
  (domainv [_self] (protocols/domainv constraint-expr))
  (decisions [_self] (api/cacheing-decisions constraint-expr))
  (bindings [_self] (protocols/bindings constraint-expr))
  (validate [self] (api/cacheing-validate constraint-expr) self)
  (translate [_self] (protocols/translate constraint-expr))
  (evaluate [_self solution] (protocols/evaluate constraint-expr solution)))

(defn soft
  "Soft constraint: wraps a constraint so it can be violated at a cost.
   Penalty can be a fixed number or a decision variable.

   Returns a handle that:
     - IS the constraint (pass directly to i/satisfy, i/and, etc.)
     - Supports (i/violation handle) to get the cost decision variable
       (0 when satisfied, penalty when violated)"
  [constraint penalty]
  (let [satisfied (api/impl (api/fresh-bool))
        cost-expr (terms/iff satisfied 0 penalty)
        ;; If satisfied, the original constraint must hold
        implication (terms.introduced/implies* satisfied constraint)
        ;; Violation = if satisfied then 0 else penalty
        ;; When penalty is ground, we can bound the violation domain
        ;; When penalty is a decision variable, leave violation unbound (solver infers range)
        violation (if (number? penalty)
                    (api/fresh-int (sorted-set 0 penalty))
                    (api/lexical (api/fresh)))
        cost-link (terms/equals violation cost-expr)
        combined (terms/and* implication cost-link)]
    (->SoftConstraint satisfied violation combined)))

(defn violation
  "Returns the violation cost decision variable from a soft constraint handle.
   Value is 0 when satisfied, penalty when violated."
  [handle]
  {:pre [(instance? SoftConstraint handle)]}
  (:violation-var handle))
