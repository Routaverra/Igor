(ns igor.types)

(def Numeric ::numeric)
(def Set ::set)
(def Bool ::boolean)

(def all-decision-types
  #{Numeric
    Set
    Bool})

(defn domain->type [domain]
  (first (sort (keys domain))))
