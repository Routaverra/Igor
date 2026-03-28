(ns routaverra.igor.api
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.set]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.types :as types]
            [routaverra.igor.utils.string :refer [>>]]))

(declare cacheing-validate)
(declare cacheing-decisions)
(declare eval-arg)

(defn type-error! [expression v1 v2]
  (let [[type1 exp1] (first v1)
        [type2 exp2] (first v2)
        jared (fn [e type]
                (str
                 (if (= e expression)
                   "is of type "
                   "is used as ")
                 type
                 (when-not (= e expression)
                   (str " in expression " (protocols/write e)))))]
    (throw (ex-info
            (str "inconsistent model: expression "
                 (protocols/write expression)
                 " "
                 (jared exp1 type1)
                 " but "
                 (jared exp2 type2))
            {}))))

(defn map-intersection [m1 m2]
  (select-keys m2 (keys m1)))

(defn intersect-domains
  "given an expression and two domains,
   return the intersection of those domains
   or throw if their intersection is empty."
  [expression domain1 domain2]
  {:pre [(and (spec/valid? ::domain domain1)
              (spec/valid? ::domain domain2))]}
  (if-let [intersection (not-empty (map-intersection domain1 domain2))]
    intersection
    (type-error! expression domain1 domain2)))

(defn binding-set [x]
  {:pre [(s/valid? (spec/nilable ::binding) x)]}
  (first x))

(defn binding-source [x]
  {:pre [(s/valid? (spec/nilable ::binding) x)]}
  (second x))

(defn binding-error! [decision binding1 binding2]
  (throw (ex-info
          (str "inconsistent model:  "
               (protocols/write decision)
               " is bound to non-intersecting sets "
               (protocols/write (binding-set binding1))
               " via "
               (protocols/write (binding-source binding1))
               " and "
               (protocols/write (binding-set binding2))
               " via "
               (protocols/write (binding-source binding2)))
          {})))

(defn forced-typed [decision]
  (::type (meta decision)))

(defrecord Decision [id]
  protocols/IExpress
  (protocols/write [self] (list 'fresh (:id self)))
  (codomain [self] (if-let [type (forced-typed self)]
                     {type ::impl}
                     (zipmap types/all-decision-types (repeat self))))
  (decisions [self] {self (if-let [type (forced-typed self)]
                           {type self}
                           (zipmap types/all-decision-types (repeat self)))})
  (bindings [self] (when-let [r (::range (meta self))]
                     {self [r self]}))
  (validate [self] self)
  (translate [self] (str (:id self)))
  (evaluate [self solution]
    (if (contains? solution self)
      (get solution self)
      (throw (ex-info (str "Decision " (:id self) " not found in solution") {:decision self})))))

(def decision? (partial instance? Decision))

(defn lexical [decision]
  (with-meta decision (merge (meta decision) {::lexical true})))

(defn lexical-decision? [decision]
  (contains? (meta decision) ::lexical))

(defn impl [decision]
  (with-meta decision (merge (meta decision) {::impl true})))

(defn impl-decision? [decision]
  (contains? (meta decision) ::impl))

(defn force-type [decision type]
  {:pre [(decision? decision)
         (types/all-decision-types type)]}
  (with-meta
    decision
    (merge
     (meta decision)
     {::type type})))

(defn bind [super decision]
  "Constrains a decision, presumably a set decision, to be a subset of super.
   Referentially transparent - evaluates to the decision.
   No-op when used with decisions of other types."
  {:pre [(decision? decision)]}
  (with-meta
    decision
    (merge
     (meta decision)
     {::range (apply sorted-set super)})))

(defn intersect-bindings
  "given a decision and two bindings,
   return the intersection of those bindings
   or throw if their intersection is empty."
  [expression decision binding1 binding2]
  {:pre [(and (spec/valid? ::binding binding1)
              (spec/valid? ::binding binding2))]
   :post [(spec/valid? ::binding %)]}
  (if-let [intersection (not-empty
                         (clojure.set/intersection
                          (first binding1)
                          (first binding2)))]
    [intersection expression]
    (binding-error! decision binding1 binding2)))

(defn merge-with-key
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f key val-in-result val-in-latter)."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f k (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(extend-protocol protocols/IExpress
  clojure.lang.IPersistentSet
  (write [self] (set (map protocols/write self)))
  (codomain [self] {types/Set self})
  (decisions [self] (->> self
                         (map cacheing-decisions)
                         (apply merge-with-key intersect-domains)))
  (bindings [self] (->> self
                        (map protocols/bindings)
                        (apply merge-with-key (partial intersect-bindings self))))
  (validate [self] (doall (map cacheing-validate self)) self)
  (translate [self] (>> {:elements
                         (apply str (interpose "," (map protocols/translate self)))}
                        "{{{elements}}}"))
  (evaluate [self solution]
    (into (sorted-set) (map #(protocols/evaluate % solution) self))))

(spec/def ::domain (spec/map-of types/all-decision-types #(some? (protocols/write %))))
(spec/def ::domainv (spec/coll-of ::domain))
(spec/def ::decisions (spec/nilable (spec/map-of decision? ::domain)))
(spec/def ::binding (spec/tuple (every-pred set? sorted?) #(some? (protocols/write %))))
(spec/def ::bindings (spec/nilable (spec/map-of decision? ::binding)))

(extend-protocol protocols/IExpress
  Number
  (write [self] self)
  (codomain [self] {types/Numeric self})
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (str self))
  (evaluate [self _solution] self))

(extend-protocol protocols/IExpress
  Boolean
  (write [self] self)
  (codomain [self] {types/Bool self})
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (str self))
  (evaluate [self _solution] self))

(defn keyword->mzn-name
  "Convert a Clojure keyword to a valid MiniZinc enum identifier.
   :red -> kw_red, :my-color -> kw_my_color, :ns/foo -> kw_ns__foo"
  [kw]
  (let [ns-part (namespace kw)
        name-part (name kw)
        sanitize #(string/replace % "-" "_")]
    (if ns-part
      (str "kw_" (sanitize ns-part) "__" (sanitize name-part))
      (str "kw_" (sanitize name-part)))))

(extend-protocol protocols/IExpress
  clojure.lang.Keyword
  (write [self] self)
  (codomain [self] {types/Keyword self})
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (keyword->mzn-name self))
  (evaluate [self _solution] self))

(extend-protocol protocols/IExpress
  Object
  (write [self] self)
  (codomain [self] (throw (ex-info (str "unsupported codomain for type " (type self)) {:self self})))
  (decisions [_self] nil)
  (bindings [_self] nil)
  (validate [self] self)
  (translate [self] (throw (ex-info (str "unsupported translation for type" (type self)) {:self self})))
  (evaluate [self _solution] self))

(extend-protocol protocols/IExpress
  nil
  (write [self] self)
  (decisions [self] self)
  (bindings [self] self)
  (validate [self] self)
  (codomain [self] self)
  (translate [self] (throw (ex-info "unsupported type" {:self self})))
  (evaluate [self _solution] self))

(defn eval-arg
  "Evaluate x against a solution map. If x satisfies IExpress, recurse;
   otherwise return as-is (ground literal)."
  [x solution]
  (if (satisfies? protocols/IExpress x)
    (protocols/evaluate x solution)
    x))

(defn eval-argv
  "Evaluate all elements in (:argv term) against a solution map."
  [term solution]
  (mapv #(eval-arg % solution) (:argv term)))

(defn unify-argv-decisions [expression]
  {:post [(spec/valid? ::decisions %)]}
  (->> (:argv expression)
       (map (fn [domain arg]
              (if (decision? arg)
                (merge-with-key
                 intersect-domains
                 (cacheing-decisions arg)
                 {arg domain})
                (cacheing-decisions arg)))
            (protocols/domainv expression))
       (apply merge-with-key intersect-domains)))

(defn validate-domains [expression]
  (doall
   (->> (:argv expression)
        (map cacheing-validate)
        (map
         (fn [domain arg]
           (intersect-domains arg (protocols/codomain arg) domain))
         (protocols/domainv expression))))
  (cacheing-decisions expression)
  expression)

(defn translate-binary-operation [op-string left right]
  (>> {:left left :right right :op-string op-string}
      "({{left}} {{op-string}} {{right}})"))

(defn translate-nary-operation [op-string args]
  (reduce (partial translate-binary-operation op-string) args))

(defn unify-argv-bindings [e]
  (->> (:argv e)
       (map protocols/bindings)
       (apply merge-with-key (partial intersect-bindings e))))

(def cacheing-validate (memoize protocols/validate))
(def cacheing-decisions (memoize protocols/decisions))

;; --- Shared decision variable constructors ---

(defn fresh
  "Mint a fresh decision variable."
  []
  (->Decision (str (gensym))))

(defn fresh-bool []
  (force-type (fresh) types/Bool))

(defn fresh-int
  [domain]
  (force-type (bind domain (fresh)) types/Numeric))

(defn fresh-set [super]
  (when (and (seq super) (not (or (every? number? super) (every? keyword? super))))
    (throw (ex-info "set domain must be homogeneous — all integers or all keywords" {:domain super})))
  (force-type (bind super (fresh)) types/Set))

(defn fresh-keyword [domain]
  (when-not (every? keyword? domain)
    (throw (ex-info "keyword domain must contain only keywords" {:domain domain})))
  (force-type (bind domain (fresh)) types/Keyword))
