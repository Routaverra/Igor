(ns igor.notation
  (:require [igor.protocols :as protocols]
            [igor.api :as api]
            [igor.types :as types]
            [clojure.string :as str]
            [clojure.walk :as walk]))

;; ============================================================
;; TermAs — user-facing abstraction boundary
;; ============================================================

(defrecord TermAs [name argv]
  protocols/IInclude
  (mzn-includes [_self] #{})

  protocols/IExpress
  (write [_self] (list 'as name (protocols/write (first argv))))
  (codomain [_self] (protocols/codomain (first argv)))
  (domainv [_self] (protocols/domainv (first argv)))
  (decisions [_self] (api/cacheing-decisions (first argv)))
  (bindings [_self] (protocols/bindings (first argv)))
  (validate [_self] (api/cacheing-validate (first argv)))
  (translate [_self] (protocols/translate (first argv)))

  (evaluate [_self solution] (api/eval-arg (first argv) solution)))

(defn as
  "Wrap an expression with a name for notation rendering.
   The name is a keyword, e.g. :C_step.
   Does not affect solving — purely a notation concern."
  [name expr]
  (->TermAs name [expr]))

;; ============================================================
;; Precedence table
;; ============================================================

(def ^:private precedence-map
  "Map from record type to precedence level. Lower = binds looser."
  {igor.terms.core.TermIf      0
   igor.terms.core.TermCond     0
   igor.terms.core.TermWhen     1
   igor.terms.core.TermOr       2
   igor.terms.core.TermAnd      3
   igor.terms.core.TermNot      4
   igor.terms.core.TermEquals   5
   igor.terms.core.TermGreaterThan 5
   igor.terms.core.TermLessThan    5
   igor.terms.core.TermGreaterThanOrEqualTo 5
   igor.terms.core.TermLessThanOrEqualTo    5
   igor.terms.core.TermContains 5
   igor.terms.core.TermPlus     6
   igor.terms.core.TermMinus    6
   igor.terms.core.TermProduct  7
   igor.terms.core.TermDivide   7
   igor.terms.core.TermMod      7
   igor.terms.core.TermRem      7
   igor.terms.core.TermPow      8
   igor.terms.core.TermAbs      8
   igor.terms.core.TermInc      8
   igor.terms.core.TermDec      8})

(defn- precedence [node]
  (get precedence-map (type node) 9))

(defn- needs-parens?
  "Does child need parens when rendered inside parent?"
  [parent child]
  (let [p-prec (precedence parent)
        c-prec (precedence child)]
    (< c-prec p-prec)))

(defn- wrap-parens [s]
  (str "(" s ")"))

;; ============================================================
;; Render multimethod — dispatches on [type format]
;; ============================================================

(defmulti render-node
  "Render an AST node in a given format.
   Dispatch: [(type node) format-keyword].
   The env map carries {:format, :definitions (set of TermAs names)}."
  (fn [node env] [(type node) (:format env)]))

;; Forward declaration for recursive rendering
(declare render-subtree)

(defn- render-child
  "Render a child node, adding parens if needed for precedence."
  [parent child env]
  (let [rendered (render-subtree child env)]
    (if (and (satisfies? protocols/IExpress child)
             (not (instance? TermAs child))
             (needs-parens? parent child))
      (case (:format env)
        :latex (str "\\left(" rendered "\\right)")
        :unicode (wrap-parens rendered))
      rendered)))

(defn- render-children
  "Render all children of an n-ary node with separator."
  [parent sep env]
  (str/join sep (map #(render-child parent % env) (:argv parent))))

;; ============================================================
;; render-subtree — the recursive renderer
;; ============================================================

(defn render-subtree
  "Recursively render an AST node. TermAs nodes that appear in
   the definitions set are replaced with their name reference."
  [node env]
  (if (and (instance? TermAs node)
           (contains? (:definitions env) (:name node)))
    ;; Emit name reference
    (render-node node env)
    ;; Dispatch to multimethod
    (render-node node env)))

;; ============================================================
;; Name formatting helpers
;; ============================================================

(defn- keyword->name
  "Convert a keyword like :C_step to a display name.
   Underscores become subscript separators."
  [kw]
  (clojure.core/name kw))

(defn- latex-name
  "Format a keyword name for LaTeX. Trailing digits become subscripts.
   Underscores after the first segment become subscript text."
  [kw]
  (let [s (keyword->name kw)
        parts (str/split s #"_" 2)]
    (if (= 1 (count parts))
      ;; No underscore — check for trailing digits
      (if-let [[_ base digits] (re-matches #"(.+?)(\d+)$" s)]
        (str base "_{" digits "}")
        s)
      ;; Has underscore — first part is base, rest is subscript
      (let [base (first parts)
            sub (second parts)]
        (str base "_{\\text{" sub "}}")))))

(defn- unicode-name
  "Format a keyword name for Unicode. Trailing digits become subscript chars."
  [kw]
  (let [s (keyword->name kw)
        sub-digits {\0 \u2080 \1 \u2081 \2 \u2082 \3 \u2083 \4 \u2084
                     \5 \u2085 \6 \u2086 \7 \u2087 \8 \u2088 \9 \u2089}]
    (if-let [[_ base digits] (re-matches #"(.+?)(\d+)$" s)]
      (str base (apply str (map sub-digits digits)))
      s)))

;; ============================================================
;; Decision variable name formatting
;; ============================================================

(defn- latex-var-name
  "Format a display name for LaTeX. Trailing digits become subscripts."
  [s]
  (if-let [[_ base digits] (re-matches #"(.+?)(\d+)$" s)]
    (str base "_{" digits "}")
    s))

(defn- unicode-var-name
  "Format a display name for Unicode. Trailing digits become subscript chars."
  [s]
  (let [sub-digits {\0 \u2080 \1 \u2081 \2 \u2082 \3 \u2083 \4 \u2084
                     \5 \u2085 \6 \u2086 \7 \u2087 \8 \u2088 \9 \u2089}]
    (if-let [[_ base digits] (re-matches #"(.+?)(\d+)$" s)]
      (str base (apply str (map sub-digits digits)))
      s)))

;; ============================================================
;; Auto-naming: collect decisions and assign clean names
;; ============================================================

(def ^:private iterator-names
  "Conventional iterator variable names for quantifier-bound decisions."
  ["i" "j" "k" "l" "m" "n"])

(defn- collect-decisions
  "Walk an AST, collecting all unique non-lexical Decision nodes
   in the order they are first encountered."
  [root]
  (let [seen (atom #{})
        result (atom [])]
    (walk/prewalk
     (fn [node]
       (when (and (instance? igor.api.Decision node)
                  (not (api/lexical-decision? node))
                  (not (contains? @seen node)))
         (swap! seen conj node)
         (swap! result conj node))
       node)
     root)
    @result))

(defn- collect-quantifier-decisions
  "Walk an AST, collecting all lexical (quantifier-bound) Decision nodes
   in the order they are first encountered."
  [root]
  (let [seen (atom #{})
        result (atom [])]
    (walk/prewalk
     (fn [node]
       (when (and (instance? igor.api.Decision node)
                  (api/lexical-decision? node)
                  (not (contains? @seen node)))
         (swap! seen conj node)
         (swap! result conj node))
       node)
     root)
    @result))

(defn- assign-display-names
  "Assign clean display names to all decisions in an AST.
   Problem variables get x_1, x_2, ...
   Quantifier-bound variables get i, j, k, l, m, n, i_2, j_2, ..."
  [root]
  (let [problem-vars (collect-decisions root)
        quant-vars (collect-quantifier-decisions root)
        problem-names (into {}
                            (map-indexed
                             (fn [idx d]
                               [d (str "x" (inc idx))])
                             problem-vars))
        quant-names (into {}
                          (map-indexed
                           (fn [idx d]
                             (let [base-idx (mod idx (count iterator-names))
                                   cycle (quot idx (count iterator-names))
                                   base (nth iterator-names base-idx)]
                               [d (if (zero? cycle)
                                    base
                                    (str base (inc cycle)))]))
                           quant-vars))]
    (merge problem-names quant-names)))

;; ============================================================
;; Domain declarations
;; ============================================================

(defn- compact-range
  "Format a sorted set of integers as a compact range string.
   If contiguous, renders as {min, ..., max}.
   Otherwise renders the full set (for small sets) or {min, ..., max} anyway."
  [sorted-set format-kw]
  (let [elems (sort sorted-set)
        n (count elems)
        lo (first elems)
        hi (last elems)
        contiguous? (= n (inc (- hi lo)))]
    (if (and contiguous? (> n 5))
      (case format-kw
        :latex (str "\\{" lo ", \\ldots, " hi "\\}")
        :unicode (str "{" lo ", \u2026, " hi "}"))
      ;; Small or non-contiguous: enumerate
      (case format-kw
        :latex (str "\\{" (str/join ", " elems) "\\}")
        :unicode (str "{" (str/join ", " elems) "}")))))

(defn- render-domain-declaration
  "Render a single variable's domain declaration.
   For int vars: x ∈ {0, ..., 9}
   For set vars: A ⊆ {0, ..., 11}
   For bool vars: b ∈ {⊤, ⊥}"
  [decision display-name format-kw]
  (let [var-type (api/forced-typed decision)
        range-set (::api/range (meta decision))
        name-str (case format-kw
                   :latex (latex-var-name display-name)
                   :unicode (unicode-var-name display-name))]
    (cond
      ;; Set variable: A ⊆ universe
      (= var-type :igor.types/set)
      (when range-set
        (str name-str
             (case format-kw :latex " \\subseteq " :unicode " \u2286 ")
             (compact-range range-set format-kw)))

      ;; Boolean variable
      (= var-type :igor.types/boolean)
      (str name-str
           (case format-kw
             :latex " \\in \\{\\top, \\bot\\}"
             :unicode " \u2208 {\u22A4, \u22A5}"))

      ;; Numeric (int) variable with range
      range-set
      (str name-str
           (case format-kw :latex " \\in " :unicode " \u2208 ")
           (compact-range range-set format-kw))

      ;; Untyped/unbound — no declaration
      :else nil)))

(defn- render-declarations
  "Render domain declarations for all problem variables.
   Groups variables with identical domains onto one line."
  [decisions var-names format-kw]
  (let [;; Group decisions by [type range] to merge declarations
        grouped (group-by (fn [d]
                            [(api/forced-typed d) (::api/range (meta d))])
                          decisions)
        lines (keep
               (fn [[_key ds]]
                 (let [d (first ds)
                       var-type (api/forced-typed d)
                       range-set (::api/range (meta d))
                       names (map #(let [dn (get var-names %)]
                                     (case format-kw
                                       :latex (latex-var-name dn)
                                       :unicode (unicode-var-name dn)))
                                  ds)
                       name-str (str/join ", " names)]
                   (cond
                     (= var-type :igor.types/set)
                     (when range-set
                       (str name-str
                            (case format-kw :latex " \\subseteq " :unicode " \u2286 ")
                            (compact-range range-set format-kw)))

                     (= var-type :igor.types/boolean)
                     (str name-str
                          (case format-kw
                            :latex " \\in \\{\\top, \\bot\\}"
                            :unicode " \u2208 {\u22A4, \u22A5}"))

                     range-set
                     (str name-str
                          (case format-kw :latex " \\in " :unicode " \u2208 ")
                          (compact-range range-set format-kw))

                     :else nil)))
               grouped)]
    (vec lines)))

;; ============================================================
;; LaTeX render methods
;; ============================================================

;; --- TermAs ---
(defmethod render-node [TermAs :latex] [node env]
  (if (contains? (:definitions env) (:name node))
    (latex-name (:name node))
    ;; Not in definitions — render the inner expression directly
    (render-subtree (first (:argv node)) env)))

;; --- Decision variables ---
(defmethod render-node [igor.api.Decision :latex] [node env]
  (let [display-name (get-in env [:var-names node] (str (:id node)))]
    (latex-var-name display-name)))

;; --- Ground values ---
(defmethod render-node [java.lang.Long :latex] [node _env]
  (str node))

(defmethod render-node [java.lang.Double :latex] [node _env]
  (str node))

(defmethod render-node [java.lang.Integer :latex] [node _env]
  (str node))

(defmethod render-node [java.lang.Boolean :latex] [node _env]
  (if node "\\top" "\\bot"))

(defmethod render-node [clojure.lang.PersistentHashSet :latex] [node env]
  (str "\\{" (str/join ", " (map #(render-subtree % env) (sort node))) "\\}"))

(defmethod render-node [clojure.lang.PersistentTreeSet :latex] [node env]
  (str "\\{" (str/join ", " (map #(render-subtree % env) (sort node))) "\\}"))

;; --- Arithmetic ---
(defmethod render-node [igor.terms.core.TermPlus :latex] [node env]
  (render-children node " + " env))

(defmethod render-node [igor.terms.core.TermMinus :latex] [node env]
  (render-children node " - " env))

(defmethod render-node [igor.terms.core.TermProduct :latex] [node env]
  (render-children node " \\cdot " env))

(defmethod render-node [igor.terms.core.TermDivide :latex] [node env]
  (let [args (:argv node)]
    (str "\\frac{" (render-subtree (first args) env) "}{"
         (render-subtree (second args) env) "}")))

(defmethod render-node [igor.terms.core.TermAbs :latex] [node env]
  (str "\\left|" (render-subtree (first (:argv node)) env) "\\right|"))

(defmethod render-node [igor.terms.core.TermPow :latex] [node env]
  (let [[base exp] (:argv node)]
    (str "{" (render-child node base env) "}^{" (render-subtree exp env) "}")))

(defmethod render-node [igor.terms.core.TermMax :latex] [node env]
  (str "\\max(" (str/join ", " (map #(render-subtree % env) (:argv node))) ")"))

(defmethod render-node [igor.terms.core.TermMin :latex] [node env]
  (str "\\min(" (str/join ", " (map #(render-subtree % env) (:argv node))) ")"))

(defmethod render-node [igor.terms.core.TermMod :latex] [node env]
  (let [[a b] (:argv node)]
    (str (render-child node a env) " \\bmod " (render-child node b env))))

(defmethod render-node [igor.terms.core.TermRem :latex] [node env]
  (let [[a b] (:argv node)]
    (str (render-child node a env) " \\bmod " (render-child node b env))))

(defmethod render-node [igor.terms.core.TermInc :latex] [node env]
  (str (render-child node (first (:argv node)) env) " + 1"))

(defmethod render-node [igor.terms.core.TermDec :latex] [node env]
  (str (render-child node (first (:argv node)) env) " - 1"))

;; --- Comparison ---
(defmethod render-node [igor.terms.core.TermEquals :latex] [node env]
  (render-children node " = " env))

(defmethod render-node [igor.terms.core.TermGreaterThan :latex] [node env]
  (render-children node " > " env))

(defmethod render-node [igor.terms.core.TermLessThan :latex] [node env]
  (render-children node " < " env))

(defmethod render-node [igor.terms.core.TermGreaterThanOrEqualTo :latex] [node env]
  (render-children node " \\geq " env))

(defmethod render-node [igor.terms.core.TermLessThanOrEqualTo :latex] [node env]
  (render-children node " \\leq " env))

;; --- Logic ---
(defmethod render-node [igor.terms.core.TermAnd :latex] [node env]
  (render-children node " \\wedge " env))

(defmethod render-node [igor.terms.core.TermOr :latex] [node env]
  (render-children node " \\vee " env))

(defmethod render-node [igor.terms.core.TermNot :latex] [node env]
  (str "\\neg " (render-child node (first (:argv node)) env)))

(defmethod render-node [igor.terms.core.TermWhen :latex] [node env]
  (let [[ante conseq] (:argv node)]
    (str (render-child node ante env) " \\Rightarrow " (render-child node conseq env))))

;; --- Conditional ---
(defmethod render-node [igor.terms.core.TermIf :latex] [node env]
  (let [[test then else] (:argv node)]
    (str "\\begin{cases} "
         (render-subtree then env) " & \\text{if } " (render-subtree test env)
         " \\\\ "
         (render-subtree else env) " & \\text{otherwise}"
         " \\end{cases}")))

(defmethod render-node [igor.terms.core.TermCond :latex] [node env]
  (let [pairs (partition-all 2 (:argv node))]
    (str "\\begin{cases} "
         (str/join " \\\\ "
                   (map (fn [[test-or-val expr :as pair]]
                          (if (= 2 (count pair))
                            (str (render-subtree expr env) " & \\text{if } " (render-subtree test-or-val env))
                            (str (render-subtree test-or-val env) " & \\text{otherwise}")))
                        pairs))
         " \\end{cases}")))

;; --- Set membership / contains ---
(defmethod render-node [igor.terms.core.TermContains :latex] [node env]
  (let [[set-expr elem] (:argv node)]
    (str (render-subtree elem env) " \\in " (render-subtree set-expr env))))

(defmethod render-node [igor.terms.core.TermCount :latex] [node env]
  (str "\\left|" (render-subtree (first (:argv node)) env) "\\right|"))

;; --- Set operations ---
(defmethod render-node [igor.terms.set.TermUnion :latex] [node env]
  (render-children node " \\cup " env))

(defmethod render-node [igor.terms.set.TermIntersection :latex] [node env]
  (render-children node " \\cap " env))

(defmethod render-node [igor.terms.set.TermDifference :latex] [node env]
  (render-children node " \\setminus " env))

(defmethod render-node [igor.terms.set.TermSymDiff :latex] [node env]
  (render-children node " \\triangle " env))

(defmethod render-node [igor.terms.set.TermSubset :latex] [node env]
  (render-children node " \\subseteq " env))

(defmethod render-node [igor.terms.set.TermSuperset :latex] [node env]
  (render-children node " \\supseteq " env))

;; --- AllDifferent ---
(defmethod render-node [igor.terms.core.TermAllDifferent :latex] [node env]
  (str "\\text{allDiff}(" (str/join ", " (map #(render-subtree % env) (:argv node))) ")"))

;; --- Extensional constraints ---
(defmethod render-node [igor.extensional.TermTable :latex] [node env]
  (str "\\langle " (str/join ", " (map #(render-subtree % env) (:argv node)))
       " \\rangle \\in T"))

(defmethod render-node [igor.extensional.TermRegular :latex] [node env]
  (str "\\text{regular}(\\vec{x}, \\mathcal{A})"))

(defmethod render-node [igor.extensional.TermCostRegular :latex] [node env]
  (str "\\text{costRegular}(\\vec{x}, "
       (render-subtree (:cost node) env)
       ", \\mathcal{A})"))

;; --- Quantifiers ---
(defmethod render-node [igor.terms.introduced.TermEvery? :latex] [node env]
  (let [[local-decision set-expr constraint-expr] (:argv node)]
    (str "\\forall " (render-subtree local-decision env)
         " \\in " (render-subtree set-expr env)
         " : " (render-subtree constraint-expr env))))

(defmethod render-node [igor.terms.introduced.TermImage :latex] [node env]
  (let [[local-decision set-expr generator-expr] (:argv node)]
    (str "\\{ " (render-subtree generator-expr env)
         " \\mid " (render-subtree local-decision env)
         " \\in " (render-subtree set-expr env) " \\}")))

;; --- Graph constraints (function-call style) ---
(defmethod render-node [igor.graph.TermGraphCircuit :latex] [node _env]
  "\\text{circuit}(G)")

(defmethod render-node [igor.graph.TermGraphSubCircuit :latex] [node _env]
  "\\text{subcircuit}(G)")

(defmethod render-node [igor.graph.TermGraphSubgraph :latex] [node _env]
  "\\text{subgraph}(G)")

(defmethod render-node [igor.graph.TermGraphPath :latex] [node env]
  (str "\\text{path}(G, " (render-subtree (:source node) env) ", " (render-subtree (:target node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDPath :latex] [node env]
  (str "\\text{dpath}(G, " (render-subtree (:source node) env) ", " (render-subtree (:target node) env) ")"))

(defmethod render-node [igor.graph.TermGraphBoundedPath :latex] [node env]
  (str "\\text{boundedPath}(G, " (render-subtree (:source node) env) ", "
       (render-subtree (:target node) env) ", " (render-subtree (:cost node) env) ")"))

(defmethod render-node [igor.graph.TermGraphBoundedDPath :latex] [node env]
  (str "\\text{boundedDPath}(G, " (render-subtree (:source node) env) ", "
       (render-subtree (:target node) env) ", " (render-subtree (:cost node) env) ")"))

(defmethod render-node [igor.graph.TermGraphConnected :latex] [node _env]
  "\\text{connected}(G)")

(defmethod render-node [igor.graph.TermGraphDConnected :latex] [node _env]
  "\\text{dconnected}(G)")

(defmethod render-node [igor.graph.TermGraphDag :latex] [node _env]
  "\\text{dag}(G)")

(defmethod render-node [igor.graph.TermGraphTree :latex] [node env]
  (str "\\text{tree}(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDTree :latex] [node env]
  (str "\\text{dtree}(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphReachable :latex] [node env]
  (str "\\text{reachable}(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDReachable :latex] [node env]
  (str "\\text{dreachable}(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphWeightedSpanningTree :latex] [node env]
  (str "\\text{wst}(G, " (render-subtree (:cost node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDWeightedSpanningTree :latex] [node env]
  (str "\\text{dwst}(G, " (render-subtree (:root node) env) ", " (render-subtree (:cost node) env) ")"))

;; --- Expansion-only terms (render pre-expansion form) ---
(defmethod render-node [igor.terms.core.TermEven? :latex] [node env]
  (str (render-subtree (first (:argv node)) env) " \\text{ is even}"))

(defmethod render-node [igor.terms.core.TermOdd? :latex] [node env]
  (str (render-subtree (first (:argv node)) env) " \\text{ is odd}"))

(defmethod render-node [igor.terms.core.TermPos? :latex] [node env]
  (str (render-child node (first (:argv node)) env) " > 0"))

(defmethod render-node [igor.terms.core.TermNeg? :latex] [node env]
  (str (render-child node (first (:argv node)) env) " < 0"))

(defmethod render-node [igor.terms.core.TermZero? :latex] [node env]
  (str (render-child node (first (:argv node)) env) " = 0"))

(defmethod render-node [igor.terms.core.TermTrue? :latex] [node env]
  (str (render-subtree (first (:argv node)) env) " = \\top"))

(defmethod render-node [igor.terms.core.TermFalse? :latex] [node env]
  (str (render-subtree (first (:argv node)) env) " = \\bot"))

(defmethod render-node [igor.terms.core.TermNth :latex] [node env]
  (let [elems (subvec (:argv node) 0 (:n node))
        idx (get (:argv node) (:n node))]
    (str "[" (str/join ", " (map #(render-subtree % env) elems)) "]_{"
         (render-subtree idx env) "}")))

;; --- Default fallback ---
(defmethod render-node :default [node env]
  (if (satisfies? protocols/IExpress node)
    (str (protocols/write node))
    (str node)))

;; ============================================================
;; Unicode render methods
;; ============================================================

;; --- TermAs ---
(defmethod render-node [TermAs :unicode] [node env]
  (if (contains? (:definitions env) (:name node))
    (unicode-name (:name node))
    (render-subtree (first (:argv node)) env)))

;; --- Decision variables ---
(defmethod render-node [igor.api.Decision :unicode] [node env]
  (let [display-name (get-in env [:var-names node] (str (:id node)))]
    (unicode-var-name display-name)))

;; --- Ground values ---
(defmethod render-node [java.lang.Long :unicode] [node _env]
  (str node))

(defmethod render-node [java.lang.Double :unicode] [node _env]
  (str node))

(defmethod render-node [java.lang.Integer :unicode] [node _env]
  (str node))

(defmethod render-node [java.lang.Boolean :unicode] [node _env]
  (if node "\u22A4" "\u22A5"))

(defmethod render-node [clojure.lang.PersistentHashSet :unicode] [node env]
  (str "{" (str/join ", " (map #(render-subtree % env) (sort node))) "}"))

(defmethod render-node [clojure.lang.PersistentTreeSet :unicode] [node env]
  (str "{" (str/join ", " (map #(render-subtree % env) (sort node))) "}"))

;; --- Arithmetic ---
(defmethod render-node [igor.terms.core.TermPlus :unicode] [node env]
  (render-children node " + " env))

(defmethod render-node [igor.terms.core.TermMinus :unicode] [node env]
  (render-children node " - " env))

(defmethod render-node [igor.terms.core.TermProduct :unicode] [node env]
  (render-children node " \u00B7 " env))

(defmethod render-node [igor.terms.core.TermDivide :unicode] [node env]
  (let [[a b] (:argv node)]
    (str (render-child node a env) " / " (render-child node b env))))

(defmethod render-node [igor.terms.core.TermAbs :unicode] [node env]
  (str "|" (render-subtree (first (:argv node)) env) "|"))

(defmethod render-node [igor.terms.core.TermPow :unicode] [node env]
  (let [[base exp] (:argv node)]
    (str (render-child node base env) "^" (render-subtree exp env))))

(defmethod render-node [igor.terms.core.TermMax :unicode] [node env]
  (str "max(" (str/join ", " (map #(render-subtree % env) (:argv node))) ")"))

(defmethod render-node [igor.terms.core.TermMin :unicode] [node env]
  (str "min(" (str/join ", " (map #(render-subtree % env) (:argv node))) ")"))

(defmethod render-node [igor.terms.core.TermMod :unicode] [node env]
  (let [[a b] (:argv node)]
    (str (render-child node a env) " mod " (render-child node b env))))

(defmethod render-node [igor.terms.core.TermRem :unicode] [node env]
  (let [[a b] (:argv node)]
    (str (render-child node a env) " mod " (render-child node b env))))

(defmethod render-node [igor.terms.core.TermInc :unicode] [node env]
  (str (render-child node (first (:argv node)) env) " + 1"))

(defmethod render-node [igor.terms.core.TermDec :unicode] [node env]
  (str (render-child node (first (:argv node)) env) " - 1"))

;; --- Comparison ---
(defmethod render-node [igor.terms.core.TermEquals :unicode] [node env]
  (render-children node " = " env))

(defmethod render-node [igor.terms.core.TermGreaterThan :unicode] [node env]
  (render-children node " > " env))

(defmethod render-node [igor.terms.core.TermLessThan :unicode] [node env]
  (render-children node " < " env))

(defmethod render-node [igor.terms.core.TermGreaterThanOrEqualTo :unicode] [node env]
  (render-children node " \u2265 " env))

(defmethod render-node [igor.terms.core.TermLessThanOrEqualTo :unicode] [node env]
  (render-children node " \u2264 " env))

;; --- Logic ---
(defmethod render-node [igor.terms.core.TermAnd :unicode] [node env]
  (render-children node " \u2227 " env))

(defmethod render-node [igor.terms.core.TermOr :unicode] [node env]
  (render-children node " \u2228 " env))

(defmethod render-node [igor.terms.core.TermNot :unicode] [node env]
  (str "\u00AC" (render-child node (first (:argv node)) env)))

(defmethod render-node [igor.terms.core.TermWhen :unicode] [node env]
  (let [[ante conseq] (:argv node)]
    (str (render-child node ante env) " \u21D2 " (render-child node conseq env))))

;; --- Conditional ---
(defmethod render-node [igor.terms.core.TermIf :unicode] [node env]
  (let [[test then else] (:argv node)]
    (str "if " (render-subtree test env) " then " (render-subtree then env)
         " else " (render-subtree else env))))

(defmethod render-node [igor.terms.core.TermCond :unicode] [node env]
  (let [pairs (partition-all 2 (:argv node))]
    (str/join "; "
              (map (fn [[test-or-val expr :as pair]]
                     (if (= 2 (count pair))
                       (str (render-subtree expr env) " if " (render-subtree test-or-val env))
                       (str (render-subtree test-or-val env) " otherwise")))
                   pairs))))

;; --- Set membership ---
(defmethod render-node [igor.terms.core.TermContains :unicode] [node env]
  (let [[set-expr elem] (:argv node)]
    (str (render-subtree elem env) " \u2208 " (render-subtree set-expr env))))

(defmethod render-node [igor.terms.core.TermCount :unicode] [node env]
  (str "|" (render-subtree (first (:argv node)) env) "|"))

;; --- Set operations ---
(defmethod render-node [igor.terms.set.TermUnion :unicode] [node env]
  (render-children node " \u222A " env))

(defmethod render-node [igor.terms.set.TermIntersection :unicode] [node env]
  (render-children node " \u2229 " env))

(defmethod render-node [igor.terms.set.TermDifference :unicode] [node env]
  (render-children node " \\ " env))

(defmethod render-node [igor.terms.set.TermSymDiff :unicode] [node env]
  (render-children node " \u25B3 " env))

(defmethod render-node [igor.terms.set.TermSubset :unicode] [node env]
  (render-children node " \u2286 " env))

(defmethod render-node [igor.terms.set.TermSuperset :unicode] [node env]
  (render-children node " \u2287 " env))

;; --- AllDifferent ---
(defmethod render-node [igor.terms.core.TermAllDifferent :unicode] [node env]
  (str "allDiff(" (str/join ", " (map #(render-subtree % env) (:argv node))) ")"))

;; --- Extensional ---
(defmethod render-node [igor.extensional.TermTable :unicode] [node env]
  (str "\u27E8" (str/join ", " (map #(render-subtree % env) (:argv node))) "\u27E9 \u2208 T"))

(defmethod render-node [igor.extensional.TermRegular :unicode] [node _env]
  "regular(x\u20D7, \uD835\uDC9C)")

(defmethod render-node [igor.extensional.TermCostRegular :unicode] [node env]
  (str "costRegular(x\u20D7, " (render-subtree (:cost node) env) ", \uD835\uDC9C)"))

;; --- Quantifiers ---
(defmethod render-node [igor.terms.introduced.TermEvery? :unicode] [node env]
  (let [[local-decision set-expr constraint-expr] (:argv node)]
    (str "\u2200 " (render-subtree local-decision env)
         " \u2208 " (render-subtree set-expr env)
         " : " (render-subtree constraint-expr env))))

(defmethod render-node [igor.terms.introduced.TermImage :unicode] [node env]
  (let [[local-decision set-expr generator-expr] (:argv node)]
    (str "{ " (render-subtree generator-expr env)
         " | " (render-subtree local-decision env)
         " \u2208 " (render-subtree set-expr env) " }")))

;; --- Graph constraints ---
(defmethod render-node [igor.graph.TermGraphCircuit :unicode] [node _env]
  "circuit(G)")

(defmethod render-node [igor.graph.TermGraphSubCircuit :unicode] [node _env]
  "subcircuit(G)")

(defmethod render-node [igor.graph.TermGraphSubgraph :unicode] [node _env]
  "subgraph(G)")

(defmethod render-node [igor.graph.TermGraphPath :unicode] [node env]
  (str "path(G, " (render-subtree (:source node) env) ", " (render-subtree (:target node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDPath :unicode] [node env]
  (str "dpath(G, " (render-subtree (:source node) env) ", " (render-subtree (:target node) env) ")"))

(defmethod render-node [igor.graph.TermGraphConnected :unicode] [node _env]
  "connected(G)")

(defmethod render-node [igor.graph.TermGraphDConnected :unicode] [node _env]
  "dconnected(G)")

(defmethod render-node [igor.graph.TermGraphDag :unicode] [node _env]
  "dag(G)")

(defmethod render-node [igor.graph.TermGraphTree :unicode] [node env]
  (str "tree(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDTree :unicode] [node env]
  (str "dtree(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphReachable :unicode] [node env]
  (str "reachable(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDReachable :unicode] [node env]
  (str "dreachable(G, " (render-subtree (:root node) env) ")"))

(defmethod render-node [igor.graph.TermGraphWeightedSpanningTree :unicode] [node env]
  (str "wst(G, " (render-subtree (:cost node) env) ")"))

(defmethod render-node [igor.graph.TermGraphDWeightedSpanningTree :unicode] [node env]
  (str "dwst(G, " (render-subtree (:root node) env) ", " (render-subtree (:cost node) env) ")"))

;; --- Expansion-only terms ---
(defmethod render-node [igor.terms.core.TermEven? :unicode] [node env]
  (str (render-subtree (first (:argv node)) env) " is even"))

(defmethod render-node [igor.terms.core.TermOdd? :unicode] [node env]
  (str (render-subtree (first (:argv node)) env) " is odd"))

(defmethod render-node [igor.terms.core.TermPos? :unicode] [node env]
  (str (render-child node (first (:argv node)) env) " > 0"))

(defmethod render-node [igor.terms.core.TermNeg? :unicode] [node env]
  (str (render-child node (first (:argv node)) env) " < 0"))

(defmethod render-node [igor.terms.core.TermZero? :unicode] [node env]
  (str (render-child node (first (:argv node)) env) " = 0"))

(defmethod render-node [igor.terms.core.TermTrue? :unicode] [node env]
  (str (render-subtree (first (:argv node)) env) " = \u22A4"))

(defmethod render-node [igor.terms.core.TermFalse? :unicode] [node env]
  (str (render-subtree (first (:argv node)) env) " = \u22A5"))

(defmethod render-node [igor.terms.core.TermNth :unicode] [node env]
  (let [elems (subvec (:argv node) 0 (:n node))
        idx (get (:argv node) (:n node))]
    (str "[" (str/join ", " (map #(render-subtree % env) elems)) "]["
         (render-subtree idx env) "]")))

;; ============================================================
;; Definition collection and topological sort
;; ============================================================

(defn- collect-definitions
  "Walk an AST, collecting all TermAs nodes into {name -> node}."
  [root]
  (let [defs (atom {})]
    (walk/prewalk
     (fn [node]
       (when (instance? TermAs node)
         (swap! defs assoc (:name node) node))
       node)
     root)
    @defs))

(defn- walk-argv
  "Walk the argv structure of AST nodes, collecting relevant children."
  [node]
  (cond
    (instance? TermAs node) [node]
    (and (record? node) (:argv node))
    (mapcat walk-argv (:argv node))
    (set? node)
    (mapcat walk-argv node)
    :else []))

(defn- definition-deps
  "For a TermAs node, find which other TermAs names it references."
  [term-as-node]
  (let [inner (first (:argv term-as-node))
        deps (atom #{})]
    (walk/prewalk
     (fn [node]
       (when (and (instance? TermAs node)
                  (not= node term-as-node))
         (swap! deps conj (:name node)))
       node)
     inner)
    @deps))

(defn- topo-sort
  "Topological sort of definition names by dependency.
   definitions is {name -> TermAs-node}.
   Returns names in dependency order (dependencies first)."
  [definitions]
  (let [def-name-set (set (keys definitions))
        ;; dep-graph: name -> set of names this definition depends on
        ;; Only count deps that are actually in the definitions map
        dep-graph (into {} (map (fn [[name node]]
                                  [name (clojure.set/intersection
                                         (definition-deps node)
                                         def-name-set)])
                                definitions))
        ;; Reverse graph: name -> set of names that depend on this one
        rev-graph (reduce (fn [m [name deps]]
                            (reduce (fn [m2 d]
                                      (update m2 d (fnil conj #{}) name))
                                    m deps))
                          {} dep-graph)
        ;; Kahn's algorithm using in-degree from dep-graph (forward deps)
        in-degree (atom (into {} (map (fn [n] [n (count (get dep-graph n))]) (keys dep-graph))))
        queue (atom (into clojure.lang.PersistentQueue/EMPTY
                          (filter #(zero? (get @in-degree %)) (keys dep-graph))))
        result (atom [])]
    (while (seq @queue)
      (let [n (peek @queue)]
        (swap! queue pop)
        (swap! result conj n)
        ;; For each name that depends on n, decrement in-degree
        (doseq [dependent (get rev-graph n)]
          (when (contains? @in-degree dependent)
            (swap! in-degree update dependent clojure.core/dec)
            (when (zero? (get @in-degree dependent))
              (swap! queue conj dependent))))))
    @result))

;; ============================================================
;; render-notation — main entry point
;; ============================================================

(defn- alias-definition?
  "True when a TermAs wraps a bare Decision (no expression to define)."
  [term-as-node]
  (instance? igor.api.Decision (first (:argv term-as-node))))

(defn- extract-aliases
  "From a definitions map, extract alias entries (as wrapping bare Decision).
   Returns {:aliases {Decision -> keyword-name}, :real-defs {name -> node}}."
  [definitions]
  (reduce-kv (fn [acc kw-name node]
               (if (alias-definition? node)
                 (update acc :aliases assoc (first (:argv node)) kw-name)
                 (update acc :real-defs assoc kw-name node)))
             {:aliases {} :real-defs {}}
             definitions))

(defn- alias-var-names
  "Convert alias entries {Decision -> :keyword} into var-names {Decision -> string}."
  [aliases]
  (into {} (map (fn [[decision kw]] [decision (keyword->name kw)]) aliases)))

(defn render-definition
  "Render a single definition line."
  [name term-as-node env]
  (let [inner (first (:argv term-as-node))
        name-str (case (:format env)
                   :latex (latex-name name)
                   :unicode (unicode-name name))]
    (str name-str
         (case (:format env)
           :latex " := "
           :unicode " := ")
         (render-subtree inner env))))

(defn render-notation
  "Render a constraint AST as mathematical notation.

   Options:
     :format - :latex (default) or :unicode

   Returns a string with definitions in dependency order,
   followed by the top-level expression."
  [expr & {:keys [format] :or {format :latex}}]
  (let [definitions (collect-definitions expr)
        {:keys [aliases real-defs]} (extract-aliases definitions)
        def-names (set (keys real-defs))
        sorted-names (topo-sort real-defs)
        var-names (merge (assign-display-names expr) (alias-var-names aliases))
        env {:format format :definitions def-names :var-names var-names}
        problem-vars (collect-decisions expr)
        decl-lines (render-declarations problem-vars var-names format)
        def-lines (mapv (fn [name]
                          (render-definition name (get real-defs name) env))
                        sorted-names)
        top-line (render-subtree expr env)]
    (str/join "\n" (concat decl-lines def-lines [top-line]))))

(defn render-problem
  "Render a constraint problem as mathematical notation.
   Like render-notation but wraps the top-level expression in
   \\text{satisfy}(...) or \\text{maximize}(obj, ...).

   Options:
     :format - :latex (default) or :unicode
     :objective - if provided, renders as an optimization problem
     :direction - :maximize (default) or :minimize"
  [expr & {:keys [format objective direction] :or {format :latex}}]
  (let [definitions (collect-definitions expr)
        obj-defs (when objective (collect-definitions objective))
        all-defs (merge definitions obj-defs)
        {:keys [aliases real-defs]} (extract-aliases all-defs)
        def-names (set (keys real-defs))
        sorted-names (topo-sort real-defs)
        ;; Collect var names from both expr and objective
        var-names (merge (assign-display-names expr)
                         (alias-var-names aliases)
                         (when objective
                           (let [existing-count (count (collect-decisions expr))
                                 obj-decisions (collect-decisions objective)
                                 ;; Only name decisions not already seen in expr
                                 expr-decisions (set (collect-decisions expr))
                                 new-decisions (remove expr-decisions obj-decisions)]
                             (into {}
                                   (map-indexed
                                    (fn [idx d]
                                      [d (str "x" (+ existing-count idx 1))])
                                    new-decisions)))))
        env {:format format :definitions def-names :var-names var-names}
        ;; Collect all problem vars from expr and objective
        all-problem-vars (distinct (concat (collect-decisions expr)
                                           (when objective (collect-decisions objective))))
        decl-lines (render-declarations all-problem-vars var-names format)
        def-lines (mapv (fn [name]
                          (render-definition name (get real-defs name) env))
                        sorted-names)
        constraint-str (render-subtree expr env)
        dir-name (name (or direction :maximize))
        problem-line (if objective
                       (let [obj-str (render-subtree objective env)]
                         (case format
                           :latex (str "\\text{" dir-name " } " obj-str " \\text{ subject to } " constraint-str)
                           :unicode (str dir-name " " obj-str " subject to " constraint-str)))
                       (case format
                         :latex (str "\\text{satisfy}(" constraint-str ")")
                         :unicode (str "satisfy(" constraint-str ")")))]
    (str/join "\n" (concat decl-lines def-lines [problem-line]))))
