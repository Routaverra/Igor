(ns routaverra.igor.graph
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.types :as types]
            [routaverra.igor.terms.core :as terms]))

;; ============================================================
;; Digraph record — immutable ground topology
;; ============================================================

(defrecord Digraph [nodes edges from-arr to-arr weights n-nodes n-edges])

(defn digraph
  "Create a directed graph from an edge list. Edges are [from to] or [from to weight].
   Optional first arg n-nodes for graphs with isolated nodes."
  ([edge-list]
   (digraph nil edge-list))
  ([n-nodes edge-list]
   (let [edges (vec edge-list)
         has-weights? (and (seq edges) (= 3 (count (first edges))))
         from-arr (mapv first edges)
         to-arr (mapv #(nth % 1) edges)
         weights (when has-weights? (mapv #(nth % 2) edges))
         inferred-nodes (into (sorted-set)
                              (concat from-arr to-arr
                                      (when n-nodes (range n-nodes))))
         actual-n-nodes (if n-nodes
                          (max n-nodes (count inferred-nodes))
                          (count inferred-nodes))
         n-edges (count edges)]
     (->Digraph inferred-nodes edges from-arr to-arr weights actual-n-nodes n-edges))))

;; ============================================================
;; Internal helpers
;; ============================================================

(defn- make-ns-es
  "Create n-nodes bool vars + n-edges bool vars."
  [graph]
  {:ns-vars (vec (repeatedly (:n-nodes graph) api/fresh-bool))
   :es-vars (vec (repeatedly (:n-edges graph) api/fresh-bool))})

(defn- make-es
  "Create just n-edges bool vars (for spanning tree variants)."
  [graph]
  {:es-vars (vec (repeatedly (:n-edges graph) api/fresh-bool))})

(defn- translate-1idx
  "Translate a value to 1-indexed MiniZinc. If ground integer, increment.
   If decision var, emit (var + 1)."
  [x]
  (if (integer? x)
    (str (inc x))
    (str "(" (protocols/translate x) " + 1)")))

(defn- graph-from-str [graph]
  (terms/to-literal-array (map #(str (inc %)) (:from-arr graph))))

(defn- graph-to-str [graph]
  (terms/to-literal-array (map #(str (inc %)) (:to-arr graph))))

(defn- graph-weight-str [graph]
  (terms/to-literal-array (map str (:weights graph))))

(defn- ns-str [ns-vars]
  (terms/to-literal-array (map protocols/translate ns-vars)))

(defn- es-str [es-vars]
  (terms/to-literal-array (map protocols/translate es-vars)))

;; ============================================================
;; Local graph algorithms (ground pass-through)
;; ============================================================

(defn adjacency-list
  "Build adjacency list from graph edges. Returns {from -> [to ...]}."
  [graph]
  (reduce (fn [m i]
            (update m (nth (:from-arr graph) i)
                    (fnil conj []) (nth (:to-arr graph) i)))
          {} (range (:n-edges graph))))

(defn undirected-adjacency-list
  "Build undirected adjacency list from graph edges."
  [graph]
  (reduce (fn [m i]
            (let [f (nth (:from-arr graph) i)
                  t (nth (:to-arr graph) i)]
              (-> m
                  (update f (fnil conj []) t)
                  (update t (fnil conj []) f))))
          {} (range (:n-edges graph))))

(defn dfs-reachable
  "Set of nodes reachable from start following directed edges."
  [adj start]
  (loop [stack [start] visited #{}]
    (if (empty? stack)
      visited
      (let [node (peek stack)
            stack (pop stack)]
        (if (visited node)
          (recur stack visited)
          (recur (into stack (get adj node [])) (conj visited node)))))))

(defn bfs-reachable
  "Set of nodes reachable from start following adjacency list."
  [adj start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start) visited #{}]
    (if (empty? queue)
      visited
      (let [node (peek queue)
            queue (pop queue)]
        (if (visited node)
          (recur queue visited)
          (recur (into queue (get adj node [])) (conj visited node)))))))

;; ============================================================
;; Evaluation helpers (pure Clojure graph checks)
;; ============================================================

(defn- eval-subgraph
  "Given a graph handle with ns-vars and es-vars, evaluate which nodes/edges are active.
   Returns {:active-nodes #{...} :active-edges #{[from to] ...}} or nil if invalid subgraph."
  [graph ns-vars es-vars solution]
  (let [nodes (vec (sort (:nodes graph)))
        active-ns (into (sorted-set)
                        (for [i (range (count ns-vars))
                              :when (true? (api/eval-arg (nth ns-vars i) solution))]
                          (nth nodes i)))
        active-es (into #{}
                        (for [i (range (count es-vars))
                              :when (true? (api/eval-arg (nth es-vars i) solution))]
                          (let [e (nth (:edges graph) i)]
                            [(first e) (second e)])))
        ;; subgraph validity: every active edge must connect active nodes
        valid? (every? (fn [[f t]] (and (contains? active-ns f) (contains? active-ns t)))
                       active-es)]
    (when valid?
      {:active-nodes active-ns :active-edges active-es})))

(defn- subgraph-adj
  "Build directed adjacency list from active edges."
  [active-edges]
  (reduce (fn [m [f t]] (update m f (fnil conj []) t)) {} active-edges))

(defn- subgraph-undirected-adj
  "Build undirected adjacency list from active edges."
  [active-edges]
  (reduce (fn [m [f t]]
            (-> m
                (update f (fnil conj []) t)
                (update t (fnil conj []) f)))
          {} active-edges))

(defn- strongly-connected?
  "Check if the active nodes form a strongly connected subgraph."
  [active-nodes active-edges]
  (if (empty? active-nodes)
    true
    (let [adj (subgraph-adj active-edges)
          rev-adj (reduce (fn [m [f t]] (update m t (fnil conj []) f)) {} active-edges)
          start (first active-nodes)
          fwd (dfs-reachable adj start)
          bwd (dfs-reachable rev-adj start)]
      (and (every? fwd active-nodes)
           (every? bwd active-nodes)))))

(defn- undirected-connected?
  "Check if the active nodes form a connected undirected subgraph."
  [active-nodes active-edges]
  (if (empty? active-nodes)
    true
    (let [adj (subgraph-undirected-adj active-edges)
          start (first active-nodes)
          reached (bfs-reachable adj start)]
      (every? reached active-nodes))))

(defn- has-directed-cycle?
  "Check if the directed graph on active-nodes/edges has a cycle."
  [active-nodes active-edges]
  (let [adj (subgraph-adj active-edges)]
    (loop [remaining active-nodes
           visited #{}
           in-stack #{}]
      (if (empty? remaining)
        false
        (let [start (first remaining)]
          (if (visited start)
            (recur (rest remaining) visited in-stack)
            (let [result (atom false)
                  v (atom visited)
                  s (atom in-stack)]
              ;; iterative DFS with explicit stack tracking path
              (loop [stack [[start :enter]]]
                (when (seq stack)
                  (let [[node action] (peek stack)
                        stack (pop stack)]
                    (case action
                      :enter
                      (if (@s node)
                        (reset! result true)
                        (when-not (@v node)
                          (swap! v conj node)
                          (swap! s conj node)
                          (recur (into (conj stack [node :exit])
                                       (map (fn [n] [n :enter]) (get adj node []))))))
                      :exit
                      (do (swap! s disj node)
                          (recur stack))))))
              (if @result
                true
                (recur (rest remaining) @v @s)))))))))

;; ============================================================
;; Constraint term records
;; ============================================================

;; --- subgraph.mzn ---

(defrecord TermGraphSubgraph [argv graph ns-vars es-vars]
  protocols/IInclude
  (mzn-includes [_self] #{"subgraph.mzn"})
  protocols/IExpress
  (write [_self] (list 'subgraph (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (repeat (count argv) {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "subgraph(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (some? (eval-subgraph graph ns-vars es-vars solution))))

(defrecord TermGraphReachable [argv graph ns-vars es-vars root]
  protocols/IInclude
  (mzn-includes [_self] #{"subgraph.mzn"})
  protocols/IExpress
  (write [_self] (list 'reachable (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "reachable(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (translate-1idx (:root self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [root-val (api/eval-arg root solution)
            adj (subgraph-undirected-adj active-edges)
            reached (bfs-reachable adj root-val)]
        (every? reached active-nodes)))))

(defrecord TermGraphDReachable [argv graph ns-vars es-vars root]
  protocols/IInclude
  (mzn-includes [_self] #{"subgraph.mzn"})
  protocols/IExpress
  (write [_self] (list 'dreachable (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "dreachable(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (translate-1idx (:root self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [root-val (api/eval-arg root solution)
            adj (subgraph-adj active-edges)
            reached (dfs-reachable adj root-val)]
        (every? reached active-nodes)))))

;; --- connected.mzn (enum-indexed only, no N/E params) ---

(defrecord TermGraphConnected [argv graph ns-vars es-vars]
  protocols/IInclude
  (mzn-includes [_self] #{"connected.mzn"})
  protocols/IExpress
  (write [_self] (list 'connected (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (repeat (count argv) {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)]
      (str "connected("
           (graph-from-str g) ", " (graph-to-str g) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (undirected-connected? active-nodes active-edges))))

(defrecord TermGraphDConnected [argv graph ns-vars es-vars]
  protocols/IInclude
  (mzn-includes [_self] #{"connected.mzn"})
  protocols/IExpress
  (write [_self] (list 'dconnected (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (repeat (count argv) {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)]
      (str "dconnected("
           (graph-from-str g) ", " (graph-to-str g) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (strongly-connected? active-nodes active-edges))))

;; --- dag.mzn (enum-indexed only) ---

(defrecord TermGraphDag [argv graph ns-vars es-vars]
  protocols/IInclude
  (mzn-includes [_self] #{"dag.mzn"})
  protocols/IExpress
  (write [_self] (list 'dag (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (repeat (count argv) {types/Bool self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)]
      (str "dag("
           (graph-from-str g) ", " (graph-to-str g) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (not (has-directed-cycle? active-nodes active-edges)))))

;; --- path.mzn ---

(defrecord TermGraphPath [argv graph ns-vars es-vars source target]
  protocols/IInclude
  (mzn-includes [_self] #{"path.mzn"})
  protocols/IExpress
  (write [_self] (list 'path (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "path(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (translate-1idx (:source self)) ", " (translate-1idx (:target self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [src (api/eval-arg source solution)
            tgt (api/eval-arg target solution)]
        (and (contains? active-nodes src)
             (contains? active-nodes tgt)
             (undirected-connected? active-nodes active-edges))))))

(defrecord TermGraphDPath [argv graph ns-vars es-vars source target]
  protocols/IInclude
  (mzn-includes [_self] #{"path.mzn"})
  protocols/IExpress
  (write [_self] (list 'dpath (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "dpath(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (translate-1idx (:source self)) ", " (translate-1idx (:target self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [src (api/eval-arg source solution)
            tgt (api/eval-arg target solution)
            adj (subgraph-adj active-edges)
            reached (dfs-reachable adj src)]
        (and (contains? active-nodes src)
             (contains? active-nodes tgt)
             (contains? reached tgt))))))

;; --- bounded_path.mzn ---

(defrecord TermGraphBoundedPath [argv graph ns-vars es-vars source target cost]
  protocols/IInclude
  (mzn-includes [_self] #{"bounded_path.mzn"})
  protocols/IExpress
  (write [_self] (list 'bounded-path (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self} {types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "bounded_path(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (graph-weight-str g) ", "
           (translate-1idx (:source self)) ", " (translate-1idx (:target self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ", "
           (protocols/translate (:cost self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [src (api/eval-arg source solution)
            tgt (api/eval-arg target solution)
            cost-val (api/eval-arg cost solution)
            edge-weights (into {} (map (fn [e] [[(first e) (second e)] (nth e 2)]) (:edges graph)))
            total (reduce + (map #(get edge-weights % 0) active-edges))]
        (and (contains? active-nodes src)
             (contains? active-nodes tgt)
             (undirected-connected? active-nodes active-edges)
             (= cost-val total))))))

(defrecord TermGraphBoundedDPath [argv graph ns-vars es-vars source target cost]
  protocols/IInclude
  (mzn-includes [_self] #{"bounded_path.mzn"})
  protocols/IExpress
  (write [_self] (list 'bounded-dpath (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self} {types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "bounded_dpath(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (graph-weight-str g) ", "
           (translate-1idx (:source self)) ", " (translate-1idx (:target self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ", "
           (protocols/translate (:cost self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [src (api/eval-arg source solution)
            tgt (api/eval-arg target solution)
            cost-val (api/eval-arg cost solution)
            adj (subgraph-adj active-edges)
            reached (dfs-reachable adj src)
            edge-weights (into {} (map (fn [e] [[(first e) (second e)] (nth e 2)]) (:edges graph)))
            total (reduce + (map #(get edge-weights % 0) active-edges))]
        (and (contains? active-nodes src)
             (contains? active-nodes tgt)
             (contains? reached tgt)
             (= cost-val total))))))

;; --- tree.mzn ---

(defrecord TermGraphTree [argv graph ns-vars es-vars root]
  protocols/IInclude
  (mzn-includes [_self] #{"tree.mzn"})
  protocols/IExpress
  (write [_self] (list 'tree (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "tree(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (translate-1idx (:root self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [root-val (api/eval-arg root solution)
            n-nodes (count active-nodes)
            n-edges (count active-edges)]
        (and (contains? active-nodes root-val)
             (= n-edges (dec n-nodes))
             (undirected-connected? active-nodes active-edges))))))

(defrecord TermGraphDTree [argv graph ns-vars es-vars root]
  protocols/IInclude
  (mzn-includes [_self] #{"tree.mzn"})
  protocols/IExpress
  (write [_self] (list 'dtree (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self}]
            (repeat (+ (:n-nodes graph) (:n-edges graph)) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "dtree(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (translate-1idx (:root self)) ", "
           (ns-str (:ns-vars self)) ", " (es-str (:es-vars self)) ")")))
  (evaluate [self solution]
    (when-let [{:keys [active-nodes active-edges]} (eval-subgraph graph ns-vars es-vars solution)]
      (let [root-val (api/eval-arg root solution)
            adj (subgraph-adj active-edges)
            reached (dfs-reachable adj root-val)
            n-nodes (count active-nodes)
            n-edges (count active-edges)]
        (and (contains? active-nodes root-val)
             (= n-edges (dec n-nodes))
             (every? reached active-nodes))))))

;; --- weighted_spanning_tree.mzn (no ns — all nodes participate) ---

(defrecord TermGraphWeightedSpanningTree [argv graph es-vars cost]
  protocols/IInclude
  (mzn-includes [_self] #{"weighted_spanning_tree.mzn"})
  protocols/IExpress
  (write [_self] (list 'weighted-spanning-tree (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self}]
            (repeat (:n-edges graph) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "weighted_spanning_tree(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (graph-weight-str g) ", "
           (es-str (:es-vars self)) ", "
           (protocols/translate (:cost self)) ")")))
  (evaluate [self solution]
    (let [nodes (:nodes graph)
          active-es (into #{}
                          (for [i (range (count es-vars))
                                :when (true? (api/eval-arg (nth es-vars i) solution))]
                            (let [e (nth (:edges graph) i)]
                              [(first e) (second e)])))
          cost-val (api/eval-arg cost solution)
          edge-weights (into {} (map (fn [e] [[(first e) (second e)] (nth e 2)]) (:edges graph)))
          total (reduce + (map #(get edge-weights % 0) active-es))
          n-edges (count active-es)]
      (and (= n-edges (dec (count nodes)))
           (undirected-connected? nodes active-es)
           (= cost-val total)))))

(defrecord TermGraphDWeightedSpanningTree [argv graph es-vars root cost]
  protocols/IInclude
  (mzn-includes [_self] #{"weighted_spanning_tree.mzn"})
  protocols/IExpress
  (write [_self] (list 'd-weighted-spanning-tree (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self]
    (concat [{types/Numeric self} {types/Numeric self}]
            (repeat (:n-edges graph) {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (let [g (:graph self)
          n (:n-nodes g)
          e (:n-edges g)]
      (str "d_weighted_spanning_tree(" n ", " e ", "
           (graph-from-str g) ", " (graph-to-str g) ", "
           (graph-weight-str g) ", "
           (translate-1idx (:root self)) ", "
           (es-str (:es-vars self)) ", "
           (protocols/translate (:cost self)) ")")))
  (evaluate [self solution]
    (let [nodes (:nodes graph)
          active-es (into #{}
                          (for [i (range (count es-vars))
                                :when (true? (api/eval-arg (nth es-vars i) solution))]
                            (let [e (nth (:edges graph) i)]
                              [(first e) (second e)])))
          root-val (api/eval-arg root solution)
          cost-val (api/eval-arg cost solution)
          adj (subgraph-adj active-es)
          reached (dfs-reachable adj root-val)
          edge-weights (into {} (map (fn [e] [[(first e) (second e)] (nth e 2)]) (:edges graph)))
          total (reduce + (map #(get edge-weights % 0) active-es))
          n-edges (count active-es)]
      (and (= n-edges (dec (count nodes)))
           (every? reached nodes)
           (= cost-val total)))))

;; --- circuit.mzn / subcircuit.mzn (successor-array pattern) ---

(defrecord TermGraphCircuit [argv graph succ]
  protocols/IInclude
  (mzn-includes [_self] #{"circuit.mzn"})
  protocols/IExpress
  (write [_self] (list 'circuit (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat (count argv) {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (str "circuit("
         (terms/to-literal-array (map #(str (protocols/translate %) " + 1") (:argv self)))
         ")"))
  (evaluate [self solution]
    (let [nodes (vec (sort (:nodes graph)))
          n (count nodes)
          succ-vals (mapv #(api/eval-arg % solution) succ)]
      ;; Single Hamiltonian cycle: follow successor from node 0, must visit all nodes
      (loop [current (first nodes) visited #{} steps 0]
        (if (>= steps n)
          (and (= steps n) (= current (first nodes)) (= (count visited) n))
          (let [idx (.indexOf nodes current)
                next-node (nth succ-vals idx)]
            (if (contains? visited current)
              false
              (recur next-node (conj visited current) (inc steps)))))))))

(defrecord TermGraphSubCircuit [argv graph succ]
  protocols/IInclude
  (mzn-includes [_self] #{"subcircuit.mzn"})
  protocols/IExpress
  (write [_self] (list 'subcircuit (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat (count argv) {types/Numeric self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self]
    (str "subcircuit("
         (terms/to-literal-array (map #(str (protocols/translate %) " + 1") (:argv self)))
         ")"))
  (evaluate [self solution]
    (let [nodes (vec (sort (:nodes graph)))
          n (count nodes)
          succ-vals (mapv #(api/eval-arg % solution) succ)
          ;; Active nodes are those where succ[i] != node[i]
          active (into #{} (for [i (range n)
                                 :when (not= (nth nodes i) (nth succ-vals i))]
                             (nth nodes i)))]
      (if (empty? active)
        true  ;; no active nodes = valid subcircuit
        ;; Each active node's successor must be active, and following successors forms a single cycle
        (and (every? (fn [node]
                       (let [idx (.indexOf nodes node)]
                         (contains? active (nth succ-vals idx))))
                     active)
             ;; Follow from any active node — must return to start after visiting all active
             (let [start (first active)]
               (loop [current start visited #{} steps 0]
                 (if (>= steps (count active))
                   (and (= current start) (= (count visited) (count active)))
                   (let [idx (.indexOf nodes current)
                         next-node (nth succ-vals idx)]
                     (if (contains? visited current)
                       false
                       (recur next-node (conj visited current) (inc steps))))))))))))

;; ============================================================
;; Constructor functions
;; ============================================================

(defn subgraph
  "Constrain a subgraph of g. Returns a handle with auto-created ns/es vars."
  [g]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphSubgraph argv g ns-vars es-vars))))

(defn reachable
  "Constrain reachable subgraph from root in undirected graph g."
  [g root]
  {:pre [(instance? Digraph g)]}
  (if (and (terms/ground? root))
    (let [adj (undirected-adjacency-list g)
          reached (bfs-reachable adj root)]
      (= (:nodes g) (into (sorted-set) reached)))
    (let [{:keys [ns-vars es-vars]} (make-ns-es g)
          argv (vec (concat [root] ns-vars es-vars))]
      (api/cacheing-validate
       (->TermGraphReachable argv g ns-vars es-vars root)))))

(defn dreachable
  "Constrain reachable subgraph from root in directed graph g."
  [g root]
  {:pre [(instance? Digraph g)]}
  (if (terms/ground? root)
    (let [adj (adjacency-list g)
          reached (dfs-reachable adj root)]
      (= (:nodes g) (into (sorted-set) reached)))
    (let [{:keys [ns-vars es-vars]} (make-ns-es g)
          argv (vec (concat [root] ns-vars es-vars))]
      (api/cacheing-validate
       (->TermGraphDReachable argv g ns-vars es-vars root)))))

(defn connected
  "Constrain that the selected subgraph is connected (undirected)."
  [g]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphConnected argv g ns-vars es-vars))))

(defn dconnected
  "Constrain that the selected subgraph is strongly connected (directed)."
  [g]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphDConnected argv g ns-vars es-vars))))

(defn dag
  "Constrain that the selected subgraph is a DAG."
  [g]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphDag argv g ns-vars es-vars))))

(defn path
  "Constrain an undirected path from source to target in g.
   Always returns a handle (never constant-folds) so that ns/es vars
   are available for solution reading via active-nodes/active-edges."
  [g source target]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat [source target] ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphPath argv g ns-vars es-vars source target))))

(defn dpath
  "Constrain a directed path from source to target in g.
   Always returns a handle (never constant-folds) so that ns/es vars
   are available for solution reading via active-nodes/active-edges."
  [g source target]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat [source target] ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphDPath argv g ns-vars es-vars source target))))

(defn bounded-path
  "Constrain a bounded undirected path from source to target in weighted g.
   cost is a decision variable that will be bound to the path cost."
  [g source target cost]
  {:pre [(instance? Digraph g)
         (some? (:weights g))]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat [source target cost] ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphBoundedPath argv g ns-vars es-vars source target cost))))

(defn bounded-dpath
  "Constrain a bounded directed path from source to target in weighted g.
   cost is a decision variable that will be bound to the path cost."
  [g source target cost]
  {:pre [(instance? Digraph g)
         (some? (:weights g))]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat [source target cost] ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphBoundedDPath argv g ns-vars es-vars source target cost))))

(defn tree
  "Constrain a spanning tree rooted at root in undirected g."
  [g root]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat [root] ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphTree argv g ns-vars es-vars root))))

(defn dtree
  "Constrain a directed spanning tree (arborescence) rooted at root in g."
  [g root]
  {:pre [(instance? Digraph g)]}
  (let [{:keys [ns-vars es-vars]} (make-ns-es g)
        argv (vec (concat [root] ns-vars es-vars))]
    (api/cacheing-validate
     (->TermGraphDTree argv g ns-vars es-vars root))))

(defn weighted-spanning-tree
  "Constrain a minimum spanning tree in weighted g. cost is bound to total weight."
  [g cost]
  {:pre [(instance? Digraph g)
         (some? (:weights g))]}
  (let [{:keys [es-vars]} (make-es g)
        argv (vec (concat [cost] es-vars))]
    (api/cacheing-validate
     (->TermGraphWeightedSpanningTree argv g es-vars cost))))

(defn d-weighted-spanning-tree
  "Constrain a directed minimum spanning tree rooted at root in weighted g."
  [g root cost]
  {:pre [(instance? Digraph g)
         (some? (:weights g))]}
  (let [{:keys [es-vars]} (make-es g)
        argv (vec (concat [root cost] es-vars))]
    (api/cacheing-validate
     (->TermGraphDWeightedSpanningTree argv g es-vars root cost))))

(defn circuit
  "Constrain a Hamiltonian cycle on g. Creates successor variables with domains
   restricted to out-neighbors."
  [g]
  {:pre [(instance? Digraph g)]}
  (let [adj (adjacency-list g)
        nodes (vec (sort (:nodes g)))
        n (:n-nodes g)
        succ (vec (for [node nodes]
                    (let [out-neighbors (set (get adj node []))]
                      (api/fresh-int out-neighbors))))
        argv (vec succ)]
    (api/cacheing-validate
     (->TermGraphCircuit argv g succ))))

(defn subcircuit
  "Constrain a sub-Hamiltonian cycle on g. Creates successor variables where
   each node can map to itself (non-participant) or an out-neighbor."
  [g]
  {:pre [(instance? Digraph g)]}
  (let [adj (adjacency-list g)
        nodes (vec (sort (:nodes g)))
        succ (vec (for [node nodes]
                    (let [out-neighbors (set (get adj node []))
                          domain (conj out-neighbors node)]
                      (api/fresh-int domain))))
        argv (vec succ)]
    (api/cacheing-validate
     (->TermGraphSubCircuit argv g succ))))

;; ============================================================
;; Solution readers
;; ============================================================

(defn active-nodes
  "Returns set of 0-indexed node IDs where ns[i] = true in the solution.
   For handles without ns-vars (spanning trees), returns all graph nodes.
   For circuit/subcircuit, returns nodes where succ[i] != i."
  [handle solution]
  (cond
    ;; Circuit: active = all nodes (Hamiltonian)
    (instance? TermGraphCircuit handle)
    (:nodes (:graph handle))

    ;; SubCircuit: active where succ[i] != i
    (instance? TermGraphSubCircuit handle)
    (let [nodes (vec (sort (:nodes (:graph handle))))
          succ-vals (mapv #(get solution %) (:succ handle))]
      (into (sorted-set)
            (filter (fn [i] (not= (nth nodes i) (nth succ-vals i)))
                    (range (count nodes)))))

    ;; Spanning tree variants (no ns-vars)
    (or (instance? TermGraphWeightedSpanningTree handle)
        (instance? TermGraphDWeightedSpanningTree handle))
    (:nodes (:graph handle))

    ;; All other handles have ns-vars
    :else
    (let [ns-vars (:ns-vars handle)
          nodes (vec (sort (:nodes (:graph handle))))]
      (into (sorted-set)
            (for [i (range (count ns-vars))
                  :when (true? (get solution (nth ns-vars i)))]
              (nth nodes i))))))

(defn active-edges
  "Returns set of [from to] pairs where es[i] = true in the solution.
   For circuit/subcircuit, returns edges from the successor assignment."
  [handle solution]
  (cond
    ;; Circuit/SubCircuit: reconstruct edges from successor vars
    (or (instance? TermGraphCircuit handle)
        (instance? TermGraphSubCircuit handle))
    (let [nodes (vec (sort (:nodes (:graph handle))))
          succ-vals (mapv #(get solution %) (:succ handle))]
      (into #{}
            (for [i (range (count nodes))
                  :let [from-node (nth nodes i)
                        to-node (nth succ-vals i)]
                  :when (not= from-node to-node)]
              [from-node to-node])))

    ;; All other handles have es-vars
    :else
    (let [es-vars (:es-vars handle)
          edges (:edges (:graph handle))]
      (into #{}
            (for [i (range (count es-vars))
                  :when (true? (get solution (nth es-vars i)))]
              (let [e (nth edges i)]
                [(first e) (second e)]))))))
