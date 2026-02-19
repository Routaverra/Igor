(ns igor.graph-test
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.graph :as graph]))

;; ============================================================
;; Shared test data: neo-Riemannian infrastructure
;; ============================================================

(defn- rising-neighbors
  "Returns the vector of triad IDs reachable via rising RPL transforms from triad `id`."
  [id]
  (if (< id 12)
    [(+ 12 (mod (+ id 9) 12))]
    (let [r (- id 12)]
      [r (mod (+ r 8) 12)])))

(def ^:private edge-weight
  (vec (for [id (range 24)]
         (if (< id 12) [2] [1 1]))))

(def ^:private rising-graph
  "Rising-only digraph: 24 triads, 36 edges, weighted."
  (graph/digraph 24 (for [id (range 24)
                          [j nbr] (map-indexed vector (rising-neighbors id))]
                      [id nbr (nth (nth edge-weight id) j)])))

;; Small synthetic graphs for testing
(def ^:private small-bidir-graph
  "4-node bidirectional graph: 0-1, 1-2, 2-3, 0-3"
  (graph/digraph [[0 1] [1 0] [1 2] [2 1] [2 3] [3 2] [0 3] [3 0]]))

(def ^:private small-acyclic-graph
  "4-node DAG: 0->1, 0->2, 1->3, 2->3"
  (graph/digraph [[0 1] [0 2] [1 3] [2 3]]))

(def ^:private small-weighted-bidir-graph
  "4-node bidirectional weighted graph"
  (graph/digraph [[0 1 2] [1 0 2] [1 2 3] [2 1 3] [2 3 1] [3 2 1] [0 3 5] [3 0 5]]))

(def ^:private small-weighted-graph
  "4-node directed weighted graph: 0->1(2), 0->2(4), 1->2(1), 1->3(7), 2->3(3)"
  (graph/digraph [[0 1 2] [0 2 4] [1 2 1] [1 3 7] [2 3 3]]))

;; ============================================================
;; Construction tests
;; ============================================================

(deftest digraph-construction-test
  (testing "Digraph correctly constructed from edge list"
    (let [g (graph/digraph [[0 1] [1 2] [2 0]])]
      (is (= #{0 1 2} (:nodes g)))
      (is (= 3 (:n-nodes g)))
      (is (= 3 (:n-edges g)))
      (is (= [0 1 2] (:from-arr g)))
      (is (= [1 2 0] (:to-arr g)))
      (is (nil? (:weights g)))))

  (testing "Weighted graph construction"
    (let [g (graph/digraph [[0 1 5] [1 2 3]])]
      (is (= [5 3] (:weights g)))
      (is (= 2 (:n-edges g)))))

  (testing "Explicit n-nodes for isolated nodes"
    (let [g (graph/digraph 5 [[0 1] [1 2]])]
      (is (= 5 (:n-nodes g)))
      (is (>= (count (:nodes g)) 5)))))

;; ============================================================
;; Path constraints
;; ============================================================

(deftest graph-dpath-test
  (testing "directed path from 0 to 6 (C major to F# major) in rising graph"
    (let [handle (i/dpath rising-graph 0 6)
          solution (i/satisfy handle)
          nodes (i/active-nodes handle solution)
          edges (i/active-edges handle solution)]
      (is (contains? nodes 0))
      (is (contains? nodes 6))
      (is (pos? (count edges)))
      ;; all active edges are valid rising edges
      (doseq [[u v] edges]
        (is (some #{v} (rising-neighbors u)))))))

(deftest graph-dreachable-ground-test-reverse
  (testing "ground pass-through: dreachable returns boolean"
    ;; 0 can reach all 24 triads via rising edges
    (is (true? (i/dreachable rising-graph 0)))
    ;; reachable on bidirectional graph
    (is (true? (i/reachable small-bidir-graph 0)))))

(deftest graph-path-test
  (testing "undirected path in bidirectional graph"
    (let [handle (i/path small-bidir-graph 0 3)
          solution (i/satisfy handle)
          nodes (i/active-nodes handle solution)
          edges (i/active-edges handle solution)]
      (is (contains? nodes 0))
      (is (contains? nodes 3))
      (is (pos? (count edges))))))

(deftest graph-bounded-dpath-test
  (testing "bounded directed path with cost minimization"
    (let [cost (i/fresh-int (range 1000) "cost")
          handle (i/bounded-dpath rising-graph 0 6 cost)
          solution (i/maximize (i/- 0 cost) handle)
          nodes (i/active-nodes handle solution)
          cost-val (get solution cost)]
      (is (contains? nodes 0))
      (is (contains? nodes 6))
      (is (pos? cost-val)))))

(deftest graph-bounded-path-test
  (testing "bounded undirected path with cost"
    (let [cost (i/fresh-int (range 100) "cost")
          handle (i/bounded-path small-weighted-bidir-graph 0 3 cost)
          solution (i/satisfy handle)
          cost-val (get solution cost)]
      (is (pos? cost-val)))))

;; ============================================================
;; Reachability constraints
;; ============================================================

(deftest graph-dreachable-test
  (testing "all 24 triads reachable from C major (0) via directed rising edges"
    ;; ground pass-through: should return true
    (is (true? (i/dreachable rising-graph 0)))))

(deftest graph-reachable-test
  (testing "all nodes reachable in bidirectional graph"
    (is (true? (i/reachable small-bidir-graph 0)))))

;; ============================================================
;; Connectivity constraints
;; ============================================================

(deftest graph-connected-test
  (testing "connected subgraph selection"
    (let [handle (i/connected small-bidir-graph)
          solution (i/satisfy handle)
          nodes (i/active-nodes handle solution)
          edges (i/active-edges handle solution)]
      ;; should select some connected subgraph
      (is (pos? (count nodes))))))

;; ============================================================
;; DAG constraint
;; ============================================================

(deftest graph-dag-test
  (testing "DAG constraint on acyclic graph with forced nodes"
    (let [handle (i/dag small-acyclic-graph)
          ;; Force all nodes active
          ns-vars (:ns-vars handle)
          constraint (apply i/and handle
                            (map #(i/= % true) ns-vars))
          solution (i/satisfy constraint)
          nodes (i/active-nodes handle solution)]
      (is (= #{0 1 2 3} nodes)))))

;; ============================================================
;; Tree constraints
;; ============================================================

(deftest graph-tree-test
  (testing "spanning tree rooted at 0 in bidirectional graph"
    (let [handle (i/tree small-bidir-graph 0)
          solution (i/satisfy handle)
          nodes (i/active-nodes handle solution)
          edges (i/active-edges handle solution)]
      (is (contains? nodes 0))
      ;; a tree on n nodes has n-1 edges
      (is (= (dec (count nodes)) (count edges))))))

(deftest graph-dtree-test
  (testing "directed spanning tree (arborescence) from 0 in acyclic graph"
    (let [handle (i/dtree small-acyclic-graph 0)
          solution (i/satisfy handle)
          nodes (i/active-nodes handle solution)
          edges (i/active-edges handle solution)]
      (is (contains? nodes 0))
      (is (= (dec (count nodes)) (count edges))))))

;; ============================================================
;; Subgraph constraint
;; ============================================================

(deftest graph-subgraph-test
  (testing "subgraph with forced specific nodes"
    (let [handle (i/subgraph small-acyclic-graph)
          ;; Force nodes 0 and 3 to be active
          ns-vars (:ns-vars handle)
          constraint (i/and handle
                            (i/= (nth ns-vars 0) true)
                            (i/= (nth ns-vars 3) true))
          solution (i/satisfy constraint)
          nodes (i/active-nodes handle solution)]
      (is (contains? nodes 0))
      (is (contains? nodes 3)))))

;; ============================================================
;; Weighted spanning tree constraints
;; ============================================================

(deftest graph-weighted-spanning-tree-test
  (testing "minimum spanning tree in weighted bidirectional graph"
    (let [cost (i/fresh-int (range 1000) "cost")
          handle (i/weighted-spanning-tree small-weighted-bidir-graph cost)
          solution (i/maximize (i/- 0 cost) handle)
          cost-val (get solution cost)
          edges (i/active-edges handle solution)]
      (is (pos? cost-val))
      ;; spanning tree on 4 nodes = 3 edges
      (is (= 3 (count edges))))))

(deftest graph-d-weighted-spanning-tree-test
  (testing "directed weighted spanning tree from root"
    (let [cost (i/fresh-int (range 1000) "cost")
          handle (i/d-weighted-spanning-tree small-weighted-graph 0 cost)
          solution (i/satisfy handle)
          cost-val (get solution cost)
          edges (i/active-edges handle solution)]
      (is (pos? cost-val))
      ;; spanning tree on 4 nodes = 3 edges
      (is (= 3 (count edges))))))

;; ============================================================
;; Circuit constraints
;; ============================================================

(deftest graph-circuit-test
  (testing "Hamiltonian cycle on complete 5-node graph"
    (let [;; Complete graph K5: all pairs
          g (graph/digraph 5 (for [i (range 5) j (range 5) :when (not= i j)] [i j]))
          handle (i/circuit g)
          solution (i/satisfy handle)
          nodes (i/active-nodes handle solution)
          edges (i/active-edges handle solution)]
      ;; All 5 nodes visited
      (is (= #{0 1 2 3 4} nodes))
      ;; 5 edges in a Hamiltonian cycle
      (is (= 5 (count edges))))))

(deftest graph-subcircuit-test
  (testing "subcircuit on complete 5-node graph"
    (let [g (graph/digraph 5 (for [i (range 5) j (range 5) :when (not= i j)] [i j]))
          handle (i/subcircuit g)
          solution (i/satisfy handle)
          edges (i/active-edges handle solution)]
      ;; At least some edges selected (could be full or partial circuit)
      (is (pos? (count edges))))))

;; ============================================================
;; Neo-Riemannian tests using graph API
;; ============================================================

(deftest graph-neo-riemannian-dpath-test
  (testing "rising path from C major to F# major, reconstruct from active edges"
    (let [handle (i/dpath rising-graph 0 6)
          solution (i/satisfy handle)
          edges (i/active-edges handle solution)
          nodes (i/active-nodes handle solution)]
      ;; Source and target in path
      (is (contains? nodes 0))
      (is (contains? nodes 6))
      ;; Reconstruct path
      (let [adj (reduce (fn [m [u v]] (assoc m u v)) {} edges)
            path (loop [node 0 acc [0]]
                   (if (= node 6)
                     acc
                     (let [next-node (get adj node)]
                       (recur next-node (conj acc next-node)))))]
        (is (= 0 (first path)))
        (is (= 6 (last path)))
        ;; all edges are valid rising edges
        (doseq [i (range (dec (count path)))]
          (is (some #{(nth path (inc i))} (rising-neighbors (nth path i)))))))))

(deftest graph-neo-riemannian-bounded-dpath-test
  (testing "shortest rising path with weight minimization"
    (let [cost (i/fresh-int (range 1000) "cost")
          handle (i/bounded-dpath rising-graph 0 6 cost)
          solution (i/maximize (i/- 0 cost) handle)
          cost-val (get solution cost)
          nodes (i/active-nodes handle solution)]
      (is (contains? nodes 0))
      (is (contains? nodes 6))
      (is (pos? cost-val)))))
