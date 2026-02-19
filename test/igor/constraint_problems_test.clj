(ns igor.constraint-problems-test
  "Tests based on classic constraint satisfaction problems,
   inspired by core.logic CLP(FD) patterns and standard CSP benchmarks."
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.utils.test :refer [only-val throws?]]))

;; ============================================================
;; 1. Classic arithmetic CSPs
;; ============================================================

(deftest send-more-money-test
  (testing "SEND + MORE = MONEY cryptarithmetic puzzle"
    (let [digits-domain (range 10)
          s (i/fresh-int digits-domain "S") e (i/fresh-int digits-domain "E")
          n (i/fresh-int digits-domain "N") d (i/fresh-int digits-domain "D")
          m (i/fresh-int digits-domain "M") o (i/fresh-int digits-domain "O")
          r (i/fresh-int digits-domain "R") y (i/fresh-int digits-domain "Y")
          digits [s e n d m o r y]
          send  (i/+ (i/* s 1000) (i/* e 100) (i/* n 10) d)
          more  (i/+ (i/* m 1000) (i/* o 100) (i/* r 10) e)
          money (i/+ (i/* m 10000) (i/* o 1000) (i/* n 100) (i/* e 10) y)
          ;; all-different via pairwise not=
          all-diff (->> (for [i (range (count digits))
                              j (range (inc i) (count digits))]
                          (i/not= (nth digits i) (nth digits j)))
                        (apply i/conjunction))
          constraint (i/and
                      all-diff
                      (i/= (i/+ send more) money)
                      (i/> s 0)   ;; leading digits nonzero
                      (i/> m 0))
          solution (i/satisfy constraint)]
      ;; Verify the arithmetic identity holds
      (let [s* (get solution s) e* (get solution e) n* (get solution n) d* (get solution d)
            m* (get solution m) o* (get solution o) r* (get solution r) y* (get solution y)
            send* (+ (* s* 1000) (* e* 100) (* n* 10) d*)
            more* (+ (* m* 1000) (* o* 100) (* r* 10) e*)
            money* (+ (* m* 10000) (* o* 1000) (* n* 100) (* e* 10) y*)]
        (is (= (+ send* more*) money*))
        (is (pos? s*))
        (is (pos? m*))
        (is (= 8 (count (distinct [s* e* n* d* m* o* r* y*]))))))))

(deftest magic-square-3x3-test
  (testing "3x3 magic square"
    (let [cell-domain (range 1 10)
          cells (vec (repeatedly 9 #(i/fresh-int cell-domain)))
          [a b c d e f g h k] cells
          magic-sum (i/fresh-int (range 1 46) "magic")
          ;; all different
          all-diff (->> (for [i (range 9)
                              j (range (inc i) 9)]
                          (i/not= (nth cells i) (nth cells j)))
                        (apply i/conjunction))
          ;; rows, cols, diags all equal magic-sum
          sums (i/and
                (i/= magic-sum (i/+ a b c))
                (i/= magic-sum (i/+ d e f))
                (i/= magic-sum (i/+ g h k))
                (i/= magic-sum (i/+ a d g))
                (i/= magic-sum (i/+ b e h))
                (i/= magic-sum (i/+ c f k))
                (i/= magic-sum (i/+ a e k))
                (i/= magic-sum (i/+ c e g)))
          solution (i/satisfy (i/and all-diff sums))
          vals* (mapv solution cells)
          ms (get solution magic-sum)]
      ;; magic constant for 3x3 is always 15
      (is (= 15 ms))
      ;; all values distinct and in 1-9
      (is (= 9 (count (distinct vals*))))
      (is (every? #(<= 1 % 9) vals*))
      ;; verify row sums
      (is (= ms (apply + (subvec vals* 0 3))))
      (is (= ms (apply + (subvec vals* 3 6))))
      (is (= ms (apply + (subvec vals* 6 9)))))))

;; ============================================================
;; 2. Disjunction and or
;; ============================================================

(deftest disjunction-test
  (testing "or selects one of multiple alternatives"
    (let [x (i/fresh-int (range 50))]
      (is (contains?
           #{3 7}
           (get (i/satisfy (i/or (i/= x 3) (i/= x 7))) x)))))

  (testing "disjunction of many constraints"
    (let [x (i/fresh-int (range 50))
          constraint (apply i/disjunction
                            (map (fn [v] (i/= x v)) [10 20 30]))]
      (is (contains? #{10 20 30}
                     (get (i/satisfy constraint) x))))))

(deftest or-with-arithmetic-test
  (testing "or combined with arithmetic constraints"
    (let [x (i/fresh-int (range 50))
          y (i/fresh-int (range 50))
          solution (i/satisfy
                    (i/and
                     (i/or (i/= x 5) (i/= x 10))
                     (i/= y (i/* x 2))))]
      (is (= (* 2 (get solution x)) (get solution y)))
      (is (contains? #{5 10} (get solution x))))))

;; ============================================================
;; 3. Optimization (maximize)
;; ============================================================

(deftest maximize-basic-test
  (testing "maximize a bounded variable"
    (let [x (i/fresh-int (range 101))
          solution (i/maximize x (i/>= x 0))]
      (is (= 100 (get solution x))))))

(deftest maximize-with-constraints-test
  (testing "maximize subject to multiple constraints"
    (let [x (i/fresh-int (range 11))
          y (i/fresh-int (range 11))
          ;; maximize x+y where x,y in [0,10] and x+y <= 15
          constraint (i/<= (i/+ x y) 15)
          solution (i/maximize (i/+ x y) constraint)]
      (is (= 15 (+ (get solution x) (get solution y)))))))

(deftest maximize-knapsack-test
  (testing "simple 0-1 knapsack via maximize"
    ;; Items: weight/value pairs: (3,4), (4,5), (2,3), (5,7)
    ;; Capacity: 7
    (let [binary-domain (range 2)
          x0 (i/fresh-int binary-domain "x0") x1 (i/fresh-int binary-domain "x1")
          x2 (i/fresh-int binary-domain "x2") x3 (i/fresh-int binary-domain "x3")
          weight (i/+ (i/* 3 x0) (i/* 4 x1) (i/* 2 x2) (i/* 5 x3))
          value  (i/+ (i/* 4 x0) (i/* 5 x1) (i/* 3 x2) (i/* 7 x3))
          constraint (i/<= weight 7)
          solution (i/maximize value constraint)
          total-weight (+ (* 3 (get solution x0)) (* 4 (get solution x1))
                          (* 2 (get solution x2)) (* 5 (get solution x3)))
          total-value  (+ (* 4 (get solution x0)) (* 5 (get solution x1))
                          (* 3 (get solution x2)) (* 7 (get solution x3)))]
      (is (<= total-weight 7))
      ;; Optimal: x2=1, x3=1 -> w=7, v=10
      (is (= 10 total-value)))))

;; ============================================================
;; 4. N-Queens (classic CSP)
;; ============================================================

(deftest four-queens-test
  (testing "4-queens: place 4 non-attacking queens on 4x4 board"
    (let [n 4
          col-domain (range n)
          ;; queens[i] = column of queen in row i
          queens (vec (repeatedly n #(i/fresh-int col-domain)))
          ;; all columns different
          col-diff (->> (for [i (range n)
                              j (range (inc i) n)]
                          (i/not= (nth queens i) (nth queens j)))
                        (apply i/conjunction))
          ;; no two queens on same diagonal
          diag-diff (->> (for [i (range n)
                               j (range (inc i) n)
                               :let [di (- j i)]]
                           (i/and
                            (i/not= (i/+ (nth queens i) di) (nth queens j))
                            (i/not= (i/- (nth queens i) di) (nth queens j))))
                         (apply i/conjunction))
          solution (i/satisfy (i/and col-diff diag-diff))
          cols (mapv solution queens)]
      ;; verify: all columns distinct
      (is (= n (count (distinct cols))))
      ;; verify: no diagonal attacks
      (is (every? true?
                  (for [i (range n) j (range (inc i) n)]
                    (not= (Math/abs (- (nth cols i) (nth cols j)))
                           (- j i))))))))

;; ============================================================
;; 5. Set constraint problems
;; ============================================================

(deftest set-partition-test
  (testing "partition a set into two disjoint subsets with equal cardinality"
    (let [domain (range 6)
          a (i/fresh-set domain)
          b (i/fresh-set domain)
          constraint (i/and
                      (i/= (i/union a b) (set domain))
                      (i/= (i/count (i/intersection a b)) 0)
                      (i/= (i/count a) 3)
                      (i/= (i/count b) 3))
          solution (i/satisfy constraint)
          a* (get solution a)
          b* (get solution b)]
      (is (= (set domain) (clojure.set/union a* b*)))
      (is (empty? (clojure.set/intersection a* b*)))
      (is (= 3 (count a*)))
      (is (= 3 (count b*))))))

(deftest set-covering-test
  (testing "find minimal subset containing required elements"
    (let [domain (range 10)
          s (i/fresh-set domain)
          ;; must contain 1, 3, 5, 7
          constraint (i/and
                      (i/contains? s 1)
                      (i/contains? s 3)
                      (i/contains? s 5)
                      (i/contains? s 7)
                      (i/= (i/count s) 4))
          solution (i/satisfy constraint)
          s* (get solution s)]
      (is (= 4 (count s*)))
      (is (every? s* [1 3 5 7])))))

(deftest set-subset-chain-test
  (testing "chain of subsets: a subset b subset c"
    (let [domain (range 8)
          a (i/fresh-set domain)
          b (i/fresh-set domain)
          c (i/fresh-set domain)
          constraint (i/and
                      (i/= (i/count a) 2)
                      (i/= (i/count b) 4)
                      (i/= (i/count c) 6)
                      (i/subset? a b)
                      (i/subset? b c))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)]
      (is (= 2 (count a*)))
      (is (= 4 (count b*)))
      (is (= 6 (count c*)))
      (is (clojure.set/subset? a* b*))
      (is (clojure.set/subset? b* c*)))))

;; ============================================================
;; 6. Quantifier-heavy problems
;; ============================================================

(deftest forall-all-elements-divisible-test
  (testing "forall: every element in set divisible by 3"
    (let [s (i/fresh-set (range 30))
          constraint (i/and
                      (i/= (i/count s) 4)
                      (i/forall (i/bind (range 30) s)
                        (fn [elem]
                          (i/= 0 (i/mod elem 3)))))
          solution (i/satisfy constraint)
          s* (get solution s)]
      (is (= 4 (count s*)))
      (is (every? #(zero? (mod % 3)) s*)))))

(deftest for-set-image-test
  (testing "for-set: compute image of a function over a set"
    (let [s (i/fresh-set (range 10))
          ;; image = {x+1 : x in s}
          image (i/for-set (i/bind (range 10) s)
                  (fn [x] (i/+ x 1)))
          constraint (i/and
                      (i/= s #{2 4 6})
                      (i/= #{3 5 7} image))
          solution (i/satisfy constraint)]
      (is (= #{2 4 6} (get solution s))))))

;; ============================================================
;; 7. Multi-variable arithmetic constraint propagation
;; ============================================================

(deftest simultaneous-equations-test
  (testing "system of linear equations"
    ;; x + y = 10, x - y = 2  => x=6, y=4
    (let [x (i/fresh-int (range 101))
          y (i/fresh-int (range 101))
          solution (i/satisfy
                    (i/and
                     (i/= (i/+ x y) 10)
                     (i/= (i/- x y) 2)))]
      (is (= 6 (get solution x)))
      (is (= 4 (get solution y))))))

(deftest transitive-equality-test
  (testing "equality chains propagate: a=b, b=c => a=c"
    (let [a (i/fresh-int (range 101))
          b (i/fresh-int (range 101))
          c (i/fresh-int (range 101))
          solution (i/satisfy
                    (i/and
                     (i/= a 42)
                     (i/= a b)
                     (i/= b c)))]
      (is (= 42 (get solution a)))
      (is (= 42 (get solution b)))
      (is (= 42 (get solution c))))))

(deftest abs-value-via-cond-test
  (testing "absolute value expressed via cond"
    (let [x (i/fresh-int (range -100 101))
          abs-x (i/fresh-int (range 101))
          solution (i/satisfy
                    (i/and
                     (i/= x -7)
                     (i/= abs-x (i/cond
                                  (i/>= x 0) x
                                  :else (i/- 0 x)))))]
      (is (= -7 (get solution x)))
      (is (= 7 (get solution abs-x))))))

;; ============================================================
;; 8. Bounded search with multiple constraints
;; ============================================================

(deftest pythagorean-triple-test
  (testing "find a Pythagorean triple a^2 + b^2 = c^2"
    (let [a (i/fresh-int (range 1 21))
          b (i/fresh-int (range 1 21))
          c (i/fresh-int (range 1 31))
          constraint (i/and
                      (i/>= b a)     ;; b >= a to avoid duplicates
                      (i/>= c b)
                      (i/= (i/+ (i/* a a) (i/* b b)) (i/* c c)))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)]
      (is (= (+ (* a* a*) (* b* b*)) (* c* c*)))
      (is (<= 1 a* b* c*)))))

(deftest sum-of-consecutive-test
  (testing "find n consecutive integers that sum to target"
    ;; n consecutive starting from x: x + (x+1) + (x+2) = 12 => x=3
    (let [x (i/fresh-int (range 100))
          constraint (i/= (i/+ x (i/+ x 1) (i/+ x 2)) 12)]
      (is (= 3 (only-val (i/satisfy constraint)))))))

;; ============================================================
;; 9. Boolean constraint combinations
;; ============================================================

(deftest boolean-circuit-test
  (testing "boolean satisfiability: (a OR b) AND (NOT a OR c) AND (NOT b OR NOT c)"
    (let [binary (range 2)
          a (i/fresh-int binary) b (i/fresh-int binary) c (i/fresh-int binary)
          constraint (i/and
                      (i/or (i/= a 1) (i/= b 1))
                      (i/or (i/= a 0) (i/= c 1))
                      (i/or (i/= b 0) (i/= c 0)))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)]
      ;; verify all clauses
      (is (or (= 1 a*) (= 1 b*)))
      (is (or (= 0 a*) (= 1 c*)))
      (is (or (= 0 b*) (= 0 c*))))))

;; ============================================================
;; 10. Music-theory flavored constraints
;; ============================================================

(deftest interval-class-vector-test
  (testing "find a pitch class set with distinct pairwise intervals"
    ;; Find 3 notes from 0-11 where all pairwise intervals are different
    ;; (an all-interval trichord). Use integer variables instead of sets
    ;; to avoid deeply nested forall.
    (let [pc-domain (range 12)
          a (i/fresh-int pc-domain "pc0")
          b (i/fresh-int pc-domain "pc1")
          c (i/fresh-int pc-domain "pc2")
          constraint (i/and
                      ;; ordered to avoid symmetry
                      (i/< a b) (i/< b c)
                      ;; all 3 pairwise intervals must be distinct
                      (i/not= (i/- b a) (i/- c b))
                      (i/not= (i/- b a) (i/- c a))
                      (i/not= (i/- c b) (i/- c a)))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)
          intervals [(- b* a*) (- c* b*) (- c* a*)]]
      (is (< a* b* c*))
      (is (every? #(<= 0 % 11) [a* b* c*]))
      ;; all pairwise intervals distinct
      (is (= 3 (count (distinct intervals)))))))

(deftest voice-leading-smoothness-test
  (testing "minimize total voice movement between two chords"
    ;; Given chord 1 = [0, 4, 7] (C major)
    ;; Find assignment of C minor {0, 3, 7} to 3 voices minimizing total movement.
    ;; Use auxiliary variables for absolute differences to keep MiniZinc simple.
    (let [voice-domain (range 12)
          v1 (i/fresh-int voice-domain "v1")
          v2 (i/fresh-int voice-domain "v2")
          v3 (i/fresh-int voice-domain "v3")
          d1 (i/fresh-int (range 12) "d1")
          d2 (i/fresh-int (range 12) "d2")
          d3 (i/fresh-int (range 12) "d3")
          constraint (i/and
                      ;; chord 2 is C minor: {0, 3, 7}
                      (i/or (i/= v1 0) (i/= v1 3) (i/= v1 7))
                      (i/or (i/= v2 0) (i/= v2 3) (i/= v2 7))
                      (i/or (i/= v3 0) (i/= v3 3) (i/= v3 7))
                      ;; all different
                      (i/not= v1 v2)
                      (i/not= v1 v3)
                      (i/not= v2 v3)
                      ;; |v1 - 0| = d1
                      (i/>= d1 v1) (i/>= d1 (i/- 0 v1))
                      (i/or (i/= d1 v1) (i/= d1 (i/- 0 v1)))
                      ;; |v2 - 4| = d2
                      (i/>= d2 (i/- v2 4)) (i/>= d2 (i/- 4 v2))
                      (i/or (i/= d2 (i/- v2 4)) (i/= d2 (i/- 4 v2)))
                      ;; |v3 - 7| = d3
                      (i/>= d3 (i/- v3 7)) (i/>= d3 (i/- 7 v3))
                      (i/or (i/= d3 (i/- v3 7)) (i/= d3 (i/- 7 v3))))
          ;; minimize total = maximize negative total
          neg-total (i/- 0 (i/+ d1 d2 d3))
          solution (i/maximize neg-total constraint)
          v1* (get solution v1) v2* (get solution v2) v3* (get solution v3)
          total-move (+ (Math/abs (- v1* 0))
                        (Math/abs (- v2* 4))
                        (Math/abs (- v3* 7)))]
      ;; Optimal: v1=0, v2=3, v3=7 -> movement = 0+1+0 = 1
      (is (= #{0 3 7} #{v1* v2* v3*}))
      (is (= 1 total-move)))))

;; ============================================================
;; 11. Graph / Routing problems
;; ============================================================

(deftest hamiltonian-cycle-test
  (testing "find a cycle visiting all 5 nodes exactly once and returning to start"
    ;; Graph: 5 nodes (0-4), edges given as adjacency.
    ;; K5 (complete graph) so any permutation is valid.
    ;; succ[i] = the next node after node i in the cycle.
    (let [n 5
          node-domain (range n)
          succ (vec (repeatedly n #(i/fresh-int node-domain)))
          ;; all successors must be distinct (each node visited exactly once)
          all-diff (->> (for [i (range n)
                              j (range (inc i) n)]
                          (i/not= (nth succ i) (nth succ j)))
                        (apply i/conjunction))
          ;; No self-loops: succ[i] != i
          no-self (->> (for [i (range n)]
                         (i/not= (nth succ i) i))
                       (apply i/conjunction))
          ;; Follow the chain from node 0 and verify we visit all nodes
          ;; and return to 0 after exactly n steps.
          ;; pos[k] = the node at position k in the tour starting from 0.
          pos (vec (repeatedly n #(i/fresh-int node-domain)))
          chain (apply i/conjunction
                       (i/= (nth pos 0) 0) ;; start at node 0
                       ;; pos[k+1] = succ[pos[k]]
                       (for [k (range (dec n))]
                         (i/= (nth pos (inc k))
                               (i/nth (vec succ) (nth pos k)))))
          ;; succ[last node in tour] must be 0 (return to start)
          return-to-start (i/= (i/nth (vec succ) (nth pos (dec n))) 0)
          ;; all positions distinct
          pos-diff (->> (for [i (range n)
                              j (range (inc i) n)]
                          (i/not= (nth pos i) (nth pos j)))
                        (apply i/conjunction))
          solution (i/satisfy (i/and all-diff no-self chain return-to-start pos-diff))
          succ* (mapv solution succ)
          pos* (mapv solution pos)]
      ;; Verify: all successors are distinct
      (is (= n (count (distinct succ*))))
      ;; Verify: following the cycle from 0 visits all nodes
      (is (= (set (range n)) (set pos*)))
      ;; Verify: the cycle closes
      (is (= 0 (nth succ* (nth pos* (dec n))))))))

(deftest shortest-hamiltonian-cycle-test
  (testing "find the shortest cycle visiting all nodes (TSP on 4 nodes)"
    ;; 4 nodes with asymmetric distances:
    ;; cost[i][j] given as a flat lookup
    (let [n 4
          ;; Distance matrix (row i, col j):
          ;; 0: [0  10  15  20]
          ;; 1: [10  0  35  25]
          ;; 2: [15 35   0  30]
          ;; 3: [20 25  30   0]
          costs [[0 10 15 20]
                 [10 0 35 25]
                 [15 35 0 30]
                 [20 25 30 0]]
          node-domain (range n)
          cost-domain (range 101)
          ;; succ[i] = next node after i
          succ (vec (repeatedly n #(i/fresh-int node-domain)))
          ;; all successors distinct
          all-diff (->> (for [i (range n) j (range (inc i) n)]
                          (i/not= (nth succ i) (nth succ j)))
                        (apply i/conjunction))
          ;; no self-loops
          no-self (->> (for [i (range n)]
                         (i/not= (nth succ i) i))
                       (apply i/conjunction))
          ;; edge-cost[i] = cost from i to succ[i]
          edge-costs (vec (for [i (range n)]
                            (let [ec (i/fresh-int cost-domain)]
                              ;; ec = costs[i][succ[i]]
                              ;; Use i/nth to index into the cost row for node i
                              (i/= ec (i/nth (vec (nth costs i)) (nth succ i))))))
          ec-vars (vec (for [i (range n)] (i/fresh-int cost-domain)))
          edge-constraints (apply i/conjunction
                                  (for [i (range n)]
                                    (i/= (nth ec-vars i) (i/nth (vec (nth costs i)) (nth succ i)))))
          total-cost (apply i/+ ec-vars)
          ;; Chain connectivity: pos[k] = node at position k
          pos (vec (repeatedly n #(i/fresh-int node-domain)))
          chain (apply i/conjunction
                       (i/= (nth pos 0) 0)
                       (for [k (range (dec n))]
                         (i/= (nth pos (inc k))
                               (i/nth (vec succ) (nth pos k)))))
          return-to-start (i/= (i/nth (vec succ) (nth pos (dec n))) 0)
          pos-diff (->> (for [i (range n) j (range (inc i) n)]
                          (i/not= (nth pos i) (nth pos j)))
                        (apply i/conjunction))
          constraint (i/and all-diff no-self edge-constraints chain return-to-start pos-diff)
          ;; minimize total cost = maximize negative total cost
          solution (i/maximize (i/- 0 total-cost) constraint)
          succ* (mapv solution succ)
          ec* (mapv solution ec-vars)
          total* (apply + ec*)]
      ;; Optimal tour for this matrix: 0->1->3->2->0 = 10+25+30+15 = 80
      (is (= 80 total*))
      ;; Verify it's a valid cycle
      (is (= n (count (distinct succ*)))))))

(deftest graph-coloring-test
  (testing "color a 5-node graph with minimum colors, no adjacent same color"
    ;; Graph (Petersen-like subgraph):
    ;; Edges: 0-1, 0-2, 1-2, 1-3, 2-4, 3-4
    ;; This requires 3 colors.
    (let [edges [[0 1] [0 2] [1 2] [1 3] [2 4] [3 4]]
          n 5
          color-domain (range 3) ;; 3 colors: 0,1,2
          colors (vec (repeatedly n #(i/fresh-int color-domain)))
          ;; adjacent nodes must have different colors
          edge-constraints (->> edges
                                (map (fn [[u v]] (i/not= (nth colors u) (nth colors v))))
                                (apply i/conjunction))
          solution (i/satisfy edge-constraints)
          colors* (mapv solution colors)]
      ;; Verify no adjacent pair shares a color
      (is (every? (fn [[u v]] (not= (nth colors* u) (nth colors* v))) edges))
      ;; All colors in valid range
      (is (every? #(<= 0 % 2) colors*)))))

;; ============================================================
;; 12. Scheduling problems
;; ============================================================

(deftest job-sequencing-test
  (testing "sequence 4 jobs with deadlines, all must finish by their deadline"
    ;; Jobs: (processing-time, deadline)
    ;; Job 0: (2, 4)  Job 1: (1, 2)  Job 2: (3, 7)  Job 3: (1, 5)
    ;; Find ordering where each job finishes by its deadline.
    (let [n 4
          proc-times [2 1 3 1]
          deadlines  [4 2 7 5]
          pos-domain (range n)
          ;; pos[i] = position of job i in the schedule (0-indexed)
          pos (vec (repeatedly n #(i/fresh-int pos-domain)))
          ;; all positions distinct
          all-diff (->> (for [i (range n) j (range (inc i) n)]
                          (i/not= (nth pos i) (nth pos j)))
                        (apply i/conjunction))
          ;; completion time of job i = sum of processing times of all jobs
          ;; scheduled at positions <= pos[i].
          ;; We use: for each job i, the completion time is bounded by:
          ;; sum of processing times of jobs with position <= pos[i].
          ;; Simpler: start[i] = sum of proc times of jobs at positions 0..pos[i]-1.
          ;; finish[i] = start[i] + proc-time[i].
          ;; For a valid schedule, finish[i] <= deadline[i].
          ;;
          ;; We can compute this differently: for each pair (i,j),
          ;; if pos[j] < pos[i], then job j runs before job i.
          ;; start[i] = sum over j where pos[j] < pos[i] of proc-times[j]
          ;; This is complex with i/nth. Simpler formulation:
          ;; finish-at-position[k] = sum of proc times of jobs at positions 0..k
          ;; For each job i at position pos[i], finish <= deadline[i]
          ;;
          ;; Even simpler: which job is at each position?
          ;; job-at[k] = j means job j is at position k.
          job-at (vec (repeatedly n #(i/fresh-int (range n))))
          ;; job-at is inverse permutation of pos
          inverse-link (->> (for [i (range n) k (range n)]
                              ;; if pos[i] = k then job-at[k] = i
                              (i/when (i/= (nth pos i) k)
                                (i/= (nth job-at k) i)))
                            (apply i/conjunction))
          job-at-diff (->> (for [i (range n) j (range (inc i) n)]
                             (i/not= (nth job-at i) (nth job-at j)))
                           (apply i/conjunction))
          ;; finish[k] = sum of proc-times[job-at[0..k]]
          ;; Use cumulative approach: finish at position k = sum(i=0..k, proc-time[job-at[i]])
          ;; For each position k, the job there must finish by its deadline.
          ;; finish(k) = sum_{m=0}^{k} proc-time[job-at[m]]
          ;; We build incremental sums using auxiliary variables.
          finish-vars (vec (repeatedly n #(i/fresh-int (range 100))))
          ;; finish[0] = proc-time[job-at[0]]
          finish-0 (i/= (nth finish-vars 0) (i/nth (vec proc-times) (nth job-at 0)))
          ;; finish[k] = finish[k-1] + proc-time[job-at[k]]
          finish-chain (apply i/conjunction
                              (for [k (range 1 n)]
                                (i/= (nth finish-vars k)
                                      (i/+ (nth finish-vars (dec k))
                                            (i/nth (vec proc-times) (nth job-at k))))))
          ;; deadline constraint: finish[k] <= deadline[job-at[k]]
          deadline-constraints (apply i/conjunction
                                      (for [k (range n)]
                                        (i/<= (nth finish-vars k)
                                               (i/nth (vec deadlines) (nth job-at k)))))
          solution (i/satisfy (i/and all-diff job-at-diff inverse-link
                                     finish-0 finish-chain deadline-constraints))
          pos* (mapv solution pos)
          job-at* (mapv solution job-at)
          finish* (mapv solution finish-vars)]
      ;; Verify all positions distinct
      (is (= n (count (distinct pos*))))
      ;; Verify each job finishes by its deadline
      (doseq [k (range n)]
        (is (<= (nth finish* k) (nth deadlines (nth job-at* k))))))))

(deftest task-assignment-test
  (testing "assign 4 tasks to 2 workers minimizing total cost"
    ;; Cost matrix: cost[task][worker]
    ;; Task 0: [9  2]
    ;; Task 1: [6  4]
    ;; Task 2: [5  8]
    ;; Task 3: [7  3]
    ;; Each worker gets exactly 2 tasks.
    ;; Optimal: worker 0 gets tasks 1,2 (cost 6+5=11), worker 1 gets tasks 0,3 (cost 2+3=5), total=16
    (let [n-tasks 4
          n-workers 2
          costs [[9 2] [6 4] [5 8] [7 3]]
          worker-domain (range n-workers)
          ;; assignment[t] = which worker does task t
          assignment (vec (repeatedly n-tasks #(i/fresh-int worker-domain)))
          ;; each worker gets exactly 2 tasks
          balance (->> (for [w (range n-workers)]
                         (let [task-count (apply i/+
                                                 (for [t (range n-tasks)]
                                                   (i/if (i/= (nth assignment t) w) 1 0)))]
                           (i/= task-count 2)))
                       (apply i/conjunction))
          ;; cost of each task
          task-costs (vec (for [t (range n-tasks)]
                           (i/fresh-int (range 20))))
          cost-links (apply i/conjunction
                            (for [t (range n-tasks)]
                              (i/= (nth task-costs t)
                                    (i/nth (vec (nth costs t)) (nth assignment t)))))
          total-cost (apply i/+ task-costs)
          constraint (i/and balance cost-links)
          solution (i/maximize (i/- 0 total-cost) constraint)
          assignment* (mapv solution assignment)
          task-costs* (mapv solution task-costs)
          total* (apply + task-costs*)]
      ;; Optimal total cost is 16
      (is (= 16 total*))
      ;; Each worker has exactly 2 tasks
      (is (= 2 (count (filter #(= 0 %) assignment*))))
      (is (= 2 (count (filter #(= 1 %) assignment*)))))))

;; ============================================================
;; 13. Combinatorial design
;; ============================================================

(deftest latin-square-4x4-test
  (testing "fill a 4x4 grid with values 1-4, each row and column all different"
    (let [n 4
          val-domain (range 1 (inc n))
          ;; cells[row][col]
          cells (vec (for [_ (range n)]
                       (vec (repeatedly n #(i/fresh-int val-domain)))))
          ;; row constraints: all different in each row
          row-diff (->> (for [r (range n)
                              i (range n)
                              j (range (inc i) n)]
                          (i/not= (get-in cells [r i]) (get-in cells [r j])))
                        (apply i/conjunction))
          ;; column constraints: all different in each column
          col-diff (->> (for [c (range n)
                              i (range n)
                              j (range (inc i) n)]
                          (i/not= (get-in cells [i c]) (get-in cells [j c])))
                        (apply i/conjunction))
          solution (i/satisfy (i/and row-diff col-diff))
          grid (vec (for [r (range n)]
                      (vec (for [c (range n)]
                             (get solution (get-in cells [r c]))))))]
      ;; Each row has all values 1-4
      (doseq [r (range n)]
        (is (= (set val-domain) (set (nth grid r)))))
      ;; Each column has all values 1-4
      (doseq [c (range n)]
        (is (= (set val-domain) (set (map #(nth % c) grid))))))))

(deftest involution-test
  (testing "find a permutation p of size 5 such that p[p[i]] = i (an involution)"
    ;; An involution is a permutation that is its own inverse.
    ;; Uses i/nth to express p[p[i]].
    (let [n 5
          perm-domain (range n)
          p (vec (repeatedly n #(i/fresh-int perm-domain)))
          ;; p is a permutation: all different
          all-diff (->> (for [i (range n) j (range (inc i) n)]
                          (i/not= (nth p i) (nth p j)))
                        (apply i/conjunction))
          ;; involution: p[p[i]] = i for all i
          involution (->> (for [i (range n)]
                            (i/= (i/nth p (nth p i)) i))
                          (apply i/conjunction))
          solution (i/satisfy (i/and all-diff involution))
          p* (mapv solution p)]
      ;; Verify it's a permutation
      (is (= (set (range n)) (set p*)))
      ;; Verify p[p[i]] = i
      (doseq [i (range n)]
        (is (= i (nth p* (nth p* i))))))))

;; ============================================================
;; 14. Bin packing
;; ============================================================

(deftest bin-packing-test
  (testing "pack 5 items into bins of capacity 7, minimize bins used"
    ;; Items: sizes [3, 4, 2, 5, 1]
    ;; Bin capacity: 7
    ;; Optimal: bin0={3,4}, bin1={5,2}, bin2={1} => 3 bins
    ;; But we can do better: bin0={5,2}, bin1={4,3}, bin2={1} => 3 bins
    ;; Actually: bin0={5,1}, bin1={4,2}, bin2={3} => 3 bins
    ;; Or: bin0={5,2}, bin1={4,1}, bin2={3} => 3 bins
    ;; Minimum is 3 bins (total size = 15, capacity 7, ceil(15/7) = 3).
    ;; But wait: bin0={4,3}, bin1={5,2}, bin2={1} = 3 bins. We can't do 2.
    ;; Actually: bin0={5,2}, bin1={4,3} capacity 7 each, bin2={1}. Total 3.
    ;; Let's try 3 bins.
    (let [n-items 5
          sizes [3 4 2 5 1]
          capacity 7
          n-bins 3 ;; we know optimal is 3
          bin-domain (range n-bins)
          ;; bin[item] = which bin this item goes in
          bins (vec (repeatedly n-items #(i/fresh-int bin-domain)))
          ;; for each bin, sum of item sizes assigned to it <= capacity
          capacity-constraints
          (->> (for [b (range n-bins)]
                 (i/<= (apply i/+
                              (for [item (range n-items)]
                                (i/if (i/= (nth bins item) b)
                                  (nth sizes item)
                                  0)))
                        capacity))
               (apply i/conjunction))
          solution (i/satisfy capacity-constraints)
          bins* (mapv solution bins)]
      ;; Verify capacity constraint for each bin
      (doseq [b (range n-bins)]
        (let [bin-total (->> (range n-items)
                             (filter #(= b (nth bins* %)))
                             (map #(nth sizes %))
                             (apply +))]
          (is (<= bin-total capacity))))
      ;; All items assigned
      (is (= n-items (count bins*))))))

;; ============================================================
;; 15. Planning / Reachability
;; ============================================================

(deftest river-crossing-test
  (testing "farmer-wolf-goat-cabbage river crossing puzzle"
    ;; State: position of farmer(F), wolf(W), goat(G), cabbage(C)
    ;; 0 = left bank, 1 = right bank
    ;; Farmer must be present to row. Can carry at most 1 item.
    ;; Unsafe: wolf+goat alone, goat+cabbage alone (without farmer).
    ;; Find minimum steps to get all to right bank.
    ;; Known answer: 7 steps.
    (let [max-steps 7
          binary (range 2)
          ;; State at each time step: [farmer wolf goat cabbage]
          ;; state[t][entity] where entity: 0=farmer, 1=wolf, 2=goat, 3=cabbage
          state (vec (for [_ (range (inc max-steps))]
                       (vec (repeatedly 4 #(i/fresh-int binary)))))
          ;; Initial state: all on left bank (0)
          init (apply i/conjunction
                      (for [e (range 4)]
                        (i/= (get-in state [0 e]) 0)))
          ;; Final state: all on right bank (1)
          final-state (apply i/conjunction
                             (for [e (range 4)]
                               (i/= (get-in state [max-steps e]) 1)))
          ;; Safety constraints at each step (except maybe the final step which is valid)
          safety (apply i/conjunction
                        (for [t (range (inc max-steps))]
                          (let [f (get-in state [t 0])
                                w (get-in state [t 1])
                                g (get-in state [t 2])
                                c (get-in state [t 3])]
                            (i/and
                             ;; wolf and goat can't be alone (without farmer)
                             (i/or (i/= f w) (i/not= w g))
                             ;; goat and cabbage can't be alone (without farmer)
                             (i/or (i/= f g) (i/not= g c))))))
          ;; Transition constraints between steps
          transitions
          (apply i/conjunction
                 (for [t (range max-steps)]
                   (let [f0 (get-in state [t 0]) f1 (get-in state [(inc t) 0])
                         w0 (get-in state [t 1]) w1 (get-in state [(inc t) 1])
                         g0 (get-in state [t 2]) g1 (get-in state [(inc t) 2])
                         c0 (get-in state [t 3]) c1 (get-in state [(inc t) 3])]
                     (i/and
                      ;; Farmer always moves
                      (i/not= f0 f1)
                      ;; Each non-farmer entity either stays or moves WITH the farmer
                      ;; Entity moves only if it was on the same side as farmer before
                      ;; and ends on the same side as farmer after.
                      ;; At most one entity moves with the farmer.
                      ;; Entity stays: entity[t+1] = entity[t]
                      ;; Entity moves: entity[t] = farmer[t] AND entity[t+1] = farmer[t+1]
                      ;; Use: moved[e] = (entity[t] != entity[t+1])
                      ;; At most one of wolf, goat, cabbage moved.
                      ;; If entity moved, it must have been with the farmer.
                      (let [w-moved (i/not= w0 w1)
                            g-moved (i/not= g0 g1)
                            c-moved (i/not= c0 c1)]
                        (i/and
                         ;; If wolf moved, it was with farmer both before and after
                         (i/when w-moved (i/and (i/= w0 f0) (i/= w1 f1)))
                         ;; If goat moved, it was with farmer both before and after
                         (i/when g-moved (i/and (i/= g0 f0) (i/= g1 f1)))
                         ;; If cabbage moved, it was with farmer both before and after
                         (i/when c-moved (i/and (i/= c0 f0) (i/= c1 f1)))
                         ;; At most one entity moves with the farmer
                         ;; (at most one of w-moved, g-moved, c-moved is true)
                         ;; Using: NOT(a AND b) for each pair
                         (i/not (i/and w-moved g-moved))
                         (i/not (i/and w-moved c-moved))
                         (i/not (i/and g-moved c-moved))))))))
          solution (i/satisfy (i/and init final-state safety transitions))
          result (vec (for [t (range (inc max-steps))]
                        (vec (for [e (range 4)]
                               (get solution (get-in state [t e]))))))]
      ;; Verify initial state
      (is (= [0 0 0 0] (first result)))
      ;; Verify final state
      (is (= [1 1 1 1] (last result)))
      ;; Verify safety at each step
      (doseq [[f w g c] result]
        ;; wolf-goat: if farmer != wolf, then wolf != goat
        (is (or (= f w) (not= w g)))
        ;; goat-cabbage: if farmer != goat, then goat != cabbage
        (is (or (= f g) (not= g c)))))))

;; ============================================================
;; 16. Production planning (integer linear programming)
;; ============================================================

(deftest production-planning-test
  (testing "maximize profit from two products with resource constraints"
    ;; Product A: uses 2 units material, 1 unit labor, profit 3
    ;; Product B: uses 1 unit material, 2 units labor, profit 5
    ;; Available: 14 material, 12 labor
    ;; Maximize: 3*a + 5*b
    ;; Optimal (LP relaxation): a=16/3, b=10/3 -> profit ~30.3
    ;; Integer optimal: a=2, b=5 -> profit 31? No: 2*2+1*5=9<=14, 1*2+2*5=12<=12. profit=31.
    ;; Actually: a=2, b=5 -> material=2*2+1*5=9, labor=1*2+2*5=12. Profit=3*2+5*5=31. Valid.
    ;; Try a=4, b=4: material=12, labor=12. Profit=32. Even better!
    ;; Try a=6, b=2: material=14, labor=10. Profit=28. Worse.
    ;; Try a=5, b=4: material=14, labor=13 > 12. Invalid.
    ;; Try a=4, b=4: profit=32. Valid.
    ;; Try a=3, b=4: material=10, labor=11. profit=29. Worse.
    ;; Try a=4, b=5: material=13, labor=14>12. Invalid.
    ;; Try a=2, b=5: profit=31. Valid.
    ;; So a=4, b=4 -> 32 is optimal.
    (let [a (i/fresh-int (range 20))
          b (i/fresh-int (range 20))
          profit (i/+ (i/* 3 a) (i/* 5 b))
          constraint (i/and
                      (i/<= (i/+ (i/* 2 a) b) 14)         ;; material
                      (i/<= (i/+ a (i/* 2 b)) 12))        ;; labor
          solution (i/maximize profit constraint)
          a* (get solution a) b* (get solution b)
          profit* (+ (* 3 a*) (* 5 b*))]
      ;; Verify constraints
      (is (<= (+ (* 2 a*) b*) 14))
      (is (<= (+ a* (* 2 b*)) 12))
      ;; Optimal profit is 32 (a=4, b=4)
      (is (= 32 profit*)))))
