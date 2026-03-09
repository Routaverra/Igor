(ns routaverra.igor.extensional-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]))

;; ============================================================
;; table tests
;; ============================================================

(deftest table-basic-test
  (testing "table constrains vars to one of the allowed tuples"
    (let [x1 (i/fresh-int (range 10))
          x2 (i/fresh-int (range 10))
          x3 (i/fresh-int (range 10))
          allowed [[1 2 3] [4 5 6] [7 8 9]]
          constraint (i/table [x1 x2 x3] allowed)
          solution (i/satisfy constraint)]
      (is (some? solution))
      (let [result [(get solution x1) (get solution x2) (get solution x3)]]
        (is (some #{result} allowed))))))

(deftest table-ground-passthrough-test
  (testing "table with ground vars returns boolean directly"
    (is (true? (i/table [1 2 3] [[1 2 3] [4 5 6]])))
    (is (false? (i/table [1 2 4] [[1 2 3] [4 5 6]])))))

;; ============================================================
;; regular tests
;; ============================================================

(deftest regular-basic-test
  (testing "regular constrains sequence to DFA-accepted language"
    ;; DFA: 2 states, alphabet {0 1}, accepts sequences ending in 1
    ;; state 0 = "last was 0 or start", state 1 = "last was 1"
    (let [dfa {:states      2
               :alphabet    #{0 1}
               :transitions [{0 0, 1 1}   ; state 0
                             {0 0, 1 1}]  ; state 1
               :start       0
               :accept      #{1}}
          vars (vec (repeatedly 4 #(i/fresh-int #{0 1})))
          solution (i/satisfy (i/regular vars dfa))]
      (is (some? solution))
      ;; last element must be 1
      (is (= 1 (get solution (last vars)))))))

(deftest regular-negative-alphabet-test
  (testing "regular with negative alphabet values (set-overload)"
    ;; DFA: 2 states, alphabet {-1 0 1}, accepts if sum of steps >= 1
    ;; state 0 = "not yet positive enough", state 1 = "positive enough"
    ;; Simplification: state 0 ->  {-1 0, 0 0, 1 1}, state 1 -> {-1 0, 0 1, 1 1}
    (let [dfa {:states      2
               :alphabet    #{-1 0 1}
               :transitions [{-1 0, 0 0, 1 1}
                             {-1 0, 0 1, 1 1}]
               :start       0
               :accept      #{1}}
          vars (vec (repeatedly 3 #(i/fresh-int #{-1 0 1})))
          solution (i/satisfy (i/regular vars dfa))]
      (is (some? solution))
      ;; must end in accepting state — at least one 1 that isn't cancelled
      (let [vals (mapv #(get solution %) vars)]
        (is (contains? #{1} (last (reduce (fn [state sym]
                                            (conj state
                                                  (get (nth (:transitions dfa) (peek state)) sym)))
                                          [0] vals))))))))

(deftest regular-ground-passthrough-test
  (testing "regular with ground sequence simulates DFA"
    (let [dfa {:states      2
               :alphabet    #{0 1}
               :transitions [{0 0, 1 1}
                             {0 0, 1 1}]
               :start       0
               :accept      #{1}}]
      (is (true? (i/regular [0 1 0 1] dfa)))
      (is (false? (i/regular [0 1 0 0] dfa)))
      (is (true? (i/regular [1] dfa)))
      (is (false? (i/regular [0] dfa))))))

;; ============================================================
;; cost_regular tests
;; ============================================================

(deftest cost-regular-basic-test
  (testing "cost_regular constrains sequence and binds cost"
    ;; DFA: 2 states, alphabet {1 2 3}, all transitions stay in state 0
    ;; costs: symbol 1 costs 0, symbol 2 costs 1, symbol 3 costs 2
    (let [dfa {:states      1
               :alphabet    #{1 2 3}
               :transitions [{1 0, 2 0, 3 0}]
               :start       0
               :accept      #{0}
               :costs       [{1 0, 2 1, 3 2}]}
          vars (vec (repeatedly 3 #(i/fresh-int #{1 2 3})))
          cost (i/fresh-int (range 0 10))
          solution (i/satisfy (i/cost-regular vars cost dfa))]
      (is (some? solution))
      (let [vals (mapv #(get solution %) vars)
            cost-val (get solution cost)
            expected-cost (reduce + (map {1 0, 2 1, 3 2} vals))]
        (is (= expected-cost cost-val))))))

(deftest cost-regular-minimize-test
  (testing "cost_regular with minimization finds optimal cost"
    ;; 1 state DFA, alphabet {1 2 3}, always accepts
    ;; costs: 1->5, 2->1, 3->3
    (let [dfa {:states      1
               :alphabet    #{1 2 3}
               :transitions [{1 0, 2 0, 3 0}]
               :start       0
               :accept      #{0}
               :costs       [{1 5, 2 1, 3 3}]}
          vars (vec (repeatedly 3 #(i/fresh-int #{1 2 3})))
          cost (i/fresh-int (range 0 100))
          ;; minimize cost by maximizing (- cost)
          solution (i/maximize (i/- 0 cost) (i/cost-regular vars cost dfa))]
      (is (some? solution))
      (let [cost-val (get solution cost)]
        ;; minimum cost is 3 * 1 = 3 (all symbols = 2)
        (is (= 3 cost-val))
        (doseq [v vars]
          (is (= 2 (get solution v))))))))

(deftest cost-regular-negative-alphabet-test
  (testing "cost_regular with negative alphabet values (requires remapping)"
    ;; 1 state, alphabet {-1 0 1}, always accepts
    ;; costs: -1->3, 0->1, 1->2
    (let [dfa {:states      1
               :alphabet    #{-1 0 1}
               :transitions [{-1 0, 0 0, 1 0}]
               :start       0
               :accept      #{0}
               :costs       [{-1 3, 0 1, 1 2}]}
          vars (vec (repeatedly 2 #(i/fresh-int #{-1 0 1})))
          cost (i/fresh-int (range 0 20))
          solution (i/maximize (i/- 0 cost) (i/cost-regular vars cost dfa))]
      (is (some? solution))
      ;; minimum cost = 2 * 1 = 2 (all symbols = 0)
      (is (= 2 (get solution cost))))))

;; ============================================================
;; Music examples
;; ============================================================

(deftest chord-progression-table-test
  (testing "chord progressions constrained by table"
    ;; chord IDs: 0=I, 1=ii, 2=iii, 3=IV, 4=V, 5=vi
    (let [progressions [[0 3] [0 4] [3 0] [3 4] [4 0] [5 3] [5 4]]
          vars (vec (repeatedly 4 #(i/fresh-int (range 6))))
          ;; constrain each adjacent pair
          constraints (for [idx (range 3)]
                        (i/table [(vars idx) (vars (inc idx))] progressions))
          ;; start on I (0)
          solution (i/satisfy (apply i/and (i/= (first vars) 0) constraints))]
      (is (some? solution))
      (let [vals (mapv #(get solution %) vars)]
        ;; first chord is I
        (is (= 0 (first vals)))
        ;; each adjacent pair is in the allowed set
        (doseq [idx (range 3)]
          (is (some #{[(vals idx) (vals (inc idx))]} progressions)))))))

(deftest melodic-contour-regular-test
  (testing "melodic intervals constrained by regular DFA"
    ;; Simple contour grammar: after two ascending intervals (>0),
    ;; must descend (<=0). States: 0=neutral, 1=one-ascending, 2=must-descend
    ;; alphabet: -2 -1 0 1 2
    (let [alphabet #{-2 -1 0 1 2}
          asc #{1 2}
          dfa {:states      3
               :alphabet    alphabet
               :transitions [;; state 0 (neutral): ascending -> state 1, else -> 0
                             {-2 0, -1 0, 0 0, 1 1, 2 1}
                             ;; state 1 (one ascending): ascending -> state 2, else -> 0
                             {-2 0, -1 0, 0 0, 1 2, 2 2}
                             ;; state 2 (must descend): only descend/stay allowed -> 0
                             {-2 0, -1 0, 0 0}]
               :start       0
               :accept      #{0 1}}
          intervals (vec (repeatedly 6 #(i/fresh-int alphabet)))
          solution (i/satisfy (i/regular intervals dfa))]
      (is (some? solution))
      ;; verify the constraint: no three consecutive ascending intervals
      (let [vals (mapv #(get solution %) intervals)]
        (doseq [idx (range (- (count vals) 2))]
          (is (not (and (asc (vals idx))
                        (asc (vals (inc idx)))
                        (asc (vals (+ idx 2)))))))))))

;; ============================================================
;; Full music example: arch-contour cantus firmus with florid
;; counterpoint, both voices satisfying a melodic contour DFA
;; ============================================================

;; Musical scenario:
;;
;; We simultaneously compose a cantus firmus and a florid (2:1)
;; counterpoint voice. Both voices are constrained to follow an
;; "arch" melodic contour — they must ascend before they descend,
;; and once descending begins, no more ascending. This is a
;; classical rule for well-shaped melodies.
;;
;; The CF contour is enforced via `regular` (DFA accepting
;; ascending-then-descending interval sequences).
;;
;; The CP contour is enforced via `cost_regular` with the same DFA
;; plus per-transition costs that penalize large leaps. The solver
;; minimizes the CP's total leap cost, naturally producing smooth,
;; conjunct (stepwise) melodic motion.
;;
;; Second-species counterpoint rules connect the two voices:
;; strong beats consonant, weak-beat dissonances are passing tones,
;; CP above CF, start/end on a perfect consonance.

(def ^:private diatonic
  "C-major diatonic pitches as semitone offsets from C3, spanning two octaves."
  [0 2 4 5 7 9 11 12 14 16 17 19 21 23 24])

(def ^:private interval-alphabet
  "Allowed melodic intervals (semitones): up to a perfect 4th in either direction."
  (set (range -5 6)))

(def ^:private arch-transitions
  "DFA transitions for arch contour.
   State 0: pre-ascent (haven't gone up yet — only ascending or repeating allowed)
   State 1: ascending phase (ascending, repeating, or first descent transitions out)
   State 2: descending phase (only descending or repeating allowed — no more ascending)"
  [;; State 0: positive → 1, zero → 0 (negative = dead)
   (merge (zipmap (range 1 6) (repeat 1))
          {0 0})
   ;; State 1: positive → 1, zero → 1, negative → 2
   (merge (zipmap (range 1 6) (repeat 1))
          {0 1}
          (zipmap (range -5 0) (repeat 2)))
   ;; State 2: negative → 2, zero → 2 (positive = dead)
   (into {} (map #(vector % 2) (range -5 1)))])

(def ^:private arch-dfa
  "DFA accepting interval sequences with arch shape: ascend then descend.
   Rejects sequences that ascend after descending, or that never ascend,
   or that never descend."
  {:states      3
   :alphabet    interval-alphabet
   :transitions arch-transitions
   :start       0
   :accept      #{2}})

(def ^:private leap-costs
  "Per-transition costs: abs(interval). Penalizes large leaps — a step (1-2
   semitones) costs 1-2, a third (3-4) costs 3-4, a fourth (5) costs 5.
   Applied uniformly across all DFA states."
  (vec (repeat 3 (into {} (map #(vector % (Math/abs (long %)))
                                interval-alphabet)))))

(def ^:private consonant-intervals
  "Consonant harmonic intervals (semitones) within [0,24].
   Interval class in {0 3 4 7 8 9}: unison, m3, M3, P5, m6, M6, + compounds."
  (set (for [oct [0 12 24], ic [0 3 4 7 8 9]
             :let [iv (+ oct ic)] :when (<= 0 iv 24)]
         iv)))

(def ^:private perfect-intervals
  "Perfect consonances: unison, P5, P8, and octave compounds."
  (set (for [oct [0 12 24], ic [0 7]
             :let [iv (+ oct ic)] :when (<= 0 iv 24)]
         iv)))

(deftest arch-contour-counterpoint-test
  (testing "CF and florid CP both follow arch contour, CP minimizes leap cost"
    (let [;; === Cantus firmus: 5 notes ===
          n-cf 5
          cf (vec (for [_ (range n-cf)] (i/fresh-int diatonic)))
          cf-ivars (vec (for [_ (range (dec n-cf))] (i/fresh-int interval-alphabet)))
          cf-ivar-links (apply i/and
                          (for [t (range (dec n-cf))]
                            (i/= (nth cf-ivars t)
                                  (i/- (nth cf (inc t)) (nth cf t)))))
          ;; CF starts and ends on C
          cf-anchors (i/and (i/= (first cf) 0) (i/= (last cf) 0))
          ;; CF contour: arch shape via regular DFA
          cf-contour (i/regular cf-ivars arch-dfa)

          ;; === Counterpoint: 2:1 ratio = 10 notes ===
          n-cp (* 2 n-cf)
          cp (vec (for [_ (range n-cp)] (i/fresh-int diatonic)))
          cp-ivars (vec (for [_ (range (dec n-cp))] (i/fresh-int interval-alphabet)))
          cp-ivar-links (apply i/and
                          (for [t (range (dec n-cp))]
                            (i/= (nth cp-ivars t)
                                  (i/- (nth cp (inc t)) (nth cp t)))))
          ;; CP contour: arch shape + minimize leap cost via cost_regular
          cp-cost (i/fresh-int (range 0 50))
          cp-contour (i/cost-regular cp-ivars cp-cost
                       (assoc arch-dfa :costs leap-costs))

          ;; === Second-species counterpoint constraints ===
          ;; Which CF note sounds at each CP beat
          cf-at (vec (for [t (range n-cp)] (nth cf (quot t 2))))
          ;; Harmonic intervals (CP - CF)
          h-ivs (vec (for [t (range n-cp)]
                       (i/- (nth cp t) (nth cf-at t))))
          ;; CP above CF
          above (apply i/and
                  (for [t (range n-cp)] (i/>= (nth cp t) (nth cf-at t))))
          ;; Strong beats (even indices) must be consonant
          strong-con (apply i/and
                       (for [t (range 0 n-cp 2)]
                         (i/contains? consonant-intervals (nth h-ivs t))))
          ;; Weak-beat dissonances must be passing tones (stepwise approach + departure)
          weak-pass (apply i/and
                      (for [t (range 1 (dec n-cp) 2)]
                        (i/when (i/not (i/contains? consonant-intervals (nth h-ivs t)))
                          (i/and (i/<= (i/abs (nth cp-ivars (dec t))) 2)
                                 (i/<= (i/abs (nth cp-ivars t)) 2)))))
          ;; Start and end on perfect consonance
          start-end (i/and (i/contains? perfect-intervals (first h-ivs))
                           (i/contains? perfect-intervals (last h-ivs)))

          ;; === Solve: minimize CP leap cost ===
          all-constraints (i/and cf-ivar-links cf-contour cf-anchors
                                 cp-ivar-links cp-contour
                                 above strong-con weak-pass start-end)
          solution (i/maximize (i/- 0 cp-cost) all-constraints)]

      (is (some? solution) "Should find a solution")

      ;; --- Verify CF arch contour ---
      (let [cf* (mapv solution cf)
            cf-ivs* (mapv #(- (cf* (inc %)) (cf* %)) (range (dec n-cf)))]
        (is (= 0 (first cf*)) "CF starts on C")
        (is (= 0 (last cf*)) "CF ends on C")
        (is (some pos? cf-ivs*) "CF must ascend")
        (is (some neg? cf-ivs*) "CF must descend")
        ;; No ascending after first descent
        (let [desc-idx (first (keep-indexed #(when (neg? %2) %1) cf-ivs*))]
          (is (every? #(<= % 0) (subvec cf-ivs* desc-idx))
              "CF: no ascending after first descent")))

      ;; --- Verify CP arch contour ---
      (let [cp* (mapv solution cp)
            cp-ivs* (mapv #(- (cp* (inc %)) (cp* %)) (range (dec n-cp)))]
        (is (some pos? cp-ivs*) "CP must ascend")
        (is (some neg? cp-ivs*) "CP must descend")
        (let [desc-idx (first (keep-indexed #(when (neg? %2) %1) cp-ivs*))]
          (is (every? #(<= % 0) (subvec cp-ivs* desc-idx))
              "CP: no ascending after first descent")))

      ;; --- Verify counterpoint rules ---
      (let [cf* (mapv solution cf)
            cp* (mapv solution cp)
            cf-at* (vec (for [t (range n-cp)] (nth cf* (quot t 2))))]
        ;; Strong beats consonant
        (doseq [t (range 0 n-cp 2)]
          (is (contains? consonant-intervals (- (nth cp* t) (nth cf-at* t)))
              (str "Strong beat " t " interval "
                   (- (nth cp* t) (nth cf-at* t)) " not consonant")))
        ;; Weak-beat dissonances are stepwise
        (doseq [t (range 1 (dec n-cp) 2)]
          (let [interval (- (nth cp* t) (nth cf-at* t))]
            (when-not (contains? consonant-intervals interval)
              (is (<= (Math/abs (- (nth cp* t) (nth cp* (dec t)))) 2)
                  (str "Non-stepwise approach at weak beat " t))
              (is (<= (Math/abs (- (nth cp* (inc t)) (nth cp* t))) 2)
                  (str "Non-stepwise departure from weak beat " t)))))
        ;; Start/end on perfect consonance
        (is (contains? perfect-intervals (- (first cp*) (first cf-at*)))
            "First harmonic interval must be perfect")
        (is (contains? perfect-intervals (- (last cp*) (last cf-at*)))
            "Last harmonic interval must be perfect")
        ;; CP above CF
        (doseq [t (range n-cp)]
          (is (>= (nth cp* t) (nth cf-at* t))
              (str "CP below CF at beat " t))))

      ;; --- Verify cost is bound and positive ---
      (is (pos? (get solution cp-cost))
          "Leap cost should be positive (some motion required)"))))
