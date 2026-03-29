(ns routaverra.igor.native.domains)

(defprotocol IDomain
  (domain-min [d])
  (domain-max [d])
  (domain-size [d])
  (assigned? [d])
  (contains-value? [d v])
  (remove-value [d v])
  (restrict-min [d new-min])
  (restrict-max [d new-max])
  (domain-values [d]))

;; IntervalDomain: bounds-consistent integer domain [min, max]
(defrecord IntervalDomain [lo hi]
  IDomain
  (domain-min [_] lo)
  (domain-max [_] hi)
  (domain-size [_] (inc (- hi lo)))
  (assigned? [_] (= lo hi))
  (contains-value? [_ v] (and (>= v lo) (<= v hi)))
  (remove-value [d v]
    (cond
      (or (< v lo) (> v hi)) [d #{}]
      (= lo hi) ::failed
      (= v lo) [(->IntervalDomain (inc lo) hi)
                 (if (= (inc lo) hi) #{:assigned :bounds} #{:bounds})]
      (= v hi) [(->IntervalDomain lo (dec hi))
                 (if (= lo (dec hi)) #{:assigned :bounds} #{:bounds})]
      :else [d #{}])) ;; interior removal is no-op for IntervalDomain (bounds consistency)
  (restrict-min [d new-min]
    (let [new-min (max lo new-min)]
      (cond
        (> new-min hi) ::failed
        (= new-min lo) [d #{}]
        (= new-min hi) [(->IntervalDomain new-min new-min) #{:assigned :bounds}]
        :else [(->IntervalDomain new-min hi) #{:bounds}])))
  (restrict-max [d new-max]
    (let [new-max (min hi new-max)]
      (cond
        (< new-max lo) ::failed
        (= new-max hi) [d #{}]
        (= new-max lo) [(->IntervalDomain new-max new-max) #{:assigned :bounds}]
        :else [(->IntervalDomain lo new-max) #{:bounds}])))
  (domain-values [_] (range lo (inc hi))))

;; EnumeratedDomain: explicit sorted set of values (full domain consistency)
(defrecord EnumeratedDomain [values] ;; values is a sorted-set
  IDomain
  (domain-min [_] (first values))
  (domain-max [_] (if (instance? clojure.lang.Reversible values)
                    (first (rseq values))
                    (last (seq values))))
  (domain-size [_] (count values))
  (assigned? [_] (= 1 (count values)))
  (contains-value? [_ v] (contains? values v))
  (remove-value [d v]
    (if-not (contains? values v)
      [d #{}]
      (let [new-vals (disj values v)]
        (if (empty? new-vals)
          ::failed
          (let [events (cond-> #{:domain}
                         (= 1 (count new-vals)) (conj :assigned)
                         (or (= v (first values))
                             (= v (domain-max d))) (conj :bounds))]
            [(->EnumeratedDomain new-vals) events])))))
  (restrict-min [d new-min]
    (let [new-vals (into (sorted-set) (filter #(>= % new-min)) values)]
      (cond
        (empty? new-vals) ::failed
        (= new-vals values) [d #{}]
        :else (let [events (cond-> #{:bounds :domain}
                             (= 1 (count new-vals)) (conj :assigned))]
                [(->EnumeratedDomain new-vals) events]))))
  (restrict-max [d new-max]
    (let [new-vals (into (sorted-set) (filter #(<= % new-max)) values)]
      (cond
        (empty? new-vals) ::failed
        (= new-vals values) [d #{}]
        :else (let [events (cond-> #{:bounds :domain}
                             (= 1 (count new-vals)) (conj :assigned))]
                [(->EnumeratedDomain new-vals) events]))))
  (domain-values [_] (seq values)))

(defn intersect-domain
  "Intersect two domains. Returns [new-domain events] or ::failed."
  [d1 d2]
  (let [new-lo (max (domain-min d1) (domain-min d2))
        new-hi (min (domain-max d1) (domain-max d2))]
    (if (> new-lo new-hi)
      ::failed
      ;; If either is enumerated, do set intersection
      (if (or (instance? EnumeratedDomain d1) (instance? EnumeratedDomain d2))
        (let [vals1 (if (instance? EnumeratedDomain d1) (:values d1) (apply sorted-set (domain-values d1)))
              vals2 (if (instance? EnumeratedDomain d2) (:values d2) (apply sorted-set (domain-values d2)))
              new-vals (clojure.set/intersection vals1 vals2)]
          (if (empty? new-vals)
            ::failed
            (let [new-d (->EnumeratedDomain new-vals)
                  events (cond-> #{}
                           (= 1 (count new-vals)) (conj :assigned)
                           (or (not= (first new-vals) (domain-min d1))
                               (not= (first (rseq new-vals)) (domain-max d1))) (conj :bounds)
                           (not= (count new-vals) (domain-size d1)) (conj :domain))]
              [new-d events])))
        ;; Both intervals
        (let [new-d (->IntervalDomain new-lo new-hi)
              events (cond-> #{}
                       (= new-lo new-hi) (conj :assigned)
                       (or (not= new-lo (domain-min d1))
                           (not= new-hi (domain-max d1))) (conj :bounds))]
          (if (empty? events)
            [d1 #{}]
            [new-d events]))))))

(defn make-singleton
  "Create a domain containing a single value."
  [v]
  (->IntervalDomain v v))
