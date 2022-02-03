(ns cc.pool
  (:gen-class)
  (:require [cc.expression :refer :all]
            [cc.combinator :refer [combinator-arity S]]
            [cc.reaction :refer :all]
            [cc.util :refer :all]
            [clojure.java.io :as io]
            [clojure.core.async :as a]
            [clojure.core.reducers :as r]
            [random-seed.core :refer :all]
            [clojure.algo.generic.functor :refer [fmap]])
  (:refer-clojure :exclude [rand rand-nth rand-int]))

(def max-reduction-sample-size 10)

(defmacro dbg [x] `(let  [x# ~x]  (println  "dbg:" '~x  "=" x#) x#))

(def valid-combinators '(S K I))

(defn rand-expr
  [size]
  (loop [expr nil
         size size]
    (if (= size 0)
      expr
      (recur (let [rand-comb (nth valid-combinators (rand-int (count valid-combinators)))]
               (if expr
                 (if (< (rand) 0.5)
                   (normalize (xapply expr rand-comb))
                   (normalize (xapply rand-comb expr)))
                 rand-comb))
             (dec size)))))

(defn pool-set-random-seed!
  [seed]
  (set-random-seed! seed)
  (expression-set-random-seed! seed))

(defrecord Pool [content decorators])

; Decorators define hooks on pool operations to maintain cached property values

(defn call-decorator-hook
  [pool decorator hook & args]
  (if-let [hook-func (get decorator hook)]
    (apply hook-func pool args)
    pool))

(defn call-all-decorators-hook
  [pool hook & args]
  (reduce #(apply call-decorator-hook %1 %2 hook args) pool (:decorators pool)))

(defn call-all-decorators-hook-in
  [coll in-key hook & args]
  (reduce #(apply call-decorator-hook %1 %2 hook args) coll (:decorators (get-in coll in-key))))

(def size-decorator
  "Keeps track of the total number of expressions in the pool"
  {:init (fn [pool] (assoc pool :size 0))
   :add (fn [pool expr qty] (update pool :size + qty))
   :remove (fn [pool expr qty] (update pool :size #(- % qty)))})

(def squared-norm-decorator
  "Keeps track of the squared norm of the number of expressions in the pool"
  {:init (fn [pool] (assoc pool :squared-norm 0))
   :add (fn [pool expr qty] (let [prev-qty (- (get-in pool [:content expr] 0) qty)
                                  new-qty (+ prev-qty qty)]
                              (-> pool
                                  (update :squared-norm - (* prev-qty prev-qty))
                                  (update :squared-norm + (* new-qty new-qty)))))
   :remove (fn [pool expr qty] (let [prev-qty (+ (get-in pool [:content expr] 0) qty)
                                     new-qty (- prev-qty qty)]
                                 (-> pool
                                     (update :squared-norm - (* prev-qty prev-qty))
                                     (update :squared-norm + (* new-qty new-qty)))))})

(def mass-decorator
  "Keeps track of the total number of expressions in the pool"
  {:init (fn [pool] (assoc pool :mass 0))
   :add (fn [pool expr qty] (update pool :mass + (* qty (size expr))))
   :remove (fn [pool expr qty] (update pool :mass #(- % (* qty (size expr)))))})

(def mass-decorator
  "Keeps track of the total number of expressions in the pool"
  {:init (fn [pool] (assoc pool :mass 0))
   :add (fn [pool expr qty] (update pool :mass + (* qty (size expr))))
   :remove (fn [pool expr qty] (update pool :mass #(- % (* qty (size expr)))))})

(def length-distr-decorator
  {:init (fn [pool] (assoc pool :length-distr {}))
   :add (fn [pool expr qty] (update-in pool
                                       [:length-distr (size expr)]
                                       (fnil #(+ % qty) 0)))
   :remove (fn [pool expr qty] (dec-or-dissoc-in pool
                                                 [:length-distr (size expr)]
                                                 qty))})

(declare pool-size)
(def generation-decorator
  "Keeps track of the time passed as number of generations"
  {:init (fn [pool] (assoc pool :generations 0.0))
   :apply-reaction (fn [{pool :pool reaction :reaction}]
                     (let [increment (float (/ 1 (pool-size pool)))
                           current (get pool :generations)]
                       {:pool (update pool :generations + increment)
                        :reaction (assoc reaction :generations (+ current increment))}))})

(def reactions-count-decorator
  "Keeps track of the time passed as number of reactions"
  {:init (fn [pool] (assoc pool :reactions 0))
   :apply-reaction (fn [{pool :pool reaction :reaction}]
                     (let [current (get pool :reactions)]
                       {:pool (update pool :reactions inc)
                        :reaction (assoc reaction :reactions (inc current))}))})

(defn reduction-propensity
  [reduction expr combinator->reduction-rates pool]
  (* (reduce * (->> reduction
                    :reactants
                    (map #(get-in pool [:content %] 0))))
     (get-in pool [:content expr])
     (/ (combinator->reduction-rates (:type reduction))
        (if (= (:type reduction) 'S) (:volume pool) 1))))

(declare pool-contains?)
(declare cc-all-reductions)
(defn with-gillespie-parameters
  [pool condensation-rate reduction-rate volume]
  (-> pool
      (assoc :condensation-rate condensation-rate)
      (assoc :combinator->reduction-rates
             (if (number? reduction-rate)
               (into {} (for [c valid-combinators] [c reduction-rate]))
               reduction-rate))
      (assoc :volume volume)))

(def gillespie-decorator
  {:init (fn [pool] (-> pool
                        (assoc :seconds 0)
                        (assoc :expr->reductions {})
                        (assoc :redexes #{})
                        (assoc :leaves-size 0)))
   :add (fn [pool expr qty]
          (-> pool
              (cond-> (not (contains? (:expr->reductions pool) expr))
                (as-> pool
                      (let [expr-reductions
                            (->> expr
                                 (cc-all-reductions pool)
                                 (into []))]
                        (-> pool
                            (assoc-in [:expr->reductions expr] expr-reductions)
                            (cond-> (not (empty? expr-reductions))
                              (update :redexes conj expr))))))
              (cond-> (leaf? expr)
                (update :leaves-size + qty))))
   :remove (fn [pool expr qty]
             (-> pool
                 (cond-> (not (pool-contains? pool expr))
                   (-> (update :expr->reductions dissoc expr)
                       (update :redexes disj expr)))
                 (cond-> (leaf? expr)
                   (update :leaves-size - qty))))
   :apply-reaction (fn [{pool :pool reaction :reaction}]
                     {:pool (-> pool
                                (update :seconds + (get reaction :time-delta 0)))
                      :reaction (-> reaction
                                    (assoc :seconds  (+ (get pool :seconds) (get reaction :time-delta 0))))})})

(defn all-expr-reductions
  [pool]
  (for [expr (:redexes pool)
        reduction (get-in pool [:expr->reductions expr])]
    ;do (assert (<= (count (get-in pool [:expr->reductions expr])) max-reduction-sample-size))
    [expr reduction]))

(defn total-reduction-propensity
  [pool combinator->reduction-rates]
  (comment (reduce
            +
            (for [[expr reduction] (all-expr-reductions pool)]
              (reduction-propensity reduction
                                    expr
                                    combinator->reduction-rates
                                    pool))))
  (->> (all-expr-reductions pool)
       (r/map (fn [[expr reduction]] (reduction-propensity reduction
                                                           expr
                                                           combinator->reduction-rates
                                                           pool)))
       (r/fold +)))

(defn- sqrd [x] (* x x))

(defn gillespie-total-propensity-by-type
  [pool]
  {:cleave (- (:size pool) (:leaves-size pool))
   :condense (* (/ (:condensation-rate pool) (:volume pool)) (* (:size pool) (dec (:size pool)))) ;(* (/ (:condensation-rate pool) (:volume pool)) (- (sqrd (:size pool)) (/ (+ (:squared-norm pool) (:size pool)) 2)))
   :reduce (if (> (reduce max (vals (:combinator->reduction-rates pool))) 0)
             (total-reduction-propensity
              pool
              (:combinator->reduction-rates pool))
             0)})

(defn decorated
  [pool decorator]
  "Adds a decorator to the pool, which defines a number of hooks that will
  be called from certain operations in order to e.g. maintain cached properties"
  (-> pool
      (update :decorators conj decorator)))

(defn with-all-decorators
  [pool]
  (-> pool
      (decorated size-decorator)
      (decorated mass-decorator)
      (decorated length-distr-decorator)
      (decorated generation-decorator)
      (decorated reactions-count-decorator)
      (decorated squared-norm-decorator)
      (decorated gillespie-decorator)))

(def empty-pool
  (-> (Pool. {} [])
      with-all-decorators
      (call-all-decorators-hook :init)))

(defn pool-contains?
  ([pool expression cnt]
   (if-let [expression-count (get (:content pool) expression)]
     (>= expression-count cnt)))
  ([pool expression]
   (pool-contains? pool expression 1)))

(defn pool-contains-all-freq?
  [pool expressions-freq]
  (every? #(pool-contains? pool (first %) (second %)) (seq expressions-freq)))

(defn pool-contains-all?
  [pool expressions]
  (pool-contains-all-freq? pool (frequencies expressions)))

(defn cc-surface-reducible?
  "an expression is reducible in combinatory chemistry if it is reducible in 
  combinatory logic and the reactants are present in the pool"
  [pool expr]
  (and
   (surface-reducible? expr)
   (let [[combinator & args] expr]
     (or (not (= combinator 'S))
         (let [reactant (nth args 2)]
           (pool-contains? pool reactant))))))

(defn cc-all-reductions
  [pool expr]
  (-> expr
      (all-reductions (partial cc-surface-reducible? pool))
      (cond->> (some? max-reduction-sample-size)
        (take max-reduction-sample-size))))

(defn pool-multiplicity
  [pool expr]
  (or (get-in pool [:content expr]) 0))

(defn pool-add
  ([pool expression qty]
   (assert (expression? expression) (str "Invalid expression: " expression))
   (-> pool
       (update-in [:content expression] (fnil #(+ qty %) 0))
       (call-all-decorators-hook :add expression qty)))
  ([pool expression]
   (pool-add pool expression 1)))

(defn pool-add-all
  [pool exprs]
  (reduce pool-add pool exprs))

(defn- pool-decrement-or-dissoc-expression
  [pool expression qty]
  (dec-or-dissoc-in pool [:content expression] qty))

(defn pool-remove
  ([pool expression qty]
   (assert (or (= qty 0)
               (pool-contains? pool expression))
           (str "Can't remove non-existent expression " expression))
   (cond-> pool
     (> qty 0)
     (->
      (pool-decrement-or-dissoc-expression expression qty)
      (call-all-decorators-hook :remove expression qty))))
  ([pool expression]
   (pool-remove pool expression 1)))

(defn try-pool-remove
  "Removes as many copies of expression as it can up to a given amount"
  ([pool expression qty]
   (if (not (nil? expression))
     (let [av-qty (min qty (pool-multiplicity pool expression))]
       (if (> av-qty 0)
         (pool-remove pool expression av-qty)
         pool))
     pool))
  ([pool expression]
   (try-pool-remove pool expression 1)))

(defn pool-remove-all
  [pool expression]
  (pool-remove pool expression (get (:content pool) expression 0)))

(defn pool-remove-multi
  [pool expressions]
  (reduce pool-remove pool (apply concat (map #(repeat (val %) (key %)) expressions))))

(defn pool-size
  [pool]
  (assert (contains? pool :size) (str "Invalid pool: cached :size missing " pool))
  (:size pool))

(defn pool-mean-length
  [pool]
  (assert (contains? pool :length-distr)
          (str "Invalid pool: cached :length-distr missing " pool))
  (float (/ (reduce + (map #(apply * %) (seq (:length-distr pool))))
            (pool-size pool))))

(defn pool-max-length
  [pool]
  (assert (contains? pool :length-distr)
          (str "Invalid pool: cached :length-distr missing " pool))
  (float (apply max (keys (:length-distr pool)))))

(defn pool-diversity
  [pool]
  (assert (contains? pool :content) (str "Invalid pool " pool))
  (count (:content pool)))

(defn pool-mass-force
  [pool]
  (reduce + (map #(* (size (key %)) (val %)) (:content pool))))

(defn pool-mass
  [pool]
  (assert (contains? pool :mass) (str "Invalid pool: cached :mass missing " pool))
  (:mass pool))

(defn pool-generation
  [pool]
  (assert (contains? pool :generations) (str "Invalid pool: :generations missing " pool))
  (:generations pool))

(defn pool-reaction-count
  [pool]
  (assert (contains? pool :reactions) (str "Invalid pool: :reactions missing " pool))
  (:reactions pool))

(defn pool-atomic-multiplicity
  [pool]
  (into {} (map #(vector % (get-in pool [:content %]))  valid-combinators)))

(defn pool-combinators-count
  [pool]
  (reduce + (map (fn [[expr cnt]] (* cnt (size expr))) (seq (:content pool)))))

(defn add-combinators-uniformly
  ([pool total combinators]
   (let [qnt-per-combinator (quot total (count combinators))
         pool (assoc pool :base combinators)]
     (loop [pool pool
            remaining-qnt total
            remaining-combinators combinators]
       (if (= (count remaining-combinators) 1)
         (pool-add pool (first remaining-combinators) remaining-qnt)
         (recur (pool-add pool (first remaining-combinators) qnt-per-combinator)
                (- remaining-qnt qnt-per-combinator)
                (rest remaining-combinators))))))
  ([pool total]
   (add-combinators-uniformly pool total valid-combinators)))

(defn weighted-sample
  ([coll key-fn weight-fn n]
   (let [r (rand n)]
     (if (not (empty? coll))
       (loop [xs (seq coll)
              sum 0]
         (let [x (first xs)
               new-sum (+ sum (weight-fn x))]
           (if (or (< r new-sum) (empty? (rest xs)))
             (key-fn x)
             (recur (rest xs) new-sum))))
       nil))))

(defn sample-reactive
  [pool]
  (weighted-sample (:content pool) key val (pool-size pool)))

(defn rand-int-with-probs
  [probs]
  (let [r (rand)]
    (loop [ps probs
           i 0
           s 0]
      (let [p (first ps)
            s' (+ s p)]
        (if (or (< r s') (empty? probs))
          i
          (recur (rest ps) (inc i) s'))))))

(defn cleave
  [reactive]
  (generic-reaction [reactive] (break-mid reactive)))

(defn filter-pool
  [pool f]
  (reduce pool-remove-all
          pool
          (filter (comp not f) (keys (:content pool)))))

(defn sample-cleave-reaction
  [pool]
  (-> pool
      (#(reduce pool-remove-all % (:base pool)))
      sample-reactive
      cleave))

(defn condense
  [reactive1 reactive2]
  (generic-reaction [reactive1 reactive2] [(xapply reactive1 reactive2)]))

(defn sample-condense-reaction
  [pool]
  (let [reactive1 (sample-reactive pool)
        reactive2 (sample-reactive (pool-remove pool reactive1))]
    (condense reactive1 reactive2)))

(defn sample-reduce-reaction
  [pool combinator->reduction-rates total-propensity]
  (let [candidate-reductions (all-expr-reductions pool)
        [expr reduction] (weighted-sample
                          candidate-reductions
                          identity
                          #(reduction-propensity (second %) (first %) combinator->reduction-rates pool)
                          total-propensity)]
    (-> (reduction-reaction expr reduction)
        (update :product deref))))

(defn sample-map-with-probs
  [m]
  (let [i (rand-int-with-probs (into [] (map val) m))]
    (nth (map key m) i)))

(defn normalize-map
  [m]
  (let [z (reduce + (map val m))]
    (fmap #(/ % z) m)))

(defn with-time-delta
  [reaction total-propensity]
  (let [epsilon 1e-100
        r (+ (rand) epsilon)] ; Avoid numerical instabilities
    (assoc reaction :time-delta (/ (Math/log r) (* -1 total-propensity)))))

(defn gillespie-sample-reaction
  [pool]
  (let [reaction-type->propensity (gillespie-total-propensity-by-type pool)
        total-reaction-probs (normalize-map reaction-type->propensity)
        reaction-type (sample-map-with-probs total-reaction-probs)]
    (-> (case reaction-type
           :cleave (sample-cleave-reaction pool)
           :condense (sample-condense-reaction pool)
           :reduce (sample-reduce-reaction
                    pool
                    (:combinator->reduction-rates pool)
                    (:reduce reaction-type->propensity)))
         (with-time-delta (reduce + (vals reaction-type->propensity)))
         (assoc :reaction-type->propensity reaction-type->propensity))))

(defn apply-reaction
  [pool reaction]
  (let [reactives (get-reactives reaction)
        products (get-products reaction)]
    (if-not (pool-contains-all? pool reactives)
      {:pool pool :reaction nil}
      (-> {:pool pool :reaction reaction}
          (update :pool #(reduce pool-remove % reactives))
          (update :pool #(reduce pool-add % products))
          (call-all-decorators-hook-in [:pool] :apply-reaction)))))

(defn single-thread-evolution
  "Produces an infinite sequence of tuples [pool reaction]"
  ([pool sample-reaction-fn apply-reaction-fn]
   (filter :reaction
           (reductions
            (fn [{pool :pool} step-idx]
              (let [cand-reaction (sample-reaction-fn pool)
                    {pool :pool reaction :reaction} (apply-reaction-fn pool cand-reaction)]
                {:pool pool :reaction reaction}))
            {:pool pool :reaction nil} (range))))
  ([pool sample-reaction-fn]
   (single-thread-evolution pool sample-reaction-fn apply-reaction)))

(defn compute-reactions
  "Continually computes new reactions and places them into a channel to apply
  them later into the pool"
  [state-atom sample-reaction-fn reaction-candidate-chan]
  (a/thread
    (while true
      (let [reaction (sample-reaction-fn (:pool @state-atom))]
        (a/>!! reaction-candidate-chan reaction)))))

(defn parallel-evolution
  [n-threads pool sample-reaction-fn]
  (let [state-atom (atom {:reaction nil :pool pool})
        reaction-candidate-chan (a/chan n-threads)]
    (dotimes [i-thread n-threads]
      (compute-reactions state-atom sample-reaction-fn reaction-candidate-chan))
    (single-thread-evolution
     @state-atom
     (fn parallel-sample-reaction
       [& args]
       (a/<!! reaction-candidate-chan))
     (fn parallel-apply-reaction
       [state cand-reaction]
       (swap! state-atom (fn [{pool :pool}] (apply-reaction pool cand-reaction)))))))

(defn simulated-evolution
  ([pool sample-reaction-fn nthreads]
   (let [evolution-fn
         (if (> nthreads 1)
           (partial parallel-evolution nthreads)
           single-thread-evolution)]
     (evolution-fn
      pool
      sample-reaction-fn))))

(defn scripted-evolution
  "applies a list of reactions to the pool and returns a list of pool-reaction pairs"
  ([pool reactions]
   (reductions #(apply-reaction (:pool %1) %2) {:pool pool :reaction nil} reactions)))

(defn take-until
  [pools-and-reactions [nsteps step-units]]
  (take-while #(<= (-> % :pool step-units) nsteps) pools-and-reactions))

(defn simple-evolve
  ([pool nreactions]
   (-> pool
       (simulated-evolution gillespie-sample-reaction 1)
       (take-until [nreactions :reactions])
       (last)
       (:pool))))

(defn save-pool!
  [pool file]
  (spit file (pr-str (dissoc pool :decorators))))

(defn load-pool
  [file]
  (with-all-decorators
    (clojure.edn/read-string (slurp file))))
