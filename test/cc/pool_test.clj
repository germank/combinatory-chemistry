(ns cc.pool-test
  (:require [clojure.test :refer :all]
            [cc.pool :refer :all]
            [cc.util :refer :all]
            [cc.reaction :refer :all]
            [clojure.core.async :as a]
            [clojure.algo.generic.math-functions :refer [approx=]]))

(deftest adding-removing-test
  (testing "adding"
    (is (= (pool-multiplicity (pool-add empty-pool 'I) 'I) 1))
    (is (= (pool-multiplicity (pool-add empty-pool 'I 5) 'I) 5)))
  (testing "removing"
    (let [pool (pool-add empty-pool 'I 5)]
      (is (= (pool-multiplicity (pool-remove pool 'I) 'I) 4))
      (is (= (:size (pool-remove pool 'I) 'I) 4))
      (is (= (pool-multiplicity (pool-remove pool 'I 5) 'I) 0))
      (is (= (:size (pool-remove pool 'I 5) 'I) 0))
      (is (not (contains? (pool-remove pool 'I 5) 'I))))))

(defn manual-condense-propensity
  [pool]
  (*
   (/ (:condensation-rate pool) (:volume pool))
   (reduce
    +
    (for [x1 (keys (:content pool))
          x2 (keys (:content pool))]
      (if (= x1 x2)
        (* (get-in pool [:content x1]) (dec (get-in pool [:content x1])) 0.5)
        (* (get-in pool [:content x1]) (get-in pool [:content x2])))))))

(deftest gillespie-propensities-test
  (let [pool  (->
               (reduce pool-add empty-pool (repeatedly 1000 (partial rand-expr 4)))
               (as-> pool (reduce (fn [p _] (pool-remove p (first (shuffle (keys (:content p)))))) pool (range 100)))
               (as-> pool (reduce pool-add pool (repeatedly 100 (partial rand-expr 4))))
               (with-gillespie-parameters 1 100 4000))]
    (< (Math/abs (-
                  (float (-> pool manual-condense-propensity))
                  (float (-> pool (gillespie-total-propensity-by-type) :condense)))) 1e-6)))

(def ^:private some-pool
  (-> empty-pool
      (pool-add '(K I I) 3)
      (pool-add '(S I I (S I I)))
      (pool-add 'S)
      (pool-add 'I 2)))

(def ^:private some-reaction
  (reduction-reaction '(S I I (S I I))
                      {:reactants ['(S I I)]
                       :result '(I (S I I) (I (S I I)))
                       :by-products ['S]}))

(def pool-contains-all-test
  (is (pool-contains-all? some-pool (counts-to-repetitions {'S 1 'I 2}))))

(deftest pool-mass-test
  (is (= 18 (pool-mass some-pool))))

(def tested-pool-size 1000)
(def tested-num-reactions 10000)

; (deftest ^:slow evolution-test
;   (testing "emergence")
;   (is (>= (reduce + (map #(pool-multiplicity
;                            (evolve (add-combinators-uniformly empty-pool tested-pool-size) tested-num-reactions)
;                            %) '[(S I I (S I I)) (I (S I I) (I (S I I))) (I (S I I) (S I I)) (S I I (I (S I I))) (I (I (S I I)) (I (I (S I I)))) (I (I (S I I)) (I (S I I)))]))
;           0)))

(deftest conservation-test
  (testing "conservation laws"
    (let [initial-pool (-> empty-pool
                           (with-gillespie-parameters 1 1000 1000)
                           (add-combinators-uniformly 100))
          evolved-pool (simple-evolve initial-pool 10)]
      (is (= (pool-combinators-count initial-pool) (pool-combinators-count evolved-pool))))))

(defn abs [n] (max n (- n)))

(defn similar
  [a b eps]
  (<= (abs (- a b)) eps))

(deftest dying-test
  (is
   (= 2 (-> empty-pool
            (with-gillespie-parameters 1 1 1)
            (pool-add '(S I I (S I I)))
            (simple-evolve 1)
            pool-size))))

