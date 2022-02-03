(ns cc.test-plot
  (:require [clojure.test :refer :all]
            [clojure.algo.generic.functor :refer [fmap]]
            [cc.metrics :refer :all]
            [cc.plot :refer :all]))

(defn float-vals
  [ms]
  (map (partial fmap float) ms))

(deftest counting-reactants
  (is
   (= '[{(S I I) 1., (S (S I) I) 1.} {(S I I) 1.}]
      (float-vals
       (count-reactants-by-generation-parallel
        [{:S-reactant '(S I I) :generations 0.5}
         {:S-reactant '(S (S I) I) :generations 0.8}
         {:S-reactant '(S I I) :generations 1.4}]))))
  (testing "serial=parallel"
    (let [reactants [{:S-reactant '(S I I) :generations 0.5}
                     {:S-reactant '(S (S I) I) :generations 0.8}
                     {:S-reactant '(S I I) :generations 1.4}
                     {:S-reactant '(S I I) :generations 2.1}
                     {:S-reactant '(S (S I) I) :generations 2.1}]]
      (is
       (=
        (float-vals
         (count-reactants-by-generation-serial
          reactants))
        (float-vals
         (count-reactants-by-generation-parallel
          reactants)))))))

(deftest smoothing
  (let [maps [{:a 4} {:a 2 :b 4} {:a 3 :b 2}]]
    (is
     (=
      (into [] (xf:smooth-counts 2) maps)
      (smooth 2 maps)))))
