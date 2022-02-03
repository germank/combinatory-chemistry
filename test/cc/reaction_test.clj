(ns cc.reaction-test
  (:require [clojure.test :refer :all]
            [cc.expression :refer :all]
            [cc.reaction :refer :all]))

(deftest test-reduction-reaction
  (let [reactive '(S I K (S I K))
        reaction (reduction-reaction reactive (reduce-random reactive))]
    (is (= '(I (S I K) (K (S I K))) (:product reaction)))
    (is (= reactive (:substrate reaction)))
    (is (= ['S] (:by-products reaction)))
    (is (= ['(S I K)] (:reactants reaction)))))

(deftest test-print-parse
  (testing "print/parse"
    (let [reactive '(S I K (S I K))
          reaction (reduction-reaction reactive (reduce-random reactive))]
      (is (= reaction (parse-reaction (print-str reaction)))))))

(deftest test-parse
  (testing "generic"
    (let  [r (parse-reaction "1 | 0.0 | 0.0 | I + I --> II")]
      (and (is (= ['I 'I] (:reactives r)))
           (is (= ['II] (:products r))))))
  (testing "equality"
    (let  [r (parse-reaction "1 | 0.0 | 0.0 | I + I --> II")
           r2 (parse-reaction "1 | 0.0 | 0.0 |I + I --> II")]
      (is (= r r2))))
  (testing "reduce"
    (let [r (parse-reaction "1 | 0.0 | 0.0 | S: (S I I (S I I)) + (S I I) ==> (I (S I I) (I (S I I))) + S")]
      (and (is (= '(S I I (S I I)) (:substrate r)))
           (is (= ['(S I I)] (:reactants r)))
           (is (= '(I (S I I) (I (S I I))) (:product r)))
           (is (= ['S] (:by-products r)))))
    (let [r (parse-reaction "1 | 0.0 | 0.0 | K: (K S I) ==> S + K + I")]
      (and (is (= '(K S I) (:substrate r)))
           (is (= [] (:reactants r)))
           (is (= 'S (:product r)))
           (is (= ['K 'I] (:by-products r)))))))
