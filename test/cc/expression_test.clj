(ns cc.expression-test
  (:require [clojure.test :refer :all]
            [cc.expression :refer :all]
            [cc.combinator :refer :all]))

(deftest leaf-test
  (testing "combinators are leaves, lists are not"
    (is (leaf? 'S))
    (is (not (leaf? '(S K K))))))

(deftest expr-atoms-test
  (is (= {'S 1, 'I 2}
         (expr-atoms '(S I I)))))

(deftest normalize-test
  (testing "normalizing leaves"
    (is (= (normalize 'S) 'S))
    (is (= (normalize '(S)) 'S))
    (is (= (normalize '((S))) 'S)))
  (testing "uncurrying (private function)"
    (let [uncurry-first-expression #'cc.expression/uncurry-first-expression]
      (is (= (uncurry-first-expression '((S K) K)) '(S K K)))
      (is (= (uncurry-first-expression '((((S K) K) K))) '(S K K K)))))
  (testing "normalizing expressions"
    (is (= (normalize '((S K K))) '(S K K)))
    (is (= (normalize '((S K) K)) '(S K K)))))

(deftest break-test
  (testing "top breaking"
    (is (= (break-top '(S K)) ['S 'K]))
    (is (= (break-top '(S (K S))) ['S '(K S)])))
  (testing "random breaking"
    (is (= (break-random '(S K)) ['S 'K]))
    (is (= (break-random '(S (K S))) ['S '(K S)]))
    (let [broken (break-random '(S K S))]
      (is (or  (= broken ['S '(K S)])
               (= broken ['(S K) 'S])))))
  (testing "(x(y)) -> (x) (y)"
    (is (= (break-mid '(S I)) ['S 'I]))))

(deftest combinatory-logic-test
  (testing "irreducible"
    (is (= (reduce-normal 'I) 'I))
    (is (= (reduce-normal '(K K)) '(K K))))
  (testing "I combinator"
    (is (= (reduce-normal '(a (I x) b)) '(a x b)))
    (is (= (reduce-normal '(a (I x c) b)) '(a (x c) b)))
    (is (= (reduce-normal '(K (I x))) '(K x)))
    (is (= (reduce-normal '(A (S K (I A)))) '(A (S K A)))))

  ; (S I (S (S K) I) (S K (S I (S (S K) I)) (I (S I (S (S K) I))))) )
  (testing "K combinator"
    (is (= (reduce-normal '(a (K x y) b)) '(a x b)))
    (is (= (reduce-normal '(a (K x y c) b)) '(a (x c) b)))
    (is (= (reduce-normal '(K (K x y))) '(K x))))
  (testing "S combinator"
    (is (= (reduce-normal '(a (S x y z) b)) '(a (x z (y z)) b)))
    (is (= (reduce-normal '(a (S x y z c) b)) '(a (x z (y z) c) b)))
    ))

(deftest normalization-test
  (testing "renormalizes"
    (is (= (reduce-normal '(I (K K) K)) '(K K K)))
    (is (= (reduce-normal '(I ((K K) K))) '(K K K)))
    (is (= (reduce-normal '(K (((K K) K) (K K)) K)) '(K K K (K K))))
    (is (= (reduce-normal '(S (K K) S I)) '(K K I (S I))))
    (is (= (reduce-normal '(S S (K K) I)) '(S I (K K I))))))

(deftest all-reductions-test
  (testing "two redexes"
    (is (= 2 (count (all-reductions '(I K (I K) K)))))
    (doseq [reduction (all-reductions '(I K (I K) K))]
      (is (or (= (deref (:result reduction)) '(K (I K) K))
              (= (deref (:result reduction)) '(I K K K)))))
    (is (= 2 (count (all-reductions '(I (S I I) (I (S I I)))))))
    (is (= 18 (count (all-reductions '(I (I (I (I (I (S I I))))) (I (I (I (I (I (I (I (I (I (I (I (I (I (S I I)))))))))))))))))))
    (is (= (deref-result (first (all-reductions '(I x)))) (deref-result (first (all-reductions '(I x))))))))


(deftest stackoverflow-test
  (testing "stack overflow"
    (is (= 1 (count (all-reductions (nth (iterate #(list 'S % 'K) '(I I)) 1000)))))))

(deftest size-test
  (testing "size"
    (is (= (size 'K) 1))
    (is (= (size '(K I)) 2)))
  (testing "large"
    (is (= 3000 (size (nth (iterate #(xapply 'K %) 'K) 2999))))))

(deftest prefix-size-test
  (testing "prefix-size"
    (is (= (prefix-size '(K K S) '(K K I)) 2))
    (is (= (prefix-size '(K (K S)) '(K (K I))) 2))
    (is (= (prefix-size '(K (K (S K))) '(K (K (I K)))) 2))))

(deftest reduces-to-test
  (testing "simple reduction"
    (is (reduces-to? '(I x) 'x))
    (is (reduces-to? '(S I I (S I I)) '(I (S I I) (I (S I I)))))
    (is (reduces-to? '(S I I (S I I)) '(S I I (S I I))))
    (is (not (reduces-to? 'I 'y)))))

(deftest numbers-test
  (testing "numbers"
    (let [succ 'K
          one 'S
          two '(K S)
          three '(K (K S))
          four '(K (K (K S)))]
      (is (reduces-to? (xapply succ one) two))
      (is (reduces-to? (xapply succ two) three))
      (is (reduces-to? (xapply succ three) four)))))

(deftest boolean-test
  (testing "boolean"
    (let [True '(K K)
          False 'K
          And '(S (S (S S)) (K (K K)))
          Or '(S S (K (K K)))
          Not '(S (S K S) (K K))]
      (is (reduces-to? (xapply Or True False) True))
      (is (reduces-to? (xapply Or True True) True))
      (is (reduces-to? (xapply Or False True) True))
      (is (reduces-to? (xapply Or False False) False))
      (is (reduces-to? (xapply And True False) False))
      (is (reduces-to? (xapply And True True) True))
      (is (reduces-to? (xapply And False True) False))
      (is (reduces-to? (xapply And False False) False))
      (is (reduces-to? (xapply Not False) True))
      (is (reduces-to? (xapply Not True) False)))))

(deftest even-odd-test
  (testing "even-odd"
    (let [True '(K K)
          odd '(S (S (K S) K K) (S K K))
          even '(S (S K K) (K K))
          succ '(S (K (S (K (S S (K K))) K)) S (S K K) K)
          one '(S (K (S (K (S S (K K))) K)) S (S K K) (K K))
          two (xapply succ one)
          three (xapply succ two)
          four (xapply succ three)]
      (is (reduces-to? (xapply odd one) True))
      (is (reduces-to? (xapply even two) True))
      (is (reduces-to? (xapply odd three) True))
      (is (reduces-to? (xapply even four) True)))))

(deftest recursion-test
  (testing "Y-combinator"
    (let [Y '(S (K S) K ((S ((S (K (S (K (S S (K K))) K)) S) (S (S (S K K))))) S) (S (K S) K))
          f 'f
          Yf (xapply Y f)
          fYf (xapply f Yf)]
      (is (asymptotically-reduces-to? Yf fYf)))))

(deftest rec-subseq?-test
  (testing "subseq"
    (is (rec-subseq? '(S K I) '(S K I)))
    (is (rec-subseq? '(S I I (S K I)) '(S K I)))
    (is (rec-subseq? '(K (S I I (S K I))) '(S K I)))))

(deftest reductions-steps-test
  (testing "bfs-search"
    (is (= (list 'x) (reduction-steps-bfs '(I x) #(= % 'x) 1 2)))
    (is (= (list '(I x) 'x) (reduction-steps-bfs '(I I x) #(= % 'x) 1 3)))))

(deftest metabolism-test
  (testing "autopoietic"
    (is (= 3 (count (metabolic-cycle '(S I I (S I I)))))))
  (testing "tail-recursive"
    (is (= 4 (count (metabolic-cycle '(S (S I) I (S (S I) I)))))))
  (testing "self-reproductive"
    (is (= 6 (count (metabolic-cycle (xapply '(S I (S (S K) I) (S I (S (S K) I)))))))))
  (testing "dead"
    (is (nil? (metabolic-cycle '(S I I)))))
  (testing "rare"
    (is (nil? (metabolic-cycle '(S K I I)))))
  ;(testing "hanging" (println (metabolic-cycle '(S (S I) (S (S (S I))) (S (S I (K I)))))))
  )
