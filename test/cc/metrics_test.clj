(ns cc.metrics-test
  (:require [clojure.test :refer :all]
            [cc.metrics :refer :all]
            [cc.reaction :refer :all]))

(deftest windowed-counter-test
  (is (= [{:a 0.5}, {:a 0.5 :b 0.5}, {:b 0.5 :c 0.5}, {:c 1.0}]
         (into [] (xf-windowed-counter :time :elem 2) (map #(zipmap [:time :elem] %) (map vector (range) [:a :b :c :c]))))))

(deftest windowed-grouped-counter-test
  (is (= [{:a {:x 0.5}}, {:a {:x 0.5 :y 0.5}}, {:a {:y 0.5} :b {:x 0.5}}]
         (into [] (xf-windowed-grouped-counter :time :elem 2) (map #(zipmap [:time :elem] %) (map vector (range) [[:a :x] [:a :y] [:b :x]]))))))

(def ^:private reactions
  (map #(assoc %1 :reactions %2) [(reduction-reaction '(S I I (S I I))
                                                      {:reactants ['(S I I)]
                                                       :result ['(I (S I I) (I (S I I)))]
                                                       :by-products ['S]
                                                       :type 'S})
                                  (reduction-reaction '(S I I (K I I))
                                                      {:reactants ['(K I I)]
                                                       :result ['(I (K I I) (I (K I I)))]
                                                       :by-products ['S]
                                                       :type 'S})
                                  (reduction-reaction '(S I I (S I I))
                                                      {:reactants ['(S I I)]
                                                       :result ['(I (S I I) (I (S I I)))]
                                                       :by-products ['S]
                                                       :type 'S})]
       (range)))

(deftest xf-reactant-substrates-test
  (is (= ['((S I I) (S I I (S I I))) '((K I I) (S I I (K I I))) '((S I I) (S I I (S I I)))]
         (into [] (comp xf-reactant-substrates (map :reactant-substrate)) reactions))))

(deftest xf-reactants-test
  (testing "basic"
    (is (= ['(S I I) '(K I I) '(S I I)]
           (into [] (comp xf-S-reactants (map :S-reactant)) reactions))))
  (testing "counter"
    (is (= [{'(S I I) 0.5} {'(S I I) 0.5, '(K I I) 0.5} {'(K I I) 0.5, '(S I I) 0.5}]
           (into [] (comp xf-S-reactants (xf-windowed-counter :reactions :S-reactant 2)) reactions)))))

(deftest transjuxt-test
  (testing "basic"
    (is (= [{:dupl 0, :inc 1} {:dupl 2, :inc 2} {:dupl 4, :inc 3}]
           (into [] (transjuxt {:dupl (map #(* 2 %)) :inc (map inc)}) (range 3))))))

(deftest reactants-substrates-count-windowed
  (is (= '[([[(S I I) 0.5] [(S I I (S I I)) 0.5]])
           ([[(K I I) 0.5] [(S I I (K I I)) 0.5]] [[(S I I) 0.5] [(S I I (S I I)) 0.5]])
           ([[(K I I) 0.5] [(S I I (K I I)) 0.5]] [[(S I I) 0.5] [(S I I (S I I)) 0.5]])]
         (into [] (xf-S-reactants-substrates-count-windowed 2 :reactions)
               reactions))))
