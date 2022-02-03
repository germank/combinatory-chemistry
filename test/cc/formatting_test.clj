(ns cc.formatting-test
  (:require [clojure.test :refer :all]
            [cc.formatting :refer :all]))

(deftest fmt-expr-counts-test
  (is (= (list "S 1") (map fmt-expr-count (list ['S 1])))))

(deftest most-common-test
  (is (= (list [:a 5]) (most-common 1 {:a 5 :b 1})))
  (is (= (list [:a 5]) ((most-common 1) {:a 5 :b 1})))
  (is (= (list "S 5") ((comp #(map fmt-expr-count %) (most-common 1)) {'S 5 'I 1}))))

(deftest replace-all-test
  (is (= "(AA)" (replace-all "(SII(SII))" "(SII)" "A"))))
