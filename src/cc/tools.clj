(ns cc.tools
  (:gen-class)
  (:require [cc.expression :refer :all]
            [cc.pool :refer :all]
            [cc.lending :refer :all]
            [clojure.string :as s]))

(defn try-reduction-simplest
  "Try first whether we can find a reduction with no lending, and only 
  if we fail we apply reactant lending"
  [pool reactive reactant-lending mutation]
  (some
   #(reduce-simplest reactive
                     (partial cc-surface-reducible? pool %)
                     mutation)
   [no-lending reactant-lending]))

(defn singleton-evolution
  [expr]
  (rest
   (drop-last
    (reductions
     (fn [{pool :pool expr :target :as state} i]
       (if-let [reduction
                (try-reduction-simplest pool
                                        expr
                                        naive-lending
                                        (partial no-mutation nil nil))]
         (let [reaction (reduction-reaction expr reduction)
               {new-reaction :reaction new-pool :pool} (apply-reaction pool
                                                                       reaction
                                                                       naive-lending)]
           (assert new-reaction (str "Reaction failed: " (print-str reaction) "in pool " (print-str pool)))
           {:pool new-pool
            :target (if (and (not (reducible? (:product reaction)))
                             (some reducible? (:by-products reaction)))
                      (first (filter reducible? (:by-products reaction)))
                      (:product reaction))
            :reaction new-reaction})
         (reduced state)))
     {:pool (pool-add empty-pool expr) :target expr}
     (range)))))

(defn symetric-expr-evolution
  [expr]
  (let [full-expr (cc.expression/xapply expr expr)]
    (map (comp print-str :reaction) (singleton-evolution full-expr))))

(defn print-symmetric-expr-evolution
  [expr]
  (print 
    (let [str-expr (-> expr (print-str) (#(subs % 1 (dec (count %)))))] 
      (map prn-str (map #(s/replace % str-expr "A") (take 20 (symetric-expr-evolution expr)))))))

(def self-reproducing-1 '(S I (S (S K) I)))
(def self-reproducing-2 '(S K (S (S K) (S (S K) I))))
; (print-symmetric-expr-evolution '(S (S I I) I) )

; (take 5 (singleton-evolution '(S I I (S I I))))
