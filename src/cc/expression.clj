(ns cc.expression
  (:gen-class)
  (:require [cc.combinator :refer :all]
            [cc.util :refer :all]
            [random-seed.core :refer :all])
  (:refer-clojure :exclude [rand rand-nth rand-int]))

(defn expression-set-random-seed!
  [seed]
  (set-random-seed! seed))

(defmacro if-not-leaf [expr body] `(if (leaf? ~expr) ~expr ~body))

(defn eager-concat
  ([x y]
   (full-realize (concat x y)))
  ([x y z]
   (full-realize (concat x y z)))
  ([x y z & args]
   (full-realize (apply concat x y z args))))

(def lazy-concat concat)

(defn expression?
  [expr]
  (or (seq? expr) (symbol? expr)))

(defn leaf?
  [expr]
  (assert (expression? expr) (str expr " is not a leaf"))
  (symbol? expr))

(defn compound?
  [expr]
  (not (leaf? expr)))

(defn size
  [expr]
  (comment (if (leaf? expr)
             1
             (apply + (map size expr))))
  (count (filter leaf? (tree-seq compound? identity expr))))

(defn expr-atoms
  [expr]
  (if (leaf? expr)
    {expr 1}
    (apply merge-with + (map expr-atoms expr))))

(def compound? (comp not leaf?))

(defn- uncurry-first-expression
  [expr]
  (assert (expression? expr) (str expr " is not a valid expression"))
  (if-not-leaf
   expr
   (full-realize (let [head (first expr)]
                   (if (leaf? head)
                     expr
                     (concat (uncurry-first-expression head) (rest expr)))))))

(defn- normalize-top
  [expr]
  (assert (expression? expr) (str expr " is not a valid expression"))
  (if-not-leaf
   expr
   (uncurry-first-expression
    (if (= (count expr) 1)
      (first expr)
      expr))))

(defn normalize
  [expr]
  (assert (expression? expr) (str expr " is not a valid expression"))
  (if-not-leaf
   expr
   (normalize-top (map normalize expr))))

(defn xapply
  [expr & args]
  (if (leaf? expr)
    (seq (eager-concat (list expr) args))
    (eager-concat expr args)))

(defn break-top
  [expr]
  [(normalize-top (first expr)) (normalize-top (rest expr))])

(defn break-mid
  [expr]
  [(normalize-top (drop-last expr)) (normalize-top (last expr))])

(defn break-random
  [expr]
  (assert (not (leaf? expr)))
  (let [i (rand-int (- (count expr) 1))
        lhs (normalize-top (take (inc i) expr))
        rhs (normalize-top (drop (inc i) expr))]
    [lhs rhs]))

(defn surface-reducible?
  ([expr]
   (and
    (seq? expr)
    (let [[combinator & args] expr]
      (and (combinator? combinator)
           (>= (count args) (combinator-arity combinator)))))))

(defn reduction-type
  ([expr]
   (and
    (seq? expr)
    (let [[combinator & args] expr]
      (and (>= (count args) (combinator-arity combinator)) combinator)))))

(defn reduce-combinator
  [expr-constructor-fn [head-combinator & args]]
  (let [n (combinator-arity head-combinator)]
    (update
     (apply (ns-resolve 'cc.combinator head-combinator)
            expr-constructor-fn
            (take n args))
     :result
     #(delay (normalize-top (expr-constructor-fn % (drop n args)))))))


(declare all-reductions)

(defn ^:private all-subexpr-reductions
  [expr can-reduce?]
  (for [[i subexpr] (map-indexed vector expr)
        subexpr-reduction (all-reductions subexpr can-reduce?)]
    (let [left-children (take i expr)
          right-children (drop (inc i) expr)]
      (-> subexpr-reduction
          (update :result #(delay 
                             (-> (deref %)
                               list
                               ((partial eager-concat left-children) right-children)))))
      )))

(defn all-reductions
  ([expr can-reduce?]
   (try 
     (if (leaf? expr)
     '()
     (let [subexpr-reductions (all-subexpr-reductions expr can-reduce?)]
       (if (can-reduce? expr)
         (lazy-seq
          (cons (reduce-combinator eager-concat expr)
                 ;(update :result normalize-top))
                subexpr-reductions))
         subexpr-reductions)))
   (catch StackOverflowError ex
     (do
       (binding [*out* *err*]
         (println (str "Stackoverflow while reducing " expr)))
       (list)))))
  ([expr]
   (all-reductions expr surface-reducible?)))


(defn deref-result
  [reduction-result]
  (update reduction-result :result deref)
  )

(defn reducible?
  ([expr can-reduce?]
   (seq (all-reductions expr can-reduce?)))
  ([expr]
   (reducible? expr surface-reducible?)))

(defn reduce-normal
  "Returns the result of reducing an expression in normal order"
  [expr]
  (let [expr-reductions (all-reductions expr)]
    (if-not (empty? expr-reductions)
      (:result (deref-result (first expr-reductions)))
      expr)))

(defn reduce-random
  ([expr can-reduce?]
   (let [reducts (all-reductions expr can-reduce?)]
     (if-not (empty? reducts)
       (deref-result (rand-nth reducts)) )))
  ([expr]
   (reduce-random expr surface-reducible?)))

(defn reduce-simplest
  "Picks the reduction that involves the smallest reactant"
  ([expr can-reduce?]
   (let [reducts (all-reductions expr can-reduce?)]
     (if-not (empty? reducts)
       (deref-result (apply min-key #(reduce + (map size (:reactants %))) (shuffle reducts))))))
  ([expr]
   (reduce-simplest expr surface-reducible?)))

(defn reduction-steps-dfs
  [start match? min-steps max-steps]
  (if (> max-steps 0)
    (if (and (match? start) (= min-steps 0))
      (list start)
      (loop [all-expr-reductions (all-reductions start)]
        (if (seq all-expr-reductions)
          (let [[reduction & rest-reductions] all-expr-reductions]
            (if-let [steps (reduction-steps-dfs (:result (deref-result reduction)) match? (max (dec min-steps) 0) (dec max-steps))]
              (conj steps start)
              (recur rest-reductions)))
          (list))))
    nil))

(defn ^:private traceback-path
  [end prev start]
  (loop [expr end
         prev-expr (get prev expr)
         path (list)]
    (if (and (seq path) (= expr start))
      path
      (recur prev-expr
             (get prev prev-expr)
             (conj path expr)))))

(defn reduction-steps-bfs
  [start match? min-steps max-steps]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         prev {start nil}
         steps {start 0}]
    (let [expr (peek queue)
          steps-to-expr (get steps expr)]
      (if (and (seq queue) (< steps-to-expr max-steps))
        (if (and (match? expr) (>= steps-to-expr min-steps))
          (traceback-path expr prev start)
          (let [expr-reductions (doall (map #((comp :result deref-result) %) (all-reductions expr)))]
            (recur (into (pop queue) (filter #(or (not (contains? prev %)) (nil? (get prev %)))) expr-reductions)
                   (into prev (comp
                               (filter #(or (not (contains? prev %)) (nil? (get prev %))))
                               (map #(vector % expr))) expr-reductions)
                   (into steps (comp
                                (filter #(or (not (contains? steps %)) (= 0 (get steps %))))
                                (map #(vector % (inc steps-to-expr)))) expr-reductions))))))))

(defn reduces-to?
  ([expr target max-steps]
   (seq (reduction-steps-dfs expr #(= % target) 0 max-steps)))
  ([expr target]
   (reduces-to? expr target 50)))

(defn subseq? [haystack needle]
  (some #{needle} (partition (count needle) 1 haystack)))

(defn rec-subseq?
  [haystack needle]
  (if (leaf? haystack)
    (= haystack needle)
    (or (subseq? haystack needle)
        (some #(rec-subseq? % needle) haystack))))

(defn metabolic-cycle
  [expr]
  (defn minimum-size-after-reduction
    [expr']
    (let [reduction-sizes (map #(size ((comp :result deref-result) %)) (all-reductions expr'))]
      (if (seq reduction-sizes) (apply min reduction-sizes) ##Inf)))
  (reduction-steps-bfs expr #(and (rec-subseq? % expr)
                                  (< (size %) (minimum-size-after-reduction %))) 1 10))

(defn prefix-size
  [expr1 expr2]
  (if (= expr1 expr2)
    (size expr1)
    (if (or (leaf? expr1) (leaf? expr2))
      0
      (reduce + (take-while
                 #(> % 0)
                 (map
                  #(apply prefix-size %)
                  (map vector expr1 expr2)))))))

(defn asymptotically-reduces-to?
  ([expr target max-steps tolerance]
   (println expr)
   (if (> max-steps 0)
     (or (> (prefix-size expr target) tolerance)
         (asymptotically-reduces-to?
          (reduce-normal expr)
          (reduce-normal target)
          (dec max-steps)
          tolerance))
     false))
  ([expr target]
   (asymptotically-reduces-to? expr target 1000 50)))
