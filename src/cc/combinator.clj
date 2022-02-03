(ns cc.combinator)

(defrecord ReductionResult [result reactants by-products type])

(defmethod print-method ReductionResult [x ^java.io.Writer writer]
  (print-method (str (into {} x)) writer))

(defn I
  [E x]
  (ReductionResult. (list x) [] ['I] 'I))

(defn K
  [E x y]
  (ReductionResult. (list x) [] ['K y] 'K))

(defn S
  ([E x y z]
   (ReductionResult.
    [(E
      (if (seq? x) x (list x))
      (list z
            (E (if (seq? y) y (list y)) (list z))))] [z] ['S] 'S)))

(def all-combinators '[I K S])

(defn combinator?
  [x]
  (and (symbol? x) (some #{x} all-combinators)))

(defn reduced-combinator
  [x]
  [x, [], []])

(defn combinator-arity
  [combinator]
  (case combinator
    I 1
    K 2
    S 3))

