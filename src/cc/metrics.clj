(ns cc.metrics
  (:require [cc.reaction :refer :all]
            [cc.pool :refer [pool-mass]]
            [cc.util :refer :all]
            [cc.expression :refer [size]]
            [net.cgrand.xforms :as x]
            [clojure.math.numeric-tower :as math :refer  [expt]]
            [clojure.algo.generic.functor :refer [fmap]]
            [net.cgrand.xforms.rfs :as rf]
            [clojure.data.priority-map :refer [priority-map-by]]))

(defn transjuxt
  "Takes a mapping of transducers and returns a transducer that returns a mapping
  of transductions"
  [transducers]
  (comp (x/multiplex transducers)
        (x/partition (count transducers))
        (map #(apply conj {} %))))

(defn zip
  "Takes one or more transducers and returns a transducer that creates a vector
  of each transduction"
  [& transducers]
  (comp (x/multiplex transducers)
        (x/partition (count transducers))))

(defn xf-subsample-indexes
  "returns 1 of every T objects indexed according to f-idx"
  ([T f-idx]
   (comp
                            ; first we remove repetitions
    (x/partition 2 1 (x/into []))
    (filter #(apply not= %))
    (map second)
                            ; then we filter by matching the period
    (filter #(= 0 (mod % T)))))
  ([T]
   (xf-subsample-indexes T identity)))

(defn xf-subsample
  "returns 1 of every T objects"
  [T]
  (comp
   (x/partition T)
   (map first)))

(def xf-counter
  (x/into-by-key [] identity x/count))

(defn xf-windowed-counter
  [time-func elem-key window-size]
  (let [window-size (float window-size)]
    (comp (x/window-by-time (comp #(/ % window-size) time-func) window-size
                            (fn win-ent
                              ([] (priority-map-by >))
                              ([counter] (into (priority-map-by >) counter))
                              ([counter {elem elem-key}] (update counter elem (fnil inc 0))))
                            (fn win-ext [counter {elem elem-key}] (dec-or-dissoc counter elem)))
          (map (fn [m] (fmap #(/ % window-size) m))))))

(defn xf-windowed-grouped-counter
  [time-func elem-key window-size]
  (let [window-size (float window-size)]
    (comp (x/window-by-time (comp #(/ % window-size) time-func) window-size
                            (fn win-ent
                              ([] {})
                              ([counter] (into {} (map (fn [[k emb-map]] [k (into (priority-map-by >) emb-map)])) counter))
                              ([counter {elem elem-key}]
                               (update counter (first elem) (fnil update (priority-map-by >)) (second elem) (fnil inc 0))))
                            (fn win-ext [counter {elem elem-key}] (update counter (first elem) #(dec-or-dissoc % (second elem)))))
          (map (fn [m] (fmap (fn [m'] (fmap #(/ % window-size) m')) m))))))

(def !  (memoize #(apply * (map bigint (range 1  (inc %))))))

(def catalan-numbers
  (map #(/  (!  (* 2 %)) (*  (!  (inc %))  (! %)))  (range)))

(def catalan-number
  ; FIXME optimize
  (memoize (fn [n] (nth catalan-numbers n))))

(def num-expressions-with-len
  (memoize
   (fn N
     ([b l]
      (* (expt b l) (catalan-number (dec l))))
     ([l] (N 3 l)))))

(def gral-harmonic
  (memoize
   (fn gral-harmonic [n m]
     (reduce + (map #(float (/ 1 (expt % m))) (range 1 (inc n)))))))

(defn p-length
  [alpha n x]
  (/ (expt x (- alpha)) (gral-harmonic n alpha)))

(comment (defn p-expr
           ([num-combinators alpha n l]
            (/ (p-length alpha n l) (num-expressions-with-len num-combinators l)))
           ([num-combinators n l]
            (p-expr num-combinators 1.3 n l))))

(defn logp-expr
  ([b omega l]
   (-
    (Math/log omega)
    (* (Math/log (* 4 b)) l)))
  ([omega l]
   (logp-expr 3 omega l)))

(defn logp-S-arg
  ([b l]
   (comment (* (Math/log (* 4 b))
               (- l)))
   (- (Math/log (+ 3 (Math/sqrt 5)))
      (Math/log 2)
      (* (Math/log (+ 6 (* 3 (Math/sqrt 5))))
         l))
   )
  ([l]
   (logp-S-arg 3 l)))

(defn logp-expr-arb
  ([b l]
   (-
    (* (Math/log (* 5 b)) l))))

(defn log-info-ratio
  [log-P]
  (fn log-info-ratio-aux [k v Z]
    (- (Math/log (/ v Z))
       (log-P (cc.expression/size k)))))

(defn pos-log-info-ratio
  [P]
  (let [log-info-ratio-aux (log-info-ratio P)]
    (fn pos-log-info-ratio-aux [k v Z]
      (max 0 (log-info-ratio-aux k v Z)))))

(comment
  (let [omega 10]
    (reduce + (map (fn [l] (* (num-expressions-with-len l) (expt Math/E (logp-expr omega l)))) (range 1 50)))))

(* 3 (expt Math/E (logp-expr 100000 1)))

(defn xf:reactants-->reactant->consumption
  [window-size]
  (comp
   ;xf-S-reactants
   (xf-windowed-counter :generations :S-reactant window-size)))

(defn xf:regularize
  [c]
  (map (fn [m] (into {} (filter #(-> % val (> c))) m))))

(defn weight-with
  [f m]
  (let [Z (reduce + (vals m))]
    (into {} (for [[k v] m] [k (f k v Z)]))))

(defn xf:weight-with
  [f]
  (map (fn [x] (weight-with f x))))

(defn xf:smooth-counts
  [s]
  (comp
   (x/window
    s
    (comp  (partial merge-with +))
    (fn [m mx] (reduce-kv dec-or-dissoc m mx)))
   (map (fn [m] (let [s (float s)] (fmap #(/ % s) m))))
   (drop (dec s))))

(defn xf:filter-by-max-length
  [max-length]
  (map #(into {} (filter (fn [[k v]] (< (size k) max-length))) %)))

(defn normalizer
  [k v Z]
  (/ v Z))

(def xf-S-reactants
  (comp
   (filter reduction?)
   (filter #(= (:type %) 'S))
   (map #(assoc % :S-reactant ((comp first :reactants) %)))))

(def xf-reactant-substrates
  (comp
   (filter reduction?)
   (filter #(= (:type %) 'S))
   (filter #(= (count (get-reactives %)) 2)) ; This doesn't work in case of mutations (FIXME ? I think it does now)
   (map #(assoc % :reactant-substrate ((comp reverse get-reactives) %)))))

(def xf-reactants-lent
  (comp
   (filter reduction?)
   (map :reactant-lent)
   (map #(if % 1 0))))

(def xf-num-reactions
  (comp
   (map-indexed vector)
   (map (comp inc first))))

(defn xf-reduction-ind
  [elem-key]
  (map #(assoc % elem-key (if (reduction? %) 1. 0.))))

(def xf-sum
  (x/reductions +))

(defn xf-windowed-sum
  [time-func window-size]
  (x/window-by-time (comp #(/ % window-size) time-func) window-size + -))

(defn comp-rf
  [rf f]
  (fn
    ([] (rf))
    ([acc] (rf acc))
    ([acc x] (rf acc (f x)))))

(defn xf-windowed-avg
  [time-func elem-key window-size]
  (x/window-by-time (comp #(/ % window-size) time-func) window-size (comp-rf rf/avg elem-key) #(rf/avg %1 (elem-key %2) -1)))

(def xf-diversity
  (comp
   (x/reductions conj #{})
   (map count)))

(defn xf-S-reactions-windowed
  "Returns a list of S-reactions and counts sorted by frequency"
  [window-size]
  ; FIXME BROKEN
  (comp xf-reactant-substrates
        (xf-windowed-counter window-size)
        (map seq)))

(defn xf-S-reactants-count-windowed
  "Returns a list of reactant counts sorted by frequency"
  [window-size]
  (comp
   xf-S-reactants
   (xf-windowed-counter :generations :S-reactant window-size)
   (map seq)))

(defn argmax
  [d]
  (key (apply max-key val d)))

(defn reactant-count-with-most-common-substrate
  [substrates [reactant cnt]]
  (assert (contains? substrates reactant))
  [[reactant cnt] (peek (get substrates reactant))])

(defn xf-S-reactants-substrates-count-windowed
  "Returns a list-of-strings reporting number of reactants used in S-reactions sorted by frequency, with the most commonly used substrate"
  [window-size time-unit]
  (comp xf-reactant-substrates
        (transjuxt {:sorted-reactants (comp
                                       (map #(update % :reactant-substrate first))
                                       (xf-windowed-counter time-unit :reactant-substrate window-size)
                                       (map seq))
                    :reactants-substrates-counts (xf-windowed-grouped-counter time-unit :reactant-substrate window-size)})
        (filter :reactants-substrates-counts) ; must have the map (sometimes not computed )
        (map ;for every state after a new reaction
         #(map ; for every reactant
           (partial reactant-count-with-most-common-substrate (:reactants-substrates-counts %))
           (:sorted-reactants %)))))
