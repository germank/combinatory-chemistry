(ns cc.formatting
  (:require [clojure.string :as s]
            [cc.expression :refer :all]))

(defn fmt-expr
  [expr]
  (s/replace (str expr) " " ""))

(defn fmt-cap-string
  ([string max-width]
   (if (> (count string) max-width)
     (str (subs string 0 (- max-width 3)) "...")
     string))
  ([string]
   (fmt-cap-string string 90)))

(defn fmt-expr-count
  [[expr c]]
  (str (fmt-expr expr) " " c))

(defn replace-all
  [src from with]
  (let [from (if (s/starts-with? from "(")
               (subs from 1 (dec (count from)))
               from)]
    (loop [src src]
      (let [new-src (s/replace (s/replace src from with)
                               (str "(" with ")")
                               with)]
        (if (= new-src src)
          new-src
          (recur new-src))))))

(def metabolic-cycle-memo (memoize metabolic-cycle))

(defn fmt-metabolic-cycle
  [expr]
  (if-let [mc (metabolic-cycle-memo expr)] (str (fmt-expr expr) ">>" (fmt-expr (last mc))) (fmt-expr expr)))

(defn fmt-S-reactives-count
  [[[reactant reactant-cnt] [substrate substrate-cnt]]]
  (let [str-reactant (fmt-expr reactant)
        str-substrate (fmt-expr substrate)]
    (str "A=" (format "%-30s" (fmt-cap-string str-reactant 30))
         "\t"
         reactant-cnt
         "\t"
         (format "%-30s" (fmt-cap-string
                          (if (> (count str-reactant) 1)
                            (replace-all str-substrate str-reactant "A")
                            str-substrate) 30))
         "\t"
         substrate-cnt)))

(defn fmt-S-reactants-count
  [[reactant cnt]]
  (let [str-reactant (fmt-expr reactant)]
    (str "A=" (format "%-30s" str-reactant) " " cnt)))

(defn fmt-join-strings
  [strings]
  (fmt-cap-string
   (s/join " " (doall strings))))

(defn most-common
  ([n mapping]
   (take n (sort-by #(- (last %)) (seq mapping))))
  ([n]
   (fn [mapping] (doall (take n (sort-by #(- (last %)) (seq mapping)))))))

(defn xf-to-mstr
  [fmt]
  (map #(map fmt %)))
