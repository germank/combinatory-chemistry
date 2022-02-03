(ns cc.reaction
  (:require [clojure.string :as s]))

(defprotocol Reaction
  (get-reactives [reaction])
  (get-products [reaction])
  (reduction? [reaction]))

(defrecord ReductionReaction [substrate reactants product by-products type seconds generations reactions])

(defn reduction-reaction
  [substrate reduction-result]
  (map->ReductionReaction
   {:substrate substrate
    :reactants (:reactants reduction-result)
    :product (:result reduction-result)
    :by-products (:by-products reduction-result)
    :type (:type reduction-result)
    }))

(extend-type ReductionReaction
  Reaction
  (get-reactives
    [reaction]
    (concat [(:substrate reaction)] (:reactants reaction)))
  (get-products
    [reaction]
    (concat [(:product reaction)] (:by-products reaction)))
  (reduction?
    [reaction]
    true))

(defn pr-gen
  [g]
  (if g (format "%.4f" g) "nil"))

(defmethod print-method ReductionReaction [x ^java.io.Writer writer]
  (print-method (str
                 (pr-str (:reactions x))
                 " | "
                 (pr-gen (:generations x))
                 " | "
                 (pr-gen (:seconds x))
                 " | "
                 (:type x)
                 ": "
                 (s/join " + " (map pr-str (concat [(:substrate x)] (:reactants x))))
                 " ==> "
                 (s/join " + " (map pr-str (concat [(:product x)] (:by-products x)))))
                writer))

(defrecord GenericReaction [reactives products generations reactions])

(defn generic-reaction
  [reactives products]
  (map->GenericReaction
   {:reactives reactives :products products}))

(extend-type GenericReaction
  Reaction
  (get-reactives
    [reaction]
    (:reactives reaction))
  (get-products
    [reaction]
    (:products reaction))
  (reduction?
    [reaction]
    false))

(defn annotate-product
  [reaction product]
  (update :annotated-products reaction (fnil conj []) product))

(defmethod print-method GenericReaction [x ^java.io.Writer writer]
  (print-method (str
                 (pr-str (:reactions x))
                 " | "
                 (pr-gen (:generations x))
                 " | "
                 (pr-gen (:seconds x))
                 " | "
                 (s/join " + " (map pr-str (:reactives x)))
                 " --> "
                 (s/join " + " (map pr-str (:products x))))
                writer))

(defn parse-reduce-reaction
  [reaction-str]
  (let [[reactions generations seconds reaction-str] (s/split reaction-str #"\|")
        [rtype reaction-str] (s/split reaction-str #":")]
    (-> (apply ->ReductionReaction
               (conj
                (vec (apply concat
                            (map
                             #((juxt first rest) (map read-string (s/split % #"\+")))
                             (s/split reaction-str #"==>"))))
                (read-string rtype)
                (read-string seconds)
                (read-string generations)
                (read-string reactions)))
        (update :reactants #(into [] %))
        (update :by-products #(into [] %)))))

(defn parse-generic-reaction
  [reaction-str]
  (let [[reactions generations seconds reaction-str] (s/split reaction-str #"\|")]
    (map->GenericReaction
     (-> (zipmap [:reactives :products] (map
                                         #(map read-string (s/split % #"\+"))
                                         (s/split reaction-str #"-->")))
         (assoc :seconds (read-string seconds))
         (assoc :generations (read-string generations))
         (assoc :reactions (read-string reactions))))))

(defn parse-reaction
  [reaction-str]
  (if (s/includes? reaction-str "==>")
    (parse-reduce-reaction reaction-str)
    (parse-generic-reaction reaction-str)))
