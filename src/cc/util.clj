(ns cc.util
  (:require [clojure.core.async :as a]
            [parallel.core :as p]
            ))

(defn dec-or-dissoc-in
  ([m ks n]
   (if (> (get-in m ks) n)
     (update-in m ks #(- % n))
     (let [path (take (dec (count ks)) ks)
           k (nth ks (dec (count ks)))]
       (update-in m path dissoc k))))
  ([m ks]
   (dec-or-dissoc-in m ks 1)))

(defn dec-or-dissoc
  ([m k n]
   (if (> (get m k 0) n)
     (update m k #(- % n))
     (dissoc m k)))
  ([m k]
   (dec-or-dissoc m k 1)))

(defn counts-to-repetitions
  [element-counts]
  (apply concat (map #(repeat (val %) (key %)) element-counts)))

(defn in?
  "true if coll contains elm (https://stackoverflow.com/a/3249777/367489)"
  [coll elm]
  (some #(= elm %) coll))

(defmacro map-values-macro
  [f hmap-coll]
  (let [k (gensym 'k)
        v (gensym 'v)]
    `(into {} (for [[~k ~v] ~hmap-coll] (vector ~k (~f ~v))))))

(defn map-values
  [f hmap-coll]
  (into {} (map #(vector (first %) (f (second %))) hmap-coll)))

(defn filter-values
  [f hmap-coll]
  (into {} (filter #(f (second %)) hmap-coll)))

(defn chan-and-mult
  []
  (let [ch (a/chan 1000)]
    [ch (a/mult ch)]))

(defn full-realize
  [s]
  (doall (seq s)))

(defn slice
  [coll start end]
  (let [l (count coll)]
    (drop-last (- l (min l end)) (drop start coll))))

(defn p-sort-by
  ([key-fn coll cmp]
  (p/sort #(cmp (key-fn %1) (key-fn %2)) coll))
  ([key-fn coll]
   (p-sort-by key-fn coll compare)))

(defn safe-mean
  [vs]
  (float (/ (reduce (fnil + 0 0) vs) (count vs))))

(defn select-keys*
  "https://stackoverflow.com/questions/38893968/how-to-select-keys-in-nested-maps-in-clojure"
  [m paths]
  (into {} (map (fn [p]
                  [(last p) (get-in m p)]))
        paths))

(defn safe-read-string
  [s]
  (if (string? s) (clojure.edn/read-string s) s))

(def inf ##Inf)
