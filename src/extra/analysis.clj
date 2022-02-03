(ns extra.analysis
  (:gen-class)
  (:require [cc.reaction :refer :all]
            [cc.util :refer :all]
            [cc.cli :refer :all]
            [clojure.core.reducers :as r]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [parallel.core :as p]
            [clj-async-profiler.core :as prof]
            [iota]))

(defn log->S-reactions
  [path]
  (->> (iota/seq (.getPath (io/file path "reactions.log")))
       (r/filter #(and (s/includes? % "==") (s/includes? % "S:")))
       (r/map parse-reaction)))

(defn log->reductions
  [path]
  (->> (iota/seq (.getPath (io/file path "reactions.log")))
       (r/filter #(s/includes? % "=="))
       (r/map parse-reaction)))

(defn group-by-red [f coll]
  "https://gist.github.com/balinterdi/4232044"
  (letfn [(reduce-groups
            ([] {})
            ([g1 g2]
             (merge-with concat g1 g2)))]
    (r/fold reduce-groups (r/map #(hash-map (f %) [%]) coll))))

(defn top-consumers
  ([log-path top-k]
   (->> log-path
        log->S-reactions
        (group-by-red (comp first :reactants))
        (r/map #(vector (key %) (->> (val %)
                                     (map :substrate)
                                     frequencies
                                     (sort-by val >)
                                     (take top-k))))
        (into {})))
  ([log-path]
   (top-consumers log-path 10)))

(defn top-origins
  ([log-path top-k]
   (->> log-path
        log->reductions
        (group-by-red :product)
        (r/map (fn [[k v]] (vector k (->> v
                                          (into []
                                                (comp
                                                 (map #(assoc % :generations nil))
                                                 (map #(assoc % :reactions nil))
                                                 (map pr-str)
                                                 (map #(s/replace % "\"" ""))
                                                 (map #(s/split % #"\|"))
                                                 (map #(nth % 2))
                                                 (map s/trim)))
                                          (frequencies)
                                          (sort-by val >)
                                          (take top-k)))))
        (into {})))
  ([log-path]
   (top-origins log-path 10)))

(defn save-top-consumers!
  ([log-path overwrite]
   (let [fout (io/file log-path "top-consumers.edn")]
     (if (or (not (.exists fout)) overwrite)
       (spit fout (top-consumers log-path)))))
  ([log-path]
   (save-top-consumers! log-path false)))

(defn save-top-origins!
  ([log-path overwrite]
   (let [fout (io/file log-path "top-origins.edn")]
     (if (or (not (.exists fout)) overwrite)
       (spit fout (top-origins log-path)))))
  ([log-path]
   (save-top-origins! log-path false)))

(defn start
  [args options]
  (let [action (first args)]
    (time
     (case action
       "consumers" (save-top-consumers! (:log options) true)
       "origins" (save-top-origins! (:log options) true)))))

(def cli-options
  [[nil "--log LOG_DIRECTORY" "Directory where to store log files"]
   ["-o" "--output FILENAME" "Save results to filename (in html)"]])

(defn -main
  [& args]
  (let [{:keys [options args exit-message ok?]} (validate-args cli-options args)]
    (if (not exit-message)
      (start args options)
      (exit (if ok? 0 1) exit-message))))

(comment (metrics-to-pools-file! "tmp-network/l:none-m:none-s:1000000-g:1000-ls:None-lpk:None/1"))

(comment (remove-pools-from-metrics-file! "tmp-network/l:none-m:none-s:1000000-g:1000-ls:None-lpk:None/1"))

(defn remove-pools-from-metrics-file!
  [path]
  (spit (io/file path "metrics.edn")
        (->> (str path "/metrics.edn")
             (iota/seq)
             (r/map (comp prn-str #(dissoc % :pool-state) read-string))
             (r/fold str))))

(defn metrics-to-pools-file!
  [path]
  (spit (io/file path "pools.edn")
        (->> (str path "/metrics.edn")
             (iota/seq)
             (r/map (comp prn-str :pool-state read-string))
             (r/fold str))))

(defn fix-metrics-with-pool-states!
  [path]
  (when  (not (.exists (io/file path "pools.edn")))
    (metrics-to-pools-file! path)
    (remove-pools-from-metrics-file! path)
    ))

(fix-metrics-with-pool-states! "tmp-network/l:none-m:none-s:1000000-g:1000-ls:None-lpk:None/3")

(comment
  "Writing analysis files"
  (time (save-top-consumers! "tmp-network/l:none-m:none-s:10000-g:1000-ls:None-lpk:None/0" true))
  (time (save-top-origins! "tmp-network/l:none-m:none-s:100000-g:1000-ls:None-lpk:None/0")))

(comment
  (def tmp-top-consumers (read-string (slurp "tmp-network/l:none-m:none-s:1000000-g:1000-ls:None-lpk:None/0/top-consumers.edn")))

  (get tmp-top-consumers '(S (S S I) K)))
