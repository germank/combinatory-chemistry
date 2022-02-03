(ns cc.core
  (:gen-class)
  (:require [cc.cli :refer :all]
            [cc.logging :refer :all]
            [cc.expression :refer [xapply]]
            [cc.pool :refer :all]
            [cc.reaction :refer :all]
            [cc.util :refer :all]
            [clojure.core.async :as a]
            [clojure.string :as s]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [iota]
            [clj-async-profiler.core :as prof]))

(defn all-set-random-seed!
  [seed]
  (pool-set-random-seed! seed))

(defn get-total-time
  [options]
  (if (:play-log options)
    (time [(->> (:play-log options) iota/seq (r/map (constantly 1)) (r/fold +)) :reactions])
    (let [time-metric (keyword (:time-metric options))]
      [(:time options) time-metric])))

(def library
  {:rho-1 '(S (S I) I)
   :gamma-2 '(S I (S (S K) I))
   :gamma-SK-2 '(S (S K) (S (S K) (S K K)))})

(def living-library
  (into {} (map (fn [[k v]] (vector k (xapply v v))) (seq library))))

(defn play-log
  [pool log-filename]
  (let [reactions
        (->> log-filename
             (clojure.java.io/reader)
             (line-seq)
             (pmap #(try (parse-reaction %) (catch Exception e nil)))
             (remove nil?))]
    (scripted-evolution pool reactions)))

(defn report-evolution!
  [pool-reactions reactions-ch pool-ch]
  (last
   (for [{pool :pool reaction :reaction} pool-reactions]
     (do
       (if (and reaction reactions-ch) (a/>!! reactions-ch reaction))
       (if pool-ch (a/>!! pool-ch pool))
       pool))))

(defn get-starting-pool
  [options]
  (if (:continue options)
    (load-pool (io/file (:log options) "pool.edn"))
    (-> empty-pool
        (add-combinators-uniformly (:size options) (map symbol (s/split (:base options) #"")))
        (pool-add-all (reduce #(apply conj %1 (repeat (second %2) (get living-library (first %2)))) [] (:init options)))
        (with-gillespie-parameters (:condensation-rate options) (:reduction-rate options) (or (:volume options) (:size options)))
        (cond-> (some? (:S-reduction-rate options)) 
          (assoc-in [:combinator->reduction-rates 'S] (:S-reduction-rate options))))))

(defn start
  [options]
  (all-set-random-seed! (:seed options))
  (println "Arguments:")
  (clojure.pprint/pprint options)
  (let [pool (get-starting-pool options)
        [pool-ch pool-mult] (chan-and-mult)
        [reactions-ch reactions-mult] (chan-and-mult)]
    (setup-logs
     reactions-mult
     pool-mult
     (get-total-time options)
     options)
    (let [final-pool
          (if (:play-log options)
            (-> pool
                (play-log (:play-log options))
                (report-evolution! reactions-ch pool-ch))
            (-> pool
                (simulated-evolution gillespie-sample-reaction (:threads options))
                (take-until (get-total-time options))
                (report-evolution! reactions-ch pool-ch)))]
      (when (:log options)
        (save-pool! final-pool (io/file (:log options) "pool.edn"))))
    (while (> (count (.buf reactions-ch)) 0) nil) ;FIXME: remove this line?
    (shutdown-agents)
    (a/close! reactions-ch)
    (a/close! pool-ch)))

(defn -main [& args]
  (let [{:keys [options exit-message ok?]} (validate-args main-cli-options args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (if (:profile options)
        (prof/profile (start options))
        (try (start options)
             (catch Exception e (println "caught exception: " e)))))))
