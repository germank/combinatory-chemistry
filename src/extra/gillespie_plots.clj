(ns extra.gillespie-plots
  (:require [cc.plot :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(def log-path (comment "path to directory containing log files"))
(def dest-path (comment "figures output path"))

(let [options {:top-k 10
               :smoothing 20
               :min-lifetime-pct 20
               :only-keys '[S K I (S S) (K I) (K K) (S K) (S I) (S I I) (S (S I) I)]}]
  (plot-selected! log-path dest-path
                  [:reactants-consumption]
                  (-> options
                      (assoc :apply-weighting false)
                      (assoc :dest-filename "top-reactants-by-consumption.svg")))
  (plot-selected! log-path dest-path
                  [:reactants-consumption]
                  (-> options
                      (assoc :apply-weighting true)
                      (assoc :dest-filename "top-reactants-by-consumption-reweighted.svg"))))

(def selected-reactants '[(S I I)
                          (S I I (S I I))
                          (S (S I) I)
                          (S (S I I) I)
                          (S (S S I) K)
                          (S I (S (S K) I))])

(let [options {:smoothing 20
               :only-keys selected-reactants
               :apply-weighting true
               :dest-filename (str "selected-reactants-consumption.svg")}]
  (plot-selected! log-path dest-path
                  [:reactants-consumption]
                  options))

(let [options {:smoothing 20
               :min-val 0.01
               :exprs selected-reactants
               :dest-filename "reactants-count.svg"}]
  (plot-selected! log-path dest-path
                  [:content-count]
                  options))

(let [options {:smoothing nil
               :apply-weighting true
               :x-scales ["linear"]
               :y-scales ["linear"]
               :metrics [:pool-diversity]
               :labels {:pool-diversity "N. of distinct expressions"}}]
  (plot-scalar-metrics! log-path dest-path
                        options))

(let [options {:smoothing nil
               :apply-weighting true
               :x-scales ["linear"]
               :y-scales ["linear"]
               :metrics [:max-length]
               :labels {:max-length "Max. Expression Length"}}]
  (plot-scalar-metrics! log-path dest-path
                        options))
(let [options {:smoothing nil
               :apply-weighting true
               :x-scales ["linear"]
               :y-scales ["linear"]
               :metrics [:reduction-p]
               :transformation-fn #(update % :reduction-p (fn [x] (when x (* 100 x))))
               :labels {:reduction-p "% of reduction reactions"}}]
  (plot-scalar-metrics! log-path dest-path
                        options))
