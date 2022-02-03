(ns cc.logging
  (:gen-class)
  (:require [cc.metrics :refer :all]
            [cc.formatting :refer :all]
            [cc.pool :refer :all]
            [cc.util :refer :all]
            [net.cgrand.xforms :as x]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [com.walmartlabs.active-status :as as])
  (:import  [me.tongfei.progressbar ProgressBar]))

(defn chan-and-tap
  ([orig-mult xf buf]
   (let [chan (a/chan buf xf)]
     (a/tap orig-mult chan)
     chan))
  ([orig-mult xf]
   (chan-and-tap orig-mult xf (a/sliding-buffer 1000)))
  ([orig-mult]
   (chan-and-tap orig-mult nil)))

(defn progress-bar
  [total progress-chan]
  (a/go
    (let [pb (ProgressBar. "Progress" total)]
      (loop [i 0]
        (when-let [progress (a/<! progress-chan)]
          (.stepTo pb (int progress))
          (recur (inc i))))
      (.close pb))))

(defn log-progress-console-scrolling
  [metrics-ch]
  (a/go-loop [i 0]
    (let [metrics (a/<! metrics-ch)]
      (println metrics))
    (recur (inc i))))

(defn log-status-board
  [board metric-ch]
  (let [out-chan (as/add-job board)]
    (a/go
      (while true
        (if-let [value (read-string (prn-str (a/<! metric-ch)))]
          (a/>! out-chan value))))))

(defn log-status-board-multiline
  "Reports results on n separate lines"
  [board n metric-ch]
  (let [out-chans (doall (repeatedly n #(as/add-job board)))]
    (a/thread
      (while true
        (let [values (a/<!! metric-ch)]
          (doseq [[out-ch value] (map vector out-chans values)]
            (a/>!! out-ch (str value))))))))

(defn log-file
  [filename metric-ch]
  (io/make-parents filename)
  (a/thread
    (with-open [w (clojure.java.io/writer filename)]
      (while true
        (when-let [value (a/<!! metric-ch)]
          (.write w (str (if (string? value) value (pr-str value)) "\n"))
          (.flush w))))))

(defn make-parents-spit
  [f & args]
  (io/make-parents f)
  (apply spit f args))

(defn activate-loggers
  [board-ch mult loggers]
  (doseq [logger (filter #(= :scalar (:type %)) loggers)]
    (let [{label :label xf :xf fmt :format} logger]
      (log-status-board board-ch (chan-and-tap mult (if label (comp xf (map #(str label ": " (if fmt (format fmt %) %)))) xf))))))

(defn get-time-mult
  [steps-metric reactions-mult pool-mult]
  (comment (a/mult
   (case steps-metric
     :reactions (chan-and-tap reactions-mult xf-num-reactions)
     :generations (chan-and-tap pool-mult (comp (map pool-generation)
                                                (map int)))
     :seconds (chan-and-tap pool-mult (comp (map :seconds) (map int))))))
  (a/mult (chan-and-tap reactions-mult (comp (map steps-metric) (map int)))))

(defn setup-status-board
  [metric-log-cfgs reactions-mult pool-mult total-time time-mult]
  (let [board-ch (as/console-status-board)]
    (let [[multiline-log-cfg & _] (filter #(= :multiline (:type %)) metric-log-cfgs)]
      (when multiline-log-cfg
        (log-status-board-multiline
         board-ch
         10
         (chan-and-tap reactions-mult (:xf multiline-log-cfg)))))
    (activate-loggers board-ch reactions-mult (filter #(= :reactions (:origin %)) metric-log-cfgs))
    (activate-loggers board-ch pool-mult (filter #(= :pool (:origin %)) metric-log-cfgs)))
  (progress-bar total-time (chan-and-tap time-mult)))

(defn all-metric-log-cfgs
  [window-length time-unit]
  {:reactant-substrates {:xf (comp (xf-S-reactants-substrates-count-windowed window-length time-unit) (xf-to-mstr fmt-S-reactives-count)) :origin :reactions :type :multiline}
   :reactions {:xf xf-num-reactions :label "Reactions" :origin :reactions :type :scalar}
   :generations {:xf (map pool-generation) :format "%.2f" :label "Generations" :origin :pool :type :scalar}
   :seconds {:xf (map :seconds) :format "%.2f" :label "Seconds" :origin :pool :type :scalar}
   :propensities {:xf (map :reaction-type->propensity) :label "Reaction propensities" :origin :reactions :type :scalar}
   :norm-propensities {:xf (map (comp normalize-map :reaction-type->propensity)) :label "Reaction propensities" :origin :reactions :type :scalar}
   :pool-mass {:xf (map pool-mass) :label "Mass" :origin :pool :type :scalar}
   :pool-size {:xf (map pool-size) :label "Size" :origin :pool :type :scalar}
   ;:pool-state {:xf (map #(dissoc % :decorators)) :label "Pool" :origin :pool :type :scalar}
   :length-distr {:xf (map (comp #(into (sorted-map) %) :length-distr)) :label "Length distr" :origin :pool :type :scalar}
   :content-count {:xf (map #(get-in % [:content '(S I (S (S K) I))] "-")) :label "SISSKI" :origin :pool :type :scalar}
   :pool-diversity {:xf (map pool-diversity) :label "Diversity" :origin :pool :type :scalar}
   :pool-atomic-mass {:xf (map pool-atomic-multiplicity) :label "Atomic mass" :origin :pool :type :scalar}
   :mean-length {:xf (map pool-mean-length) :label "Mean Length" :origin :pool :format "%.2f" :type :scalar}
   :max-length {:xf (map pool-max-length) :label "Max Length" :origin :pool :format "%.2f" :type :scalar}
   :last-reaction {:xf (map print-str) :label nil :origin :reactions :type :scalar}
   :reactants-diversity {:xf (comp xf-S-reactants xf-diversity) :label "Reactants Diversity" :origin :reactions :type :scalar}
   :reactants-lent {:xf (comp xf-reactants-lent xf-sum) :label "Reactants Lent" :origin :reactions :type :scalar}
   :reduction-p {:xf (comp (xf-reduction-ind :reduction-ind) (xf-windowed-avg time-unit :reduction-ind window-length)) :label "Reduction-P" :format "%.4f" :origin :reactions :type :scalar}})

(def selected-loggers [:reactions
                       :generations
                       :seconds
                       :propensities
                       ;:norm-propensities
                       :reactant-substrates
                       :pool-mass
                       :pool-size
                       ;:pool-state
                       :pool-diversity
                       :pool-atomic-mass
                       :length-distr
                       :content-count
                       ;:reactants-diversity
                       ;:reactants-lent
                       :reduction-p
                       :mean-length
                       :max-length
])

(defn get-metric-log-cfgs
  [report-window time-unit selected-loggers]
  (select-keys (all-metric-log-cfgs report-window time-unit) selected-loggers))

(defn tap-metrics
  [metric-cfgs reactions-mult pool-mult]
  (map-values
   #(chan-and-tap
     (case (:origin %) :reactions reactions-mult :pool pool-mult)
     (if (:format %)
       (comp (:xf %)
             (map (partial format (:format %))))
       (:xf %))
     (a/sliding-buffer 1))
   (filter-values #(in? [:scalar :progress] (:type %)) metric-cfgs)))

(defn get-eta
  [start idx total]
  (let [now (java.time.LocalDateTime/now)]
    (-> (java.time.Duration/between start now)
        (.dividedBy (inc idx))
        (.multipliedBy (- (inc total) (inc idx)))
        (.withNanos 0)
        (.toString)
        ; the following is based on 
        ; https://stackoverflow.com/questions/3471397/how-can-i-pretty-print-a-duration-in-java
        (.substring 2)
        (.replaceAll "(\\d[HMS])(?!$)",  "$1 ")
        (.toLowerCase))))

(defn get-duration
  [start]
  (let [now (java.time.LocalDateTime/now)]
    (-> (java.time.Duration/between start now)
        (.withNanos 0)
        (.toString)
        ; the following is based on 
        ; https://stackoverflow.com/questions/3471397/how-can-i-pretty-print-a-duration-in-java
        (.substring 2)
        (.replaceAll "(\\d[HMS])(?!$)",  "$1 ")
        (.toLowerCase))))

(defn collect-metrics-to-ch
  "given a dictionary of metric-ids to metric-cfg and multiplexers of reactions and the pool, and a signalling channel, it returns a channel that produces 
  a dictionary of metric-ids into their formatted values"
  [metric-cfgs reactions-mult pool-mult sig-ch total-sig-ticks]
  (let [metric-channels (tap-metrics metric-cfgs reactions-mult pool-mult)
        collected-metrics-ch (a/chan)
        start-time (java.time.LocalDateTime/now)]
    (a/go-loop [tick-idx 0]
      (a/<! sig-ch) ; wait for a signal
      (a/put! collected-metrics-ch
              (->
               (zipmap (keys metric-channels)
                       (apply vector (map a/poll! (vals metric-channels))))
               (assoc :eta (get-eta start-time tick-idx total-sig-ticks))
               (assoc :duration (get-duration start-time))))
      (recur (inc tick-idx))) ; read channels
    collected-metrics-ch))

(defn chan-and-tap-at-signal
  [orig-mult sig-ch]
  (let [orig-ch (chan-and-tap orig-mult nil (a/sliding-buffer 1))
        filtered-ch (a/chan)]
    (a/go-loop [tick-idx 0]
      (a/<! sig-ch) ; wait for a signal
      (a/put! filtered-ch
              (a/<! orig-ch))
      (recur (inc tick-idx))) ; read channels
    filtered-ch))

(defn skipping-channel
  "Skips T-1 examples for every one that reports back"
  [orig-mult T]
  (chan-and-tap orig-mult (xf-subsample-indexes T)))

(defn get-reporting-total-count
  [total-time time-unit options]
  (let [reporting-period (:reporting-period options)
        reporting-count (int (/ total-time reporting-period))]
    reporting-count))

(defn get-reporting-ch
  [time-unit time-mult options]
  (skipping-channel time-mult (:reporting-period options)))

(defn setup-logs
  [reactions-mult pool-mult [total-time time-unit] options]
  (let [time-mult (get-time-mult time-unit reactions-mult pool-mult)
        metric-log-cfgs-map (get-metric-log-cfgs
                             (:reporting-period options)
                             time-unit
                             selected-loggers)
        metric-log-cfgs-vec ((apply juxt selected-loggers) metric-log-cfgs-map)]
    (when-not (:scrolling-log options)
      (setup-status-board
       metric-log-cfgs-vec reactions-mult pool-mult total-time time-mult))
    (let [all-metrics-mult
          (delay
           (a/mult
            (let [sig-ch (get-reporting-ch time-unit time-mult options)]
              (collect-metrics-to-ch
               metric-log-cfgs-map reactions-mult pool-mult
               sig-ch
               (get-reporting-total-count total-time time-unit options)))))]
      (when (:scrolling-log options)
        (log-progress-console-scrolling (chan-and-tap @all-metrics-mult)))
      (when (:log options)
        (do
          (make-parents-spit (io/file (:log options) "args.edn") options)
          (log-file (io/file (:log options) "metrics.edn")
                    (chan-and-tap @all-metrics-mult))
          (log-file (io/file (:log options) "pools.edn")
                    (chan-and-tap-at-signal pool-mult 
                                            (get-reporting-ch time-unit time-mult options))))))
    (when (not (:dont-log-reactions options))
      (log-file (io/file (:log options) "reactions.log")
                (chan-and-tap
                 reactions-mult (map print-str))))))
