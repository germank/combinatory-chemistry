(ns cc.plot
  (:gen-class)
  (:require [cc.cli :refer :all]
            [cc.expression :refer :all]
            [cc.reaction :refer :all]
            [cc.metrics :refer :all]
            [cc.util :refer :all]
            [clojure.string :as s]
            [clojure.math.numeric-tower :as math :refer  [expt]]
            [clojure.algo.generic.functor :refer [fmap]]
            [oz.core :as oz]
            [iota]
            [parallel.core :as p]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.algo.generic.functor :refer [fmap]]))

(def ^:dynamic *view-port* nil)

(defn str->reactant
  [reaction-str]
  (-> reaction-str
      (s/split #"\|")
      (#(zipmap [:reactions :generations :seconds :S-reactant] %))
      (update :seconds read-string)
      (update :S-reactant
              #(-> %
                   (s/split #"==>")
                   (first)
                   (s/split #"\+")
                   (second)
                   (read-string)))))

(defn log->reactants
  [path]
  (->> (iota/seq (.getPath (io/file path "reactions.log")))
       (r/filter #(and (s/includes? % "==") (s/includes? % "S:")))
       (r/map str->reactant)))

(defn log->arguments
  [path]
  (clojure.edn/read (java.io.PushbackReader. (io/reader (io/file path "args.edn")))))

(defn transpose-maps
  [ms]
  (let [ks (apply clojure.set/union (map keys ms))]
    (reduce (fn [m k] (assoc m k (map #(get % k) ms))) {} ks)))

(defn maps-pooling
  [op]
  (fn op-pooling
    ([m1 m2]
     (reduce-kv (fn [m k v] (assoc m k (op v (get m k 0)))) m1 m2))
    ([] {})))

(defn select-reactants
  [cmp-fn pooling-fn top-k-filter-fn k ms]
  (let [most-frequent-ks
        (->> ms
             ;(reduce (maps-pooling pooling-fn))
             ; parallel version:
             (p/fold (maps-pooling pooling-fn) ; remove non-positive values
                     (maps-pooling pooling-fn))
             (#(if top-k-filter-fn (into {} (comp (filter (comp top-k-filter-fn second))) %) %))
             (p/sort cmp-fn) ; parallel version
             (pmap first)
             (take k))]
    (map #(select-keys % most-frequent-ks) ms)))

(defn filter-by-min-existence
  [n-gens ms]
  (let [existence-count
        (->> ms
             (map #(into {} (filter (fn [[k v]] (and v (> v 0)))) %))
             (map keys)
             (reduce (fn [acc v] (reduce #(update %1 %2 (fnil inc 0)) acc v)) {}))]
    (map
     (fn [m] (select-keys m
                          (filter (fn [k] (>= (get existence-count k 0) n-gens))
                                  (keys m))))
     ms)))

(defn smooth
  [smoothing ms]
  (->> ms
       (partition smoothing 1)
       (map transpose-maps)
       (map (partial fmap safe-mean))))

(defn count-reactants-by-second-serial
  ([reactants]
   (->> reactants
        (into [] (xf:reactants-->reactant->consumption 1)))))

(defn count-reactants-by-second-parallel
  ([reactants]
   (->> reactants
        (p/group-by (comp int :seconds))
        (p-sort-by first)
        (pmap (comp
               frequencies
               #(map :S-reactant %)
               second)))))

(defn alpha
  [expr]
  (-> expr
      pr-str
      (s/replace "(" "")
      (s/replace ")" "")))

(defn log->reactant-consumption-per-second
  ([{:keys [top-k top-k-pooling-fn top-k-cmp-fn top-k-filter-fn window max-length smoothing min-lifetime-pct start end only-keys apply-weighting num-combinators max-time]
     :or {top-k 10
          top-k-pooling-fn max
          top-k-filter-fn nil
          top-k-cmp-fn #(compare (second %2) (second %1))
          window 1
          max-length inf
          smoothing 1
          min-lifetime-pct 10
          start 0
          end inf
          apply-weighting true
          num-combinators 3}} log-path]
   (let [args (log->arguments log-path)]
     (for [[reactant consumptions]
           (-> (log->reactants log-path)
               (cond->> (some? max-time) (r/filter #(< (:seconds %) max-time)))
               (r/foldcat)
               (count-reactants-by-second-parallel)
               (cond->>
                apply-weighting
                 (pmap (partial weight-with
                                (pos-log-info-ratio
                                 (partial logp-S-arg num-combinators))))
                 (not apply-weighting)
                 (pmap (partial weight-with normalizer)))
               (slice start end)
               (cond->
                only-keys (->> (pmap #(select-keys % only-keys)))
                (not only-keys) (->
                                 (cond-> (some? min-lifetime-pct) 
                                   (#(filter-by-min-existence (* (/ min-lifetime-pct 100) (count %)) %)))
                                 (->> (select-reactants top-k-cmp-fn top-k-pooling-fn top-k-filter-fn top-k))))
               (->> (smooth smoothing))
               (transpose-maps))
           [i consumption] (map-indexed vector consumptions)]
       {:second i :order (if only-keys (.indexOf only-keys reactant) (size reactant)) :alpha ((comp pr-str alpha) reactant) :reactant (pr-str reactant) :consumption (or consumption 0)})))
  ([log-path]
   (log->reactant-consumption-per-second log-path {})))

(defn plot-spec-reactants-consumption-per-second
  [{:keys [x-scale apply-weighting color-scheme]
    :or {x-scale "linear"
         color-scheme "tableau10"
         apply-weighting true}} data]
  {:width 400
   :height 300
   :config {:legend {:labelLimit 0 :symbolStrokeWidth 2 :labelFontSize 16 :titleFontSize 18}}
   :data {:values data}
   :encoding {:x {:field "second" :type "quantitative" :title "Time" :scale {:type x-scale} :axis {:titleFontSize 16}}
              :y {:field "consumption" :type "quantitative" :title (if apply-weighting "Pointwise Information" "Consumption Rate") :axis {:titleFontSize 16}}
              :color {:field "reactant" :type "nominal"
                      :scale {:scheme color-scheme}
                      :sort {:field "order"}}
              :tooltip [{:field "consumption" :type "quantitative"}
                        {:field "second" :type "quantitative"}
                        {:field "reactant" :type "nominal"}]}
   :mark "line"})

(defn plot!
  ([out plot-spec]
   (when out
     (do
       (io/make-parents out)
       (oz/export! plot-spec out)))
     (oz/view! plot-spec :port *view-port*))
  ([plot-spec]
   (plot! nil plot-spec)))

(defn plot-reactants-consumption!
  [log-path dest-path {:keys [dest-filename] :or {dest-filename "reactants-consumption.svg"} :as options}]
  (let [dest-fn (if (and dest-path dest-filename)
                  (str dest-path "/" dest-filename))]
    (->> log-path
         (log->reactant-consumption-per-second options)
         (plot-spec-reactants-consumption-per-second options)
         (plot! dest-fn))))

(defn log->length-distribution-per-second
  [max-length log-path]
  (for [{second :seconds length-distr :length-distr}
        (->> (io/file log-path "metrics.edn")
             io/reader
             line-seq
             (map (comp #(select-keys % [:seconds :length-distr])
                        #(update % :seconds clojure.edn/read-string)
                        read-string)))
        [length length-count]
        (seq length-distr) :when (<= length max-length)]
    {:second second :length length :length-count length-count}))

(defn plot-spec-length-distribution-per-second
  [data]
  {:data {:values data}
   :encoding {:x {:field "second" :type "quantitative" :scale {:type "log"}}
              :y {:field "length-count" :title "count" :type "quantitative" :scale {:type "log"}}
              :color {:field "length" :type "nominal"}}
   :mark "line"})

(defn plot-length-distribution!
  [log-path dest-path]
  (let [dest-fn (when dest-path (str dest-path "/length-distribution.svg"))]
    (->> log-path
         (log->length-distribution-per-second 9)
         plot-spec-length-distribution-per-second
      ;(log->combinator-count-per-second)
      ;(plot-spec-combinator-count-per-second)
         (plot! dest-fn))))

(defn log->combinator-count-per-second
  [log-path]
  (for [{second :seconds atomic-mass :pool-atomic-mass}
        (->> (io/file log-path "metrics.edn")
             io/reader
             line-seq
             (map (comp #(select-keys % [:seconds :pool-atomic-mass])
                        #(update % :seconds read-string)
                        read-string)))
        [combinator combinators-count]
        (seq atomic-mass)]
    {:second second :combinator combinator :combinators-count combinators-count}))

(defn plot-spec-combinator-count-per-second
  [data]
  {:data {:values data}
   :encoding {:x {:field "second" :type "quantitative"}
              :y {:field "combinators-count" :type "quantitative"}
              :color {:field "combinator" :type "nominal"}}
   :mark "line"})

(defn plot-combinator-count!
  [log-path dest-path]
  (let [dest-fn (str dest-path "/combinator-count-distribution.svg")]
    (->> log-path
         (log->combinator-count-per-second)
         (plot-spec-combinator-count-per-second)
         (plot! dest-fn))))

(defn log->scalar-per-second
  [k {:keys [smoothing transformation-fn] :or {smoothing 1}} log-path]
  (for [m (->  (io/file log-path "metrics.edn")
               .getPath
               iota/seq
               (->>
                (r/map read-string)
                (r/map #(update % :seconds read-string))
                (r/map (fn [m] (update m k #(if (string? %) (if (= % "-") 0 (read-string %)) %))))
                (r/map #(select-keys % [:seconds k])))
               (cond-> (some? transformation-fn) (->> (r/map transformation-fn)))
               (r/foldcat)
               (cond-> (some? smoothing) (smooth smoothing))
               ;(partition smoothing 1)
               ;(map #(hash-map :generations (mean (map :generations %)) k (mean (map k %))))
)]
    {:second (:seconds m) k (get m k)}))


(defn plot-spec-scalar-per-second
  [k {:keys [labels] :or {labels {}} :as options} data]
  {:data {:values data}
   :encoding {:x {:field "second" :title "Time" :type "quantitative" :scale {:type (:x-scale options)}}
              :y {:field (name k) :title (get labels k (name k)) :type "quantitative" :scale {:type (:y-scale options)}}}
   :tooltip [{:field "second"} {:field (name k)}]
   :mark "line"})


(defn plot-scalar-metrics!
  [log-path dest-path {:keys [metrics x-scales y-scales] :or {metrics [:reduction-p :pool-diversity :mean-length :max-length :pool-size :reduction-p :content-count] x-scales ["linear" "log"] y-scales ["linear" "log"]} :as options}]
  (doall
   (map
    (fn [[metric x-scale y-scale]]
      (let [dest-fn (when dest-path (str dest-path "/" (name metric) "-" x-scale "-" y-scale ".svg"))]
        (->> log-path
             (log->scalar-per-second metric options)
          ;(map #(clojure.set/rename-keys % {:pool-diversity :diversity}))
             (plot-spec-scalar-per-second metric (merge options {:x-scale x-scale :y-scale y-scale}))
             (plot! dest-fn))))
    (for [metric metrics
          x-scale x-scales
          y-scale y-scales]
      [metric x-scale y-scale]))))

(comment
  (->> "tmp-network/gillespie/base-SKI-s:1000000-seconds:1000-rr:10000-cr:1-srr:1000000/0/pools.edn"
    ;clojure.java.io/reader
    ;line-seq
       iota/seq
       (r/map (comp
               (fn [m] (reduce #(assoc %1 %2 (get m %2)) {} [:seconds]))
               #(binding [*data-readers* {'object (constantly 'nope)}] (read-string %))))
       (r/foldcat)
       (first)))

(defn my-select-keys
  [m v]
  (reduce #(assoc %1 %2 (get m %2)) {} v))

(defn log->content-count-per-second
  [{:keys [smoothing exprs min-val relative] :or {smoothing 1 min-val 0 relative false}} log-path]
  (for [m (-> (io/file log-path "pools.edn")
               .getPath
               iota/seq
               (cond->> relative (r/map #(let [Z (reduce + (vals (:content %)))] (fmap (fn [[k v]] [k (/ v Z)]) %))))
               (->> (r/map (comp
                       (fn [m] (merge (my-select-keys m [:seconds])
                                      (my-select-keys (:content m) exprs)))
                       #(binding [*data-readers* {'object (constantly 'nope)}] (read-string %)))))
               (r/foldcat)
               (->> (smooth smoothing)))
        expr (seq exprs)]
    {:second (get m :seconds) :expr (pr-str expr) :expr-count (if (> (or (get m expr)) 0) (get m expr) min-val) :order (.indexOf exprs expr)}))

(defn plot-spec-content-count-per-second
  [data]
  {:data {:values data}
   :encoding {:x {:field "second" :title "Time" :type "quantitative"}
              :y {:field "expr-count" :title "Expression Count" :type "quantitative" :scale {:type "log"}}
              :color {:field "expr" :title "reactant" :type "nominal" :sort {:field "order"}}}
   :mark "line"})

(defn plot-content-count!
  [log-path dest-path {:keys [dest-filename] :or {dest-filename "content-count.svg"} :as options}]
  (let [dest-fn (when dest-path (str dest-path "/" dest-filename))]
    (->> log-path
         (log->content-count-per-second options)
         (plot-spec-content-count-per-second)
         (plot! dest-fn))))

(defn plot-selected!
  [log-path dest-path selected-keys options]
  (let [plot-fns
        (-> {}
            (assoc :reactants-consumption
                   #(plot-reactants-consumption! log-path dest-path options))
            (assoc :scalar-metrics
                   #(plot-scalar-metrics! log-path dest-path options))
            (assoc :length-distribution
                   #(plot-length-distribution! log-path dest-path))
            (assoc :combinator-count
                   #(plot-combinator-count! log-path dest-path))
            (assoc :content-count
                   #(plot-content-count! log-path dest-path options)))]
    (doall (map (fn [f] (f)) (if (= selected-keys :all)
                               (vals plot-fns)
                               ((apply juxt selected-keys) plot-fns))))))

(comment
  (let [log-path "tmp-network/l:none-m:none-s:1000000-g:1000-ls:None-lpk:None/1"
        dest-path (s/replace log-path "tmp-network" "tmp-network/figures")]
    (plot-reactants-consumption! log-path dest-path {:top-k 15 :smoothing 50 :min-lifetime-pct 20 :only-keys ['(S I (S (S K) I)) '(S I I) '(S (S I) I) '(S (S S I) K)] :dest-filename "reactants-consumption-selected.svg"})
    (comment (plot-reactants-consumption! log-path dest-path {:top-k 15 :top-k-cmp-fn #(compare (/ (second %2) (size (first %2))) (/ (second %1) (size (first %1)))) :smoothing 50 :min-lifetime-pct 20}))))

(def cli-options
  [[nil "--log LOG_DIRECTORY" "Directory where to store log files"]
   ["-p" "--port PORT" "The port where to display the plot if no output file is given"
    :default 10666
    :parse-fn #(Integer/parseInt %)]
   ["-k" "--top-k" "Number of reactants to display in the plot"
    :default 10
    :parse-fn #(Integer/parseInt %)]
   ["-m" "--min-lifetime-pct" "Minimum number of seconds that the reactant must be active to be present in the reactans-consumption plot (in %)"
    :default 10
    :parse-fn #(Integer/parseInt %)]
   ["-s" "--smoothing" "Smoothing window"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-o" "--output FILENAME" "Save results to path"]]) (defn start
                                                         [options]
                                                         (binding [*view-port* (:port options)]
                                                           (plot-selected! (:log options) (:output options) :all options)))

(defn -main
  [& args]
  (let [{:keys [options exit-message ok?]} (validate-args cli-options args)]
    (if (not exit-message)
      (start options)
      (exit (if ok? 0 1) exit-message))))
