(ns cc.cli
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [cc.util :refer [in?]]
            [clojure.string :as s]))

(def main-cli-options
  [[nil "--seed SEED" "Random seed"
    :default 42
    :parse-fn #(Integer/parseInt %)]
   [nil "--base COMBINATORS_BASE" "One of SK, SKI"
    :default "SKI"]
   [nil "--size NUMBER" "Number of combinators"
    :default 10000
    :parse-fn #(Integer/parseInt %)]
   [nil "--volume NUMBER" "Volume of the container (by the default, same as size)"
    :default nil
    :parse-fn #(Integer/parseInt %)]
   [nil "--time NUMBER" "Number of time units for the simulation"
    :default 1000
    :parse-fn #(Float/parseFloat %)]
   [nil "--time-metric METRIC" "Defines how to measure time (reactions, generations, seconds)"
    :default "seconds"
    :validate [#(in? ["reactions", "generations", "seconds"] %) "Unknown time metric"]]
   [nil "--condensation-rate RATE" "Sets the condensation rate constant"
    :default 1.
    :parse-fn #(Float/parseFloat %)]
   [nil "--reduction-rate RATE" "Sets the reduction rate constant"
    :default 10000.
    :parse-fn #(Float/parseFloat %)]
   [nil "--S-reduction-rate RATE" "Sets the reduction rate constant for the S combinator"
    :default 1000000.
    :parse-fn #(Float/parseFloat %)]
   [nil "--threads NUMBER" "Number of threads for parallel processing of reactions"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   [nil "--init INITIAL_STRUCTURES" "Initial structures"
    :parse-fn #(-> %
                   (s/split #",")
                   (->>
                    (map (fn [x] (s/split x #":")))
                    (map (fn [[kw c]] (vector (keyword kw) (Integer/parseInt c))))))]
   [nil "--continue" "Load last saved state in the log directory and continue from there"]
   [nil "--log LOG_DIRECTORY" "Directory where to store log files"]
   [nil "--dont-log-reactions" "Logs all reactions to a file unless this flag is set"]
   [nil "--play-log FILENAME" "Follow log file instead of running a simulation"]
   [nil "--scrolling-log" "Print logs line by line rather than in a live board (default mode when on cluster)"
   ; By default if running on a non-interactive cluster job
    :default (and (System/getenv "SLURM_JOB_ID")
                  (not (= (System/getenv "SLURM_JOB_PARTITION") "debug")))]
   [nil "--reporting-period T" "number of time steps every which the scrolling log produces a report line"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   [nil "--jid" nil
    :default (System/getenv "SLURM_JOB_ID")]
   [nil "--profile" "Activate profiling"]
   ["-H" "--help-aux"]])

(defn usage [options-summary]
  (->> ["Combinatory Chemistry simulator"
        ""
        "Usage: cc [options] action"
        ""
        "Options:"
        options-summary]
       (s/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (s/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [cli-options args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help-aux options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      :else ; all good
      {:options options :args arguments})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

