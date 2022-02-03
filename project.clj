(defproject cc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"] 
                 [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/algo.generic "0.1.3"]
                 [org.clojure/core.async "1.2.603"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [net.cgrand/xforms "0.19.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ; reporting
                 [me.tongfei/progressbar "0.8.1"]
                 [walmartlabs/active-status "0.1.15"]
                 ; debugger
                 [spieden/spyscope "0.1.7" :exclusions [bigml/fipp]] ;excl bc of some boolean? func conflict
                 [fipp "0.6.23"]
                 [postmortem "0.4.0"]
                 [random-seed "1.0.0"]
                 [org.clojure/tools.cli  "1.0.194"]
                 [shams/priority-queue "0.1.2"]
                 [parallel  "0.10"]
                 [iota "1.1.3"]
                 ; profiling
                 [com.clojure-goes-fast/clj-async-profiler  "0.4.1"]
                 ; plotting
                 [metasoarous/oz "1.6.0-alpha34"]
                 [org.clojure/tools.reader  "1.3.6"]]
  :jvm-opts  ["-Xmx200g" "-Xss2M"]
  :injections [(require 'spyscope.core)]
  :main ^:skip-aot cc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {}
             :opt {:jvm-opts ["-XX:MaxJavaStackTraceDepth=-1"]}
             :test {:jvm-opts  ["-XX:MaxJavaStackTraceDepth=-1"]}
             :repl { :jvm-opts ["-Dcom.sun.management.jmxremote"
                     "-Dcom.sun.management.jmxremote.ssl=false"
                     "-Dcom.sun.management.jmxremote.authenticate=false"
                     "-Dcom.sun.management.jmxremote.rmi.hostname=127.0.0.1"
                     "-Dcom.sun.management.jmxremote.rmi.port=43210"
                     "-Dcom.sun.management.jmxremote.hostname=127.0.0.1"
                     "-Dcom.sun.management.jmxremote.port=43210"]}}
  :test-selectors {:default (complement :slow)
                   :slow :all})
