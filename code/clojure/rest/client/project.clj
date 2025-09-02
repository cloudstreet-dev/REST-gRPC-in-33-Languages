(defproject task-client "0.1.0"
  :description "Task Management REST API Client"
  :url "https://github.com/cloudstreet-dev/REST-gRPC-in-33-Languages"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.12.3"]
                 [cheshire "5.11.0"]]
  :main ^:skip-aot task-client.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})