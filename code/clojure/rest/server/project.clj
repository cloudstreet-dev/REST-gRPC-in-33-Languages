(defproject task-server "0.1.0"
  :description "Task Management REST API Server"
  :url "https://github.com/cloudstreet-dev/REST-gRPC-in-33-Languages"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring/ring-core "1.10.0"]
                 [ring/ring-jetty-adapter "1.10.0"]
                 [ring/ring-json "0.5.1"]
                 [compojure "1.7.0"]
                 [ring-cors "0.1.13"]
                 [cheshire "5.11.0"]
                 [clj-time "0.15.2"]
                 [com.taoensso/timbre "6.2.2"]]
  :main ^:skip-aot task-server.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})