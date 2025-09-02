(defproject task-grpc-client "0.1.0"
  :description "Task Management gRPC Client in Clojure"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [io.grpc/grpc-netty "1.58.0"]
                 [io.grpc/grpc-protobuf "1.58.0"]
                 [io.grpc/grpc-stub "1.58.0"]
                 [javax.annotation/javax.annotation-api "1.3.2"]
                 [protojure "2.8.2"]
                 [protojure/grpc-client "2.8.2"]
                 [org.clojure/core.async "1.6.681"]]
  :main ^:skip-aot task-client.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})