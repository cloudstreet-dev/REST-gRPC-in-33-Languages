(ns task-server.core
  (:require [protojure.grpc.server :as grpc.server]
            [protojure.pedestal.core :as protojure.pedestal]
            [protojure.pedestal.interceptors.grpc :as protojure.interceptors]
            [clojure.core.async :as async]
            [task-server.service :as service])
  (:gen-class))

(defn grpc-interceptors []
  (-> protojure.interceptors/interceptors
      (conj {:name ::error-handler
             :error (fn [context error]
                      (println "Error:" error)
                      context)})))

(defn start-server
  "Start the gRPC server on the specified port"
  [port]
  (let [service-map {::grpc.server/port port
                     ::grpc.server/interceptors (grpc-interceptors)}
        server (-> service-map
                   (assoc ::grpc.server/services 
                          [(service/task-service)])
                   grpc.server/start)]
    (println (str "gRPC Server started on port " port))
    server))

(defn -main
  "Main entry point for the gRPC server"
  [& args]
  (let [port (or (some-> args first Integer/parseInt) 50051)
        server (start-server port)]
    ;; Keep the server running
    (Thread/sleep Long/MAX_VALUE)))