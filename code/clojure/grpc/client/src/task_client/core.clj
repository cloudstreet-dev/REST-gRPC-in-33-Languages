(ns task-client.core
  (:require [protojure.grpc.client :as grpc.client]
            [clojure.core.async :as async]
            [task-client.demo :as demo])
  (:gen-class))

(defn create-client
  "Create a gRPC client connection"
  [host port]
  (grpc.client/connect {:host host :port port}))

(defn list-tasks
  "List tasks with optional filters (streaming response)"
  [client request]
  (let [output-ch (async/chan 100)]
    (grpc.client/invoke client
                        {:service "tasks.v1.TaskService"
                         :method "ListTasks"
                         :input request
                         :output output-ch})
    output-ch))

(defn get-task
  "Get a single task by ID"
  [client id]
  (grpc.client/invoke client
                      {:service "tasks.v1.TaskService"
                       :method "GetTask"
                       :input {:id id}}))

(defn create-task
  "Create a new task"
  [client task]
  (grpc.client/invoke client
                      {:service "tasks.v1.TaskService"
                       :method "CreateTask"
                       :input {:task task}}))

(defn update-task
  "Update an existing task"
  [client task update-mask]
  (grpc.client/invoke client
                      {:service "tasks.v1.TaskService"
                       :method "UpdateTask"
                       :input {:task task
                               :update_mask update-mask}}))

(defn delete-task
  "Delete a task"
  [client id]
  (grpc.client/invoke client
                      {:service "tasks.v1.TaskService"
                       :method "DeleteTask"
                       :input {:id id}}))

(defn watch-tasks
  "Watch for task changes (bidirectional streaming)"
  [client]
  (let [input-ch (async/chan 10)
        output-ch (async/chan 100)]
    (grpc.client/invoke client
                        {:service "tasks.v1.TaskService"
                         :method "WatchTasks"
                         :input input-ch
                         :output output-ch})
    {:input input-ch :output output-ch}))

(defn -main
  "Main entry point for the gRPC client demo"
  [& args]
  (let [host (or (first args) "localhost")
        port (or (some-> args second Integer/parseInt) 50051)]
    (println (str "Connecting to gRPC server at " host ":" port))
    (let [client (create-client host port)]
      (demo/run-demo client)
      (grpc.client/disconnect client))))