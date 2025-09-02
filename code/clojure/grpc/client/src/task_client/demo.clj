(ns task-client.demo
  (:require [task-client.core :as client]
            [clojure.core.async :as async])
  (:import [java.time Instant]))

(defn print-banner []
  (println "╔════════════════════════════════════════════════╗")
  (println "║      Clojure Task Management gRPC Client       ║")
  (println "║            Testing gRPC Operations             ║")
  (println "╚════════════════════════════════════════════════╝")
  (println))

(defn instant->timestamp [instant]
  {:seconds (.getEpochSecond instant)
   :nanos (.getNano instant)})

(defn run-demo [grpc-client]
  (print-banner)
  
  ;; 1. Create a task
  (println "1. Creating a new task...")
  (let [new-task {:title "Learn Clojure gRPC"
                  :description "Master gRPC with functional programming"
                  :priority :high
                  :tags ["clojure" "grpc" "functional"]
                  :assigned_to "dev-team"
                  :created_at (instant->timestamp (Instant/now))
                  :updated_at (instant->timestamp (Instant/now))}
        created (async/<!! (client/create-task grpc-client new-task))]
    (println "   Created task:" (:id created) "-" (:title created))
    
    ;; 2. Get the task
    (println "\n2. Getting task details...")
    (let [task (async/<!! (client/get-task grpc-client (:id created)))]
      (println "   Title:" (:title task))
      (println "   Status:" (:status task))
      (println "   Priority:" (:priority task))
      (println "   Tags:" (clojure.string/join ", " (:tags task))))
    
    ;; 3. Update the task
    (println "\n3. Updating task status...")
    (let [updated-task (assoc created :status :in_progress)
          updated (async/<!! (client/update-task grpc-client 
                                                  updated-task 
                                                  ["status"]))]
      (println "   Updated status to:" (:status updated)))
    
    ;; 4. List all tasks (streaming)
    (println "\n4. Listing all tasks (streaming)...")
    (let [list-ch (client/list-tasks grpc-client {})]
      (loop [count 0]
        (if-let [task (async/<!! list-ch)]
          (do
            (println (str "   - [" (:id task) "] " (:title task) 
                          " (" (:status task) ")"))
            (recur (inc count)))
          (println (str "   Total tasks: " count))))
    
    ;; 5. Watch for changes (bidirectional streaming)
    (println "\n5. Setting up task watch...")
    (let [{:keys [input output]} (client/watch-tasks grpc-client)]
      ;; Send watch request
      (async/>!! input {:watch_all true})
      
      ;; Create another task to trigger an event
      (println "   Creating task to trigger watch event...")
      (async/<!! (client/create-task grpc-client 
                                      {:title "Trigger watch event"
                                       :description "This should appear in the watch"
                                       :priority :medium}))
      
      ;; Read a few events
      (println "   Watching for events (3 seconds)...")
      (async/go-loop [timeout-ch (async/timeout 3000)]
        (async/alt!
          output ([event]
                  (when event
                    (println (str "   Event: " (:event_type event) 
                                  " - " (get-in event [:task :title])))
                    (recur timeout-ch)))
          timeout-ch (println "   Watch timeout")))
      
      ;; Wait for events to process
      (Thread/sleep 3500)
      (async/close! input))
    
    ;; 6. Delete the task
    (println "\n6. Deleting the task...")
    (async/<!! (client/delete-task grpc-client (:id created)))
    (println "   Task deleted successfully")
    
    ;; 7. Verify deletion
    (println "\n7. Verifying deletion...")
    (try
      (async/<!! (client/get-task grpc-client (:id created)))
      (println "   Error: Task still exists!")
      (catch Exception e
        (println "   Task not found (as expected)"))))
  
  (println "\n✅ gRPC demo completed successfully!")))