(ns task-client.core
  (:require [task-client.api :as api]
            [clojure.pprint :as pp])
  (:gen-class))

(defn print-banner []
  (println "╔════════════════════════════════════════════════╗")
  (println "║      Clojure Task Management REST Client       ║")
  (println "║           Testing API Operations               ║")
  (println "╚════════════════════════════════════════════════╝")
  (println))

(defn run-demo []
  ;; 1. List all tasks
  (println "1. Listing all tasks...")
  (let [result (api/list-tasks)]
    (if (:success result)
      (let [data (:data result)]
        (println (str "   Found " (:total-count data) " tasks"))
        (doseq [task (:tasks data)]
          (println (str "   - [" (:id task) "] " (:title task) " (" (:status task) ")"))))
      (println (str "   Error: " (:error result)))))
  
  ;; 2. Create a new task
  (println "\n2. Creating a new task...")
  (let [new-task {:title "Learn Clojure macros"
                  :description "Master macro programming and metaprogramming"
                  :priority "high"
                  :tags ["clojure" "macros" "metaprogramming"]
                  :assigned-to "clojure-team"}
        result (api/create-task new-task)]
    (if (:success result)
      (let [task (:data result)]
        (println (str "   Created task: " (:title task)))
        (println (str "   ID: " (:id task)))
        (println (str "   Priority: " (:priority task)))
        (println (str "   Tags: " (clojure.string/join ", " (:tags task))))
        
        ;; Continue with the created task
        (let [task-id (:id task)]
          ;; 3. Get task details
          (println "\n3. Getting task details...")
          (let [get-result (api/get-task task-id)]
            (if (:success get-result)
              (let [task-detail (:data get-result)]
                (println (str "   Title: " (:title task-detail)))
                (println (str "   Description: " (:description task-detail)))
                (println (str "   Status: " (:status task-detail)))
                (println (str "   Assigned to: " (:assigned-to task-detail))))
              (println (str "   Error: " (:error get-result)))))
          
          ;; 4. Update task status
          (println "\n4. Updating task status to 'in-progress'...")
          (let [status-result (api/update-task-status task-id "in-progress")]
            (if (:success status-result)
              (println (str "   Updated status to: " (get-in status-result [:data :status])))
              (println (str "   Error: " (:error status-result)))))
          
          ;; 5. Update task details
          (println "\n5. Updating task details...")
          (let [updates {:title "Master Clojure core.async"
                        :priority "urgent"}
                update-result (api/update-task task-id updates)]
            (if (:success update-result)
              (let [updated-task (:data update-result)]
                (println (str "   Updated title: " (:title updated-task)))
                (println (str "   Updated priority: " (:priority updated-task))))
              (println (str "   Error: " (:error update-result)))))
          
          ;; 6. Filter tasks by status
          (println "\n6. Filtering tasks by status...")
          (let [filter-result (api/list-tasks :status "in-progress")]
            (if (:success filter-result)
              (let [data (:data filter-result)]
                (println (str "   Found " (:total-count data) " in-progress tasks"))
                (doseq [task (:tasks data)]
                  (println (str "   - " (:title task)))))
              (println (str "   Error: " (:error filter-result)))))
          
          ;; 7. Delete the task
          (println "\n7. Deleting the task...")
          (let [delete-result (api/delete-task task-id)]
            (if (:success delete-result)
              (println "   Task deleted successfully")
              (println (str "   Error: " (:error delete-result)))))
          
          ;; 8. Verify deletion
          (println "\n8. Verifying deletion...")
          (let [verify-result (api/get-task task-id)]
            (if (:success verify-result)
              (println "   Error: Task still exists")
              (if (clojure.string/includes? (str (:error verify-result)) "404")
                (println "   Task not found (as expected)")
                (println (str "   Error: " (:error verify-result))))))))
      (println (str "   Error: " (:error result)))))
  
  (println "\n✅ Demo completed successfully!"))

(defn -main
  "Run the client demo"
  [& args]
  (print-banner)
  (Thread/sleep 1000) ;; Give server time to be ready
  (run-demo))