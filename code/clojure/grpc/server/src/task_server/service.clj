(ns task-server.service
  (:require [clojure.core.async :as async]
            [task-server.store :as store])
  (:import [com.google.protobuf Timestamp Empty]
           [java.time Instant]))

;; Note: In a real implementation, these would be generated from the .proto file
;; For demonstration, we're defining simplified versions

(defn timestamp->instant [^Timestamp ts]
  (when ts
    (Instant/ofEpochSecond (.getSeconds ts) (.getNanos ts))))

(defn instant->timestamp [^Instant instant]
  (when instant
    (-> (Timestamp/newBuilder)
        (.setSeconds (.getEpochSecond instant))
        (.setNanos (.getNano instant))
        (.build))))

(defn task->proto
  "Convert internal task representation to protobuf"
  [{:keys [id title description status priority tags 
           created-by assigned-to created-at updated-at 
           due-date completed-at]}]
  {:id id
   :title title
   :description (or description "")
   :status (keyword (name status))
   :priority (keyword (name priority))
   :tags (vec tags)
   :created_by (or created-by "")
   :assigned_to (or assigned-to "")
   :created_at (instant->timestamp created-at)
   :updated_at (instant->timestamp updated-at)
   :due_date (instant->timestamp due-date)
   :completed_at (instant->timestamp completed-at)})

(defn proto->task
  "Convert protobuf to internal task representation"
  [{:keys [id title description status priority tags 
           created_by assigned_to created_at updated_at 
           due_date completed_at]}]
  {:id id
   :title title
   :description (when-not (empty? description) description)
   :status (keyword (name status))
   :priority (keyword (name priority))
   :tags (set tags)
   :created-by (when-not (empty? created_by) created_by)
   :assigned-to (when-not (empty? assigned_to) assigned_to)
   :created-at (timestamp->instant created_at)
   :updated-at (timestamp->instant updated_at)
   :due-date (timestamp->instant due_date)
   :completed-at (timestamp->instant completed_at)})

(defn list-tasks
  "Stream tasks based on filter criteria"
  [request output-ch]
  (async/go
    (try
      (let [{:keys [status assigned_to tags]} request
            tasks (store/list-tasks)
            filtered-tasks (cond->> tasks
                             status (filter #(= (:status %) status))
                             (not-empty assigned_to) (filter #(= (:assigned-to %) assigned_to))
                             (not-empty tags) (filter #(every? (:tags %) tags)))]
        (doseq [task filtered-tasks]
          (async/>! output-ch (task->proto task)))
        (async/close! output-ch))
      (catch Exception e
        (println "Error in list-tasks:" e)
        (async/close! output-ch)))))

(defn get-task
  "Get a single task by ID"
  [request]
  (let [{:keys [id]} request
        task (store/get-task id)]
    (if task
      (task->proto task)
      (throw (ex-info "Task not found" {:code :not-found :id id})))))

(defn create-task
  "Create a new task"
  [request]
  (let [{:keys [task]} request
        new-task (store/create-task (proto->task task))]
    (task->proto new-task)))

(defn update-task
  "Update an existing task"
  [request]
  (let [{:keys [task update_mask]} request
        task-id (:id task)
        existing (store/get-task task-id)]
    (if existing
      (let [updates (if (empty? update_mask)
                      (proto->task task)
                      (select-keys (proto->task task) (map keyword update_mask)))
            updated (store/update-task task-id updates)]
        (task->proto updated))
      (throw (ex-info "Task not found" {:code :not-found :id task-id})))))

(defn delete-task
  "Delete a task"
  [request]
  (let [{:keys [id]} request]
    (if (store/delete-task id)
      (Empty.)
      (throw (ex-info "Task not found" {:code :not-found :id id})))))

(defn watch-tasks
  "Bidirectional streaming for watching task changes"
  [input-ch output-ch]
  (async/go
    (let [watch-ch (store/create-watch-channel)]
      (try
        ;; Process incoming watch requests
        (loop []
          (when-let [request (async/<! input-ch)]
            (let [{:keys [task_ids watch_all assigned_to]} request]
              (cond
                watch_all (store/watch-all watch-ch)
                (not-empty task_ids) (store/watch-tasks watch-ch task_ids)
                (not-empty assigned_to) (store/watch-assigned watch-ch assigned_to)))
            (recur)))
        
        ;; Send events to client
        (loop []
          (when-let [event (async/<! watch-ch)]
            (async/>! output-ch event)
            (recur)))
        
        (finally
          (async/close! watch-ch)
          (async/close! output-ch))))))

(defn task-service
  "Define the gRPC TaskService"
  []
  {:service-name "tasks.v1.TaskService"
   :endpoints [{:name "ListTasks"
                :handler list-tasks
                :input-stream false
                :output-stream true}
               {:name "GetTask"
                :handler get-task
                :input-stream false
                :output-stream false}
               {:name "CreateTask"
                :handler create-task
                :input-stream false
                :output-stream false}
               {:name "UpdateTask"
                :handler update-task
                :input-stream false
                :output-stream false}
               {:name "DeleteTask"
                :handler delete-task
                :input-stream false
                :output-stream false}
               {:name "WatchTasks"
                :handler watch-tasks
                :input-stream true
                :output-stream true}]})