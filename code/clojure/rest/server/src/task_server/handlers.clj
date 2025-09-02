(ns task-server.handlers
  (:require [task-server.repository :as repo]
            [task-server.task :as task]
            [ring.util.response :as response]
            [cheshire.core :as json]))

(defn- json-response
  "Create a JSON response"
  [data & [status]]
  (-> (response/response (json/generate-string data {:key-fn name}))
      (response/content-type "application/json")
      (response/status (or status 200))))

(defn- error-response
  "Create an error response"
  [message status]
  (json-response {:error message} status))

(defn health-handler
  "Health check endpoint"
  [_]
  (json-response {:status "healthy"
                  :service "clojure-task-api"
                  :task-count (repo/task-count)}))

(defn list-tasks-handler
  "List all tasks"
  [request]
  (let [params (:params request)
        filters {:status (:status params)
                 :assigned-to (:assigned_to params)
                 :tags (:tags params)
                 :page-size (when-let [ps (:page_size params)]
                             (Integer/parseInt ps))
                 :page-token (when-let [pt (:page_token params)]
                              (Integer/parseInt pt))
                 :sort-by (keyword (or (:sort_by params) "created-at"))
                 :sort-order (keyword (or (:sort_order params) "desc"))}
        result (repo/list-tasks filters)]
    (json-response (update result :tasks
                          (fn [tasks]
                            (map #(update % :status name)
                                 (map #(update % :priority name) tasks)))))))

(defn get-task-handler
  "Get a specific task"
  [request]
  (let [id (get-in request [:params :id])]
    (if-let [task (repo/get-task id)]
      (json-response (-> task
                        (update :status name)
                        (update :priority name)))
      (error-response "Task not found" 404))))

(defn create-task-handler
  "Create a new task"
  [request]
  (let [task-data (:body request)
        validation (task/validate-task task-data)]
    (if (:valid? validation)
      (let [task (repo/create-task! task-data)]
        (json-response (-> task
                          (update :status name)
                          (update :priority name)) 201))
      (error-response (:error validation) 400))))

(defn update-task-handler
  "Update a task"
  [request]
  (let [id (get-in request [:params :id])
        updates (:body request)]
    (if-let [task (repo/update-task! id updates)]
      (json-response (-> task
                        (update :status name)
                        (update :priority name)))
      (error-response "Task not found" 404))))

(defn update-status-handler
  "Update task status"
  [request]
  (let [id (get-in request [:params :id])
        status (get-in request [:body :status])]
    (cond
      (nil? status)
      (error-response "Status is required" 400)
      
      (not (task/valid-status? status))
      (error-response "Invalid status" 400)
      
      :else
      (if-let [task (repo/update-task-status! id status)]
        (json-response (-> task
                          (update :status name)
                          (update :priority name)))
        (error-response "Task not found" 404)))))

(defn delete-task-handler
  "Delete a task"
  [request]
  (let [id (get-in request [:params :id])]
    (if (repo/delete-task! id)
      (-> (response/response "")
          (response/status 204))
      (error-response "Task not found" 404))))