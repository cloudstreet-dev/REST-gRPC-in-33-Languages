(ns task-client.api
  (:require [clj-http.client :as http]
            [cheshire.core :as json]))

(def ^:private base-url
  (or (System/getenv "API_URL") "http://localhost:8080/api"))

(defn- handle-response
  "Handle HTTP response and parse JSON"
  [response]
  (let [status (:status response)
        body (when-let [b (:body response)]
               (json/parse-string b true))]
    (if (<= 200 status 299)
      {:success true :data body}
      {:success false :error (or (:error body) (str "Request failed with status " status))})))

(defn list-tasks
  "List all tasks with optional filters"
  [& {:keys [status assigned-to tags page-size page-token sort-by sort-order]
      :or {page-size 20 page-token 0 sort-by "created-at" sort-order "desc"}}]
  (try
    (let [response (http/get (str base-url "/tasks")
                            {:query-params (cond-> {:page-size page-size
                                                   :page-token page-token
                                                   :sort-by sort-by
                                                   :sort-order sort-order}
                                           status (assoc :status status)
                                           assigned-to (assoc :assigned-to assigned-to)
                                           tags (assoc :tags tags))
                             :throw-exceptions false
                             :as :json})]
      (handle-response response))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn get-task
  "Get a specific task by ID"
  [id]
  (try
    (let [response (http/get (str base-url "/tasks/" id)
                            {:throw-exceptions false
                             :as :json})]
      (handle-response response))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn create-task
  "Create a new task"
  [task-data]
  (try
    (let [response (http/post (str base-url "/tasks")
                             {:body (json/generate-string task-data)
                              :content-type :json
                              :throw-exceptions false
                              :as :json})]
      (handle-response response))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn update-task
  "Update an existing task"
  [id updates]
  (try
    (let [response (http/put (str base-url "/tasks/" id)
                            {:body (json/generate-string updates)
                             :content-type :json
                             :throw-exceptions false
                             :as :json})]
      (handle-response response))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn update-task-status
  "Update task status"
  [id status]
  (try
    (let [response (http/patch (str base-url "/tasks/" id "/status")
                              {:body (json/generate-string {:status status})
                               :content-type :json
                               :throw-exceptions false
                               :as :json})]
      (handle-response response))
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn delete-task
  "Delete a task"
  [id]
  (try
    (let [response (http/delete (str base-url "/tasks/" id)
                               {:throw-exceptions false})]
      (if (= 204 (:status response))
        {:success true}
        {:success false :error "Failed to delete task"}))
    (catch Exception e
      {:success false :error (.getMessage e)})))