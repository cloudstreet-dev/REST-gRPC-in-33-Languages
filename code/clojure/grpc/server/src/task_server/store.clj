(ns task-server.store
  (:require [clojure.core.async :as async])
  (:import [java.time Instant]
           [java.util UUID]))

;; In-memory task storage
(def tasks (atom {}))

;; Watch channels for real-time updates
(def watch-channels (atom #{}))

(defn generate-id []
  (str "task-" (UUID/randomUUID)))

(defn notify-watchers
  "Notify all watch channels of a task event"
  [event-type task]
  (let [event {:event_type event-type
               :task task
               :timestamp (Instant/now)}]
    (doseq [ch @watch-channels]
      (async/put! ch event))))

(defn list-tasks
  "List all tasks"
  []
  (vals @tasks))

(defn get-task
  "Get a task by ID"
  [id]
  (get @tasks id))

(defn create-task
  "Create a new task"
  [{:keys [title] :as task-data}]
  (let [now (Instant/now)
        id (generate-id)
        task (merge {:id id
                     :status :pending
                     :priority :medium
                     :tags #{}
                     :created-at now
                     :updated-at now}
                    task-data)]
    (swap! tasks assoc id task)
    (notify-watchers :created task)
    task))

(defn update-task
  "Update an existing task"
  [id updates]
  (when (contains? @tasks id)
    (let [updated-task (swap! tasks update id 
                              #(-> %
                                   (merge updates)
                                   (assoc :updated-at (Instant/now))))]
      (notify-watchers :updated (get updated-task id))
      (get updated-task id))))

(defn delete-task
  "Delete a task"
  [id]
  (if-let [task (get @tasks id)]
    (do
      (swap! tasks dissoc id)
      (notify-watchers :deleted task)
      true)
    false))

(defn update-task-status
  "Update task status"
  [id new-status]
  (when-let [task (get @tasks id)]
    (let [updated (update-task id {:status new-status})]
      (notify-watchers :status-changed updated)
      updated)))

(defn create-watch-channel
  "Create a new channel for watching task events"
  []
  (let [ch (async/chan 100)]
    (swap! watch-channels conj ch)
    ch))

(defn close-watch-channel
  "Close and remove a watch channel"
  [ch]
  (swap! watch-channels disj ch)
  (async/close! ch))

(defn watch-all
  "Watch all task changes"
  [ch]
  ;; Channel is already registered, all events will be sent
  nil)

(defn watch-tasks
  "Watch specific tasks by ID"
  [ch task-ids]
  ;; In a real implementation, you'd filter events by task IDs
  ;; For simplicity, we're sending all events
  nil)

(defn watch-assigned
  "Watch tasks assigned to a specific user"
  [ch assigned-to]
  ;; In a real implementation, you'd filter events by assignee
  ;; For simplicity, we're sending all events
  nil)