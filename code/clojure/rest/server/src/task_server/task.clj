(ns task-server.task
  (:require [clj-time.core :as time]
            [clj-time.format :as time-format]))

(def ^:private iso-formatter (time-format/formatters :date-time))

(defn- current-time-str []
  (time-format/unparse iso-formatter (time/now)))

(defn- generate-uuid []
  (str (java.util.UUID/randomUUID)))

;; Task status constants
(def task-statuses #{:pending :in-progress :completed :cancelled})

;; Task priority constants  
(def task-priorities #{:low :medium :high :urgent})

(defn create-task
  "Create a new task with default values"
  [{:keys [title description status priority tags assigned-to]
    :or {description ""
         status :pending
         priority :medium
         tags []
         assigned-to ""}}]
  (let [now (current-time-str)]
    {:id (generate-uuid)
     :title title
     :description description
     :status status
     :priority priority
     :tags tags
     :assigned-to assigned-to
     :created-at now
     :updated-at now}))

(defn update-task
  "Update an existing task with new values"
  [task updates]
  (let [updated-task (merge task 
                           (select-keys updates [:title :description :status 
                                                :priority :tags :assigned-to]))]
    (assoc updated-task :updated-at (current-time-str))))

(defn valid-status? [status]
  (contains? task-statuses (keyword status)))

(defn valid-priority? [priority]
  (contains? task-priorities (keyword priority)))

(defn validate-task
  "Validate task data"
  [{:keys [title status priority]}]
  (cond
    (or (nil? title) (empty? title))
    {:valid? false :error "Title is required"}
    
    (and status (not (valid-status? status)))
    {:valid? false :error "Invalid status"}
    
    (and priority (not (valid-priority? priority)))
    {:valid? false :error "Invalid priority"}
    
    :else
    {:valid? true}))