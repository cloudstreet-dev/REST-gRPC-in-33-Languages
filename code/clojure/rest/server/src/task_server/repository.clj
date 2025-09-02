(ns task-server.repository
  (:require [task-server.task :as task]
            [clojure.string :as str]))

(defonce ^:private tasks-store (atom {}))

(defn- load-sample-data! []
  (let [task1 (task/create-task {:title "Implement Clojure REST API"
                                  :description "Build a REST API server using Ring and Compojure"
                                  :status :in-progress
                                  :priority :high
                                  :tags ["clojure" "rest" "api"]
                                  :assigned-to "backend-team"})
        task2 (task/create-task {:title "Add core.async support"
                                  :description "Implement async processing with core.async"
                                  :status :pending
                                  :priority :medium
                                  :tags ["clojure" "async" "concurrency"]
                                  :assigned-to "backend-team"})
        task3 (task/create-task {:title "Write Midje tests"
                                  :description "Add comprehensive test coverage using Midje"
                                  :status :pending
                                  :priority :high
                                  :tags ["testing" "quality"]
                                  :assigned-to "qa-team"})]
    (reset! tasks-store {(:id task1) task1
                        (:id task2) task2
                        (:id task3) task3})))

;; Initialize with sample data
(load-sample-data!)

(defn list-tasks
  "List all tasks with optional filtering"
  [{:keys [status assigned-to tags page-size page-token sort-by sort-order]
    :or {page-size 20 page-token 0 sort-by :created-at sort-order :desc}}]
  (let [all-tasks (vals @tasks-store)
        ;; Apply filters
        filtered-tasks (cond->> all-tasks
                        status (filter #(= (keyword status) (:status %)))
                        assigned-to (filter #(= assigned-to (:assigned-to %)))
                        tags (filter (fn [task]
                                     (every? #(contains? (set (:tags task)) %)
                                            (if (string? tags)
                                              (str/split tags #",")
                                              tags)))))
        ;; Sort
        sorted-tasks (sort-by (keyword sort-by)
                             (if (= sort-order :asc) compare #(compare %2 %1))
                             filtered-tasks)
        ;; Paginate
        page-size (min page-size 100)
        page-token (or page-token 0)
        total-count (count sorted-tasks)
        paginated-tasks (take page-size (drop page-token sorted-tasks))
        next-token (when (< (+ page-token page-size) total-count)
                    (+ page-token page-size))]
    {:tasks paginated-tasks
     :total-count total-count
     :page-size page-size
     :next-page-token next-token}))

(defn get-task
  "Get a task by ID"
  [id]
  (get @tasks-store id))

(defn create-task!
  "Create a new task"
  [task-data]
  (let [task (task/create-task task-data)]
    (swap! tasks-store assoc (:id task) task)
    task))

(defn update-task!
  "Update an existing task"
  [id updates]
  (when-let [existing-task (get @tasks-store id)]
    (let [updated-task (task/update-task existing-task updates)]
      (swap! tasks-store assoc id updated-task)
      updated-task)))

(defn update-task-status!
  "Update task status"
  [id status]
  (update-task! id {:status (keyword status)}))

(defn delete-task!
  "Delete a task"
  [id]
  (let [exists? (contains? @tasks-store id)]
    (swap! tasks-store dissoc id)
    exists?))

(defn task-count
  "Get total number of tasks"
  []
  (count @tasks-store))