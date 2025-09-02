# Clojure REST API Implementation

This directory contains a REST API implementation in Clojure using Ring and Compojure, demonstrating functional programming on the JVM with Lisp syntax.

## Features

- **Full REST API** with CRUD operations
- **Functional design** with immutable data structures
- **Atom-based state management** for thread-safe operations
- **Ring middleware** for cross-cutting concerns
- **Compojure routing** with elegant DSL
- **REPL-driven development** support

## Prerequisites

- Java 8 or higher
- Leiningen (Clojure build tool)

Install Leiningen:
```bash
# macOS
brew install leiningen

# Linux
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein
```

## Server

The server implements a complete REST API using Ring (HTTP abstraction) and Compojure (routing).

### Running the Server

```bash
cd server
lein run
```

Or for development with auto-reload:
```bash
lein ring server
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks (with filtering and pagination)
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in-progress, completed, cancelled)
- `assigned-to` - Filter by assignee
- `tags` - Filter by tags (comma-separated)
- `page-size` - Number of results per page (default: 20, max: 100)
- `page-token` - Pagination token
- `sort-by` - Sort field (created-at, updated-at, title)
- `sort-order` - Sort order (asc, desc)

## Client

The client provides a functional SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
lein run
```

### Using the Client Library

```clojure
(require '[task-client.api :as api])

;; List all tasks
(api/list-tasks)

;; Create a task
(api/create-task {:title "Learn Clojure"
                  :priority "high"
                  :tags ["clojure" "learning"]})

;; Get a task
(api/get-task task-id)

;; Update a task
(api/update-task task-id {:title "Master Clojure"
                          :priority "urgent"})

;; Update task status
(api/update-task-status task-id "completed")

;; Delete a task
(api/delete-task task-id)
```

## Architecture

### Functional Core, Imperative Shell

```clojure
;; Pure function (functional core)
(defn calculate-priority-score [task]
  (+ (case (:priority task)
       :urgent 4
       :high 3
       :medium 2
       :low 1)
     (if (= (:status task) :in-progress) 1 0)))

;; Side-effecting function (imperative shell)
(defn save-task! [task]
  (swap! task-store assoc (:id task) task))
```

### Ring Middleware Stack

```clojure
(def app
  (-> routes
      (wrap-cors)      ; CORS headers
      (wrap-json-body) ; Parse JSON body
      (wrap-json-response) ; Serialize responses
      (wrap-params)))  ; Parse query params
```

### Atom-based State Management

```clojure
(defonce tasks-store (atom {}))

;; Thread-safe updates
(swap! tasks-store assoc task-id task)
(swap! tasks-store update task-id merge updates)
(swap! tasks-store dissoc task-id)
```

## Clojure Features Demonstrated

### Destructuring

```clojure
;; Map destructuring
(defn create-task [{:keys [title description priority]
                    :or {description "" priority :medium}}]
  ;; Use destructured values
  )

;; Sequential destructuring
(let [[first second & rest] [1 2 3 4 5]]
  ;; first = 1, second = 2, rest = (3 4 5)
  )
```

### Threading Macros

```clojure
;; Thread-first (->)
(-> task
    (assoc :updated-at (now))
    (update :version inc)
    (dissoc :temp-field))

;; Thread-last (->>)
(->> tasks
     (filter #(= (:status %) :pending))
     (map :id)
     (take 10))
```

### Multimethods

```clojure
(defmulti process-task :status)

(defmethod process-task :pending [task]
  (println "Starting task:" (:title task)))

(defmethod process-task :in-progress [task]
  (println "Continuing task:" (:title task)))

(defmethod process-task :default [task]
  (println "Task in final state:" (:status task)))
```

### Protocols

```clojure
(defprotocol Persistable
  (save [this])
  (delete [this])
  (exists? [this]))

(defrecord Task [id title status]
  Persistable
  (save [this]
    (swap! storage assoc (:id this) this))
  (delete [this]
    (swap! storage dissoc (:id this)))
  (exists? [this]
    (contains? @storage (:id this))))
```

## REPL-Driven Development

### Start REPL

```bash
lein repl
```

### Interactive Development

```clojure
;; Load namespace
(require '[task-server.core :as server] :reload)

;; Test functions
(server/create-task {:title "Test task"})

;; Inspect state
@task-server.repository/tasks-store

;; Hot reload code
(require '[task-server.handlers :as handlers] :reload)
```

## Testing

### Unit Tests with clojure.test

```clojure
(ns task-server.task-test
  (:require [clojure.test :refer :all]
            [task-server.task :as task]))

(deftest test-create-task
  (testing "Task creation with defaults"
    (let [task (task/create-task {:title "Test"})]
      (is (= "Test" (:title task)))
      (is (= :pending (:status task)))
      (is (= :medium (:priority task))))))

(deftest test-validate-task
  (testing "Validation"
    (is (= false (:valid? (task/validate-task {}))))
    (is (= true (:valid? (task/validate-task {:title "Valid"}))))))
```

Run tests:
```bash
lein test
```

### Property-Based Testing with test.check

```clojure
(require '[clojure.test.check :as tc]
         '[clojure.test.check.generators :as gen]
         '[clojure.test.check.properties :as prop])

(def task-gen
  (gen/hash-map :title gen/string-alphanumeric
                :priority (gen/elements [:low :medium :high :urgent])
                :status (gen/elements [:pending :in-progress :completed])))

(def prop-task-id-unique
  (prop/for-all [task-data task-gen]
    (let [task1 (create-task task-data)
          task2 (create-task task-data)]
      (not= (:id task1) (:id task2)))))

(tc/quick-check 100 prop-task-id-unique)
```

## Performance Optimization

### Transients for Bulk Operations

```clojure
(defn process-many-tasks [tasks]
  (persistent!
    (reduce (fn [acc task]
              (assoc! acc (:id task) (process task)))
            (transient {})
            tasks)))
```

### Lazy Sequences

```clojure
;; Process large datasets lazily
(defn process-task-file [filename]
  (->> (line-seq (io/reader filename))
       (map parse-task-line)
       (filter valid-task?)
       (map process-task)
       (take 1000))) ; Only process first 1000
```

### Memoization

```clojure
(def expensive-calculation
  (memoize
    (fn [task]
      ;; Expensive computation
      (Thread/sleep 1000)
      (calculate-score task))))
```

## Docker Support

```dockerfile
FROM clojure:openjdk-11-lein

WORKDIR /app

# Cache dependencies
COPY project.clj .
RUN lein deps

# Copy source
COPY . .

EXPOSE 8080
CMD ["lein", "run"]
```

## Advanced Features

### core.async for Concurrency

```clojure
(require '[clojure.core.async :as async])

(defn process-tasks-async [tasks]
  (let [c (async/chan)]
    (async/go-loop [tasks tasks]
      (when-let [task (first tasks)]
        (async/>! c (process-task task))
        (recur (rest tasks))))
    c))
```

### Spec for Data Validation

```clojure
(require '[clojure.spec.alpha :as s])

(s/def ::title (s/and string? #(> (count %) 0)))
(s/def ::status #{:pending :in-progress :completed :cancelled})
(s/def ::priority #{:low :medium :high :urgent})
(s/def ::tags (s/coll-of string?))

(s/def ::task
  (s/keys :req-un [::title]
          :opt-un [::status ::priority ::tags]))

(s/valid? ::task {:title "Test" :status :pending})
```

## Best Practices

1. **Prefer immutability**: Use persistent data structures
2. **Keep functions pure**: Separate side effects
3. **Use destructuring**: Cleaner function signatures
4. **Leverage REPL**: Interactive development
5. **Write idiomatic code**: Follow Clojure conventions
6. **Use threading macros**: Improve readability
7. **Handle nil gracefully**: Use some-> and some->>
8. **Document with docstrings**: Self-documenting code

## Dependencies

### Server
- `ring` - HTTP server abstraction
- `compojure` - Routing DSL
- `ring-json` - JSON middleware
- `ring-cors` - CORS support
- `cheshire` - JSON parsing
- `clj-time` - Date/time handling

### Client
- `clj-http` - HTTP client
- `cheshire` - JSON parsing