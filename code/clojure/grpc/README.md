# Clojure gRPC Implementation

This directory contains a gRPC implementation in Clojure using protojure and core.async.

## Features

- **Protojure** for idiomatic Clojure gRPC
- **core.async** for streaming support
- **Unary and streaming RPCs** 
- **Bidirectional streaming** for real-time updates
- **Functional interceptors** for cross-cutting concerns
- **Immutable data structures** throughout

## Prerequisites

- Java 11 or higher
- Leiningen 2.9+

Install Leiningen:
```bash
# macOS
brew install leiningen

# Linux
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein

# Verify installation
lein version
```

## Protocol Buffers

The implementation uses a simplified task service definition. In production, you would generate code from the shared .proto file.

## Server

The server implements all gRPC service methods using functional patterns:

### Running the Server

```bash
cd server
lein run
# Or specify port
lein run 50051
```

Or use the provided script:
```bash
./run-server.sh
```

### Service Implementation

```clojure
;; Unary RPC
(defn get-task [request]
  (let [{:keys [id]} request
        task (store/get-task id)]
    (if task
      (task->proto task)
      (throw (ex-info "Task not found" {:code :not-found})))))

;; Server streaming
(defn list-tasks [request output-ch]
  (async/go
    (doseq [task (filter-tasks request)]
      (async/>! output-ch (task->proto task)))
    (async/close! output-ch)))

;; Bidirectional streaming
(defn watch-tasks [input-ch output-ch]
  (async/go
    (loop []
      (when-let [request (async/<! input-ch)]
        (process-watch-request request)
        (recur)))))
```

## Client

The client provides both synchronous and asynchronous APIs:

### Running the Client Demo

```bash
cd client
lein run
# Or specify server
lein run grpc.example.com 50051
```

Or use the provided script:
```bash
./run-client.sh
```

### Client Usage

```clojure
(require '[task-client.core :as client])

;; Connect to server
(def conn (client/create-client "localhost" 50051))

;; Unary call
(let [task (async/<!! (client/get-task conn "task-123"))]
  (println "Got task:" (:title task)))

;; Streaming response
(let [tasks-ch (client/list-tasks conn {:status :pending})]
  (loop []
    (when-let [task (async/<!! tasks-ch)]
      (println "Task:" (:title task))
      (recur))))

;; Bidirectional streaming
(let [{:keys [input output]} (client/watch-tasks conn)]
  ;; Send watch requests
  (async/>!! input {:watch_all true})
  
  ;; Receive events
  (async/go-loop []
    (when-let [event (async/<! output)]
      (println "Event:" (:event_type event))
      (recur))))
```

## Architecture

### Functional Service Definition

Services are defined as data structures:

```clojure
(defn task-service []
  {:service-name "tasks.v1.TaskService"
   :endpoints [{:name "GetTask"
                :handler get-task
                :input-stream false
                :output-stream false}
               {:name "ListTasks"
                :handler list-tasks
                :input-stream false
                :output-stream true}
               {:name "WatchTasks"
                :handler watch-tasks
                :input-stream true
                :output-stream true}]})
```

### Interceptors

Cross-cutting concerns are handled via interceptors:

```clojure
(def error-handler
  {:name ::error-handler
   :error (fn [context error]
            (log/error error "gRPC error")
            (assoc context :response 
                   {:status :internal
                    :message (.getMessage error)}))})

(def auth-interceptor
  {:name ::auth
   :enter (fn [context]
            (if (valid-token? context)
              context
              (throw (ex-info "Unauthorized" {:code :unauthenticated}))))})
```

### State Management

Using atoms for thread-safe state:

```clojure
(def tasks (atom {}))

(defn create-task [task-data]
  (let [id (generate-id)
        task (assoc task-data :id id)]
    (swap! tasks assoc id task)
    task))

(defn update-task [id updates]
  (swap! tasks update id merge updates))
```

## Testing

```clojure
(ns task-server.service-test
  (:require [clojure.test :refer :all]
            [task-server.service :as service]
            [clojure.core.async :as async]))

(deftest test-create-task
  (let [task {:title "Test Task"
              :priority :high}
        created (service/create-task {:task task})]
    (is (not-empty (:id created)))
    (is (= "Test Task" (:title created)))
    (is (= :pending (:status created)))))

(deftest test-streaming
  (let [output-ch (async/chan 10)]
    (service/list-tasks {} output-ch)
    (let [tasks (async/<!! (async/into [] output-ch))]
      (is (sequential? tasks))
      (is (every? :id tasks)))))
```

## Performance Considerations

### Channel Buffers

Configure appropriate buffer sizes for streaming:

```clojure
;; Small buffer for backpressure
(def output-ch (async/chan 10))

;; Larger buffer for batch processing
(def batch-ch (async/chan 1000))

;; Sliding buffer to drop old items
(def events-ch (async/chan (async/sliding-buffer 100)))
```

### Transducers for Efficiency

Use transducers for efficient stream processing:

```clojure
(defn list-tasks-efficient [request output-ch]
  (let [xf (comp (filter #(= (:status %) (:status request)))
                 (map task->proto)
                 (take 100))]
    (async/pipeline 4 output-ch xf (get-all-tasks-ch))))
```

## Deployment

### Standalone JAR

```bash
cd server
lein uberjar
java -jar target/task-grpc-server-0.1.0-standalone.jar
```

### Docker

```dockerfile
FROM clojure:openjdk-17-lein

WORKDIR /app

COPY project.clj .
RUN lein deps

COPY . .
RUN lein uberjar

EXPOSE 50051

CMD ["java", "-jar", "target/task-grpc-server-0.1.0-standalone.jar"]
```

## Best Practices

1. **Use core.async for streaming** - Natural fit for gRPC streams
2. **Leverage immutability** - Thread-safe by default
3. **Functional interceptors** - Composable middleware
4. **Spec for validation** - Runtime contract checking
5. **Transducers for performance** - Efficient stream processing
6. **Proper error handling** - Map exceptions to gRPC status codes

## Dependencies

- `protojure` - Clojure gRPC implementation
- `core.async` - Asynchronous programming
- `io.grpc` - gRPC Java libraries