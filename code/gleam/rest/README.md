# Gleam REST API Implementation

This directory contains a REST API implementation in Gleam, demonstrating type-safe BEAM programming with actor-based concurrency.

## Features

- **Mist web framework** for HTTP servers
- **Actor-based state management** using OTP patterns
- **Type-safe pattern matching** with exhaustive checks
- **Immutable data structures** with functional updates
- **Pipe operator** for readable function composition
- **BEAM VM** for fault-tolerant concurrent systems

## Prerequisites

- Gleam 1.0 or higher
- Erlang/OTP 26 or higher

Install Gleam:
```bash
# macOS
brew install gleam

# Ubuntu/Debian
curl -fsSL https://repo.gleam.run/apt/pubkey.gpg | sudo apt-key add -
echo "deb https://repo.gleam.run/apt/ * *" | sudo tee /etc/apt/sources.list.d/gleam.list
sudo apt update
sudo apt install gleam

# Windows (via Scoop)
scoop install gleam

# Verify installation
gleam --version
```

## Server

The server implements a complete REST API using Mist and OTP actors.

### Running the Server

```bash
cd server
gleam run
```

Or use the provided script:
```bash
./run-server.sh
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in-progress, completed, cancelled)
- `assigned_to` - Filter by assignee

## Client

The client provides a comprehensive SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
gleam run
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```gleam
import task_client

pub fn main() {
  // Create client
  let client = task_client.new("http://localhost:8080")
  
  // List all tasks
  let assert Ok(tasks) = task_client.list_tasks(client, None, None)
  
  // Create a task
  let assert Ok(task) = task_client.create_task(
    client,
    "Learn Gleam",
    Some("Master functional programming on BEAM"),
    "high",
    ["gleam", "beam", "functional"],
    Some("developer")
  )
  
  // Update task status
  let assert Ok(updated) = task_client.update_task_status(
    client,
    task.id,
    "in-progress"
  )
  
  // Delete task
  let assert Ok(_) = task_client.delete_task(client, task.id)
}
```

## Architecture

### Type System

Gleam's type system provides exhaustive pattern matching and type safety:

```gleam
// Custom types (algebraic data types)
pub type TaskStatus {
  Pending
  InProgress
  Completed
  Cancelled
}

pub type TaskPriority {
  Low
  Medium
  High
  Urgent
}

// Record types with named fields
pub type Task {
  Task(
    id: String,
    title: String,
    description: Option(String),
    status: TaskStatus,
    priority: TaskPriority,
    tags: List(String),
    assigned_to: Option(String),
    created_at: Float,
    updated_at: Float,
  )
}

// Pattern matching with exhaustive checks
case task.status {
  Pending -> handle_pending()
  InProgress -> handle_in_progress()
  Completed -> handle_completed()
  Cancelled -> handle_cancelled()
  // Compiler ensures all cases are covered
}
```

### Actor-Based State Management

Using OTP actors for concurrent state management:

```gleam
import gleam/otp/actor
import gleam/erlang/process

// Message types for the actor
pub type Message {
  GetTask(reply_with: process.Subject(Option(Task)), id: String)
  CreateTask(reply_with: process.Subject(Task), request: CreateTaskRequest)
  UpdateTask(reply_with: process.Subject(Option(Task)), id: String, updates: UpdateTaskRequest)
  DeleteTask(reply_with: process.Subject(Bool), id: String)
}

// Actor state
type State {
  State(tasks: Dict(String, Task), counter: Int)
}

// Start the actor
pub fn start() {
  actor.start(State(dict.new(), 0), handle_message)
}

// Handle messages
fn handle_message(message: Message, state: State) {
  case message {
    GetTask(reply_with, id) -> {
      let task = dict.get(state.tasks, id) |> option.from_result
      process.send(reply_with, task)
      actor.continue(state)
    }
    // ... other message handlers
  }
}

// Type-safe RPC calls
pub fn get_task(store: process.Subject(Message), id: String) -> Option(Task) {
  process.call(store, fn(reply) { GetTask(reply, id) }, 1000)
}
```

### Pipe Operator

The pipe operator (`|>`) enables readable function composition:

```gleam
// Traditional nested function calls
json.to_string(json.object(list.map(tasks, task_to_json)))

// With pipe operator - reads left to right
tasks
|> list.map(task_to_json)
|> json.array
|> json.to_string

// Complex data transformations
request.target
|> string.split("?")
|> list.first
|> result.unwrap("/")
|> string.split("/")
|> list.filter(fn(s) { s != "" })
```

## Gleam Features Demonstrated

### Pattern Matching

```gleam
// Match on multiple values simultaneously
case method, path {
  http.Get, ["api", "tasks"] -> handle_list_tasks()
  http.Get, ["api", "tasks", id] -> handle_get_task(id)
  http.Post, ["api", "tasks"] -> handle_create_task()
  http.Put, ["api", "tasks", id] -> handle_update_task(id)
  http.Delete, ["api", "tasks", id] -> handle_delete_task(id)
  _, _ -> handle_not_found()
}

// Guards in patterns
case task {
  Task(priority: Urgent, status: Pending) -> alert_urgent_task()
  Task(status: Completed) -> archive_task()
  _ -> process_normally()
}
```

### Result and Option Types

```gleam
// Result type for error handling
pub fn parse_json(text: String) -> Result(Task, String) {
  case json.decode(text, task_decoder) {
    Ok(task) -> Ok(task)
    Error(_) -> Error("Invalid JSON format")
  }
}

// Option type for nullable values
pub fn find_task(id: String) -> Option(Task) {
  dict.get(tasks, id)
  |> option.from_result
}

// Chaining with use syntax
pub fn process_task(id: String) -> Result(String, String) {
  use task <- result.try(find_task(id) |> option.to_result("Not found"))
  use validated <- result.try(validate_task(task))
  use processed <- result.try(perform_processing(validated))
  Ok("Processed: " <> processed.id)
}
```

### Immutable Updates

```gleam
// Record update syntax
let updated_task = Task(..task, status: Completed, updated_at: now())

// Immutable collection updates
let new_tasks = dict.insert(tasks, task.id, task)
let filtered = list.filter(tasks, fn(t) { t.status != Cancelled })

// Functional state updates
fn update_task_in_state(state: State, id: String, updates: UpdateRequest) {
  case dict.get(state.tasks, id) {
    Ok(task) -> {
      let updated = apply_updates(task, updates)
      let new_tasks = dict.insert(state.tasks, id, updated)
      State(..state, tasks: new_tasks)
    }
    Error(_) -> state
  }
}
```

### Type Inference

```gleam
// Gleam infers types automatically
let tasks = []  // Inferred as List(a)
let count = list.length(tasks)  // Inferred as Int

// Explicit type annotations when needed
let empty_tasks: List(Task) = []

// Generic functions with type parameters
pub fn map_tasks(tasks: List(Task), f: fn(Task) -> a) -> List(a) {
  list.map(tasks, f)
}
```

## Testing

### Unit Tests

```gleam
import gleeunit
import gleeunit/should
import task_store

pub fn main() {
  gleeunit.main()
}

pub fn create_task_test() {
  let assert Ok(store) = task_store.start()
  
  let request = CreateTaskRequest(
    title: "Test Task",
    description: Some("Test Description"),
    priority: High,
    tags: ["test"],
    assigned_to: None
  )
  
  let task = task_store.create_task(store, request)
  
  task.title
  |> should.equal("Test Task")
  
  task.status
  |> should.equal(Pending)
  
  task.priority
  |> should.equal(High)
}

pub fn update_status_test() {
  let assert Ok(store) = task_store.start()
  let task = create_test_task(store)
  
  let assert Some(updated) = task_store.update_task_status(
    store,
    task.id,
    InProgress
  )
  
  updated.status
  |> should.equal(InProgress)
  
  updated.updated_at
  |> should.not_equal(task.updated_at)
}

pub fn concurrent_operations_test() {
  let assert Ok(store) = task_store.start()
  
  // Create multiple tasks concurrently
  let tasks = list.range(1, 100)
    |> list.map(fn(i) {
      process.start(fn() {
        create_task_with_title(store, "Task " <> int.to_string(i))
      })
    })
    |> list.map(process.receive)
  
  list.length(tasks)
  |> should.equal(100)
  
  // All tasks should have unique IDs
  let ids = list.map(tasks, fn(t) { t.id })
  list.unique(ids)
  |> list.length
  |> should.equal(100)
}
```

## Error Handling

Gleam uses the Result type for explicit error handling:

```gleam
// Define custom error types
pub type TaskError {
  NotFound(id: String)
  ValidationError(message: String)
  NetworkError(reason: String)
  ParseError(details: String)
}

// Functions return Result types
pub fn get_task_safe(id: String) -> Result(Task, TaskError) {
  case dict.get(tasks, id) {
    Ok(task) -> Ok(task)
    Error(_) -> Error(NotFound(id))
  }
}

// Handle errors explicitly
case get_task_safe(id) {
  Ok(task) -> process_task(task)
  Error(NotFound(id)) -> io.println("Task " <> id <> " not found")
  Error(ValidationError(msg)) -> io.println("Validation failed: " <> msg)
  Error(_) -> io.println("An error occurred")
}

// Chain operations with use syntax
pub fn complex_operation(id: String) -> Result(String, TaskError) {
  use task <- result.try(get_task_safe(id))
  use validated <- result.try(validate_task(task))
  use processed <- result.map(process_task(validated))
  "Success: " <> processed.id
}
```

## Concurrency Patterns

### Supervisors

```gleam
import gleam/otp/supervisor

pub fn start_link() {
  supervisor.start_link(fn(children) {
    children
    |> supervisor.add(supervisor.worker(task_store.start))
    |> supervisor.add(supervisor.worker(metrics_collector.start))
    |> supervisor.add(supervisor.worker(cache_manager.start))
  })
}
```

### Parallel Processing

```gleam
import gleam/list
import gleam/erlang/process

// Process tasks in parallel
pub fn process_all_tasks(tasks: List(Task)) -> List(Result(ProcessedTask, Error)) {
  tasks
  |> list.map(fn(task) {
    process.start(fn() { process_task(task) })
  })
  |> list.map(fn(pid) {
    process.receive(pid, 5000)
  })
}

// Fan-out/fan-in pattern
pub fn bulk_update(ids: List(String), status: TaskStatus) {
  let store = get_store()
  
  // Fan-out: spawn process for each update
  let processes = list.map(ids, fn(id) {
    process.start(fn() {
      task_store.update_task_status(store, id, status)
    })
  })
  
  // Fan-in: collect results
  let results = list.map(processes, process.receive)
  
  // Return summary
  let successful = list.filter(results, option.is_some)
  #(list.length(successful), list.length(ids))
}
```

## Best Practices

1. **Use exhaustive pattern matching**: Let the compiler catch missing cases
2. **Prefer immutability**: Use record update syntax instead of mutation
3. **Leverage the type system**: Use custom types for domain modeling
4. **Handle errors explicitly**: Use Result and Option types
5. **Use actors for state**: Encapsulate mutable state in actors
6. **Compose with pipes**: Use the pipe operator for readable transformations
7. **Write property-based tests**: Use generators for comprehensive testing

## Docker Support

```dockerfile
FROM ghcr.io/gleam-lang/gleam:v1.0.0-erlang-alpine

WORKDIR /app

# Copy project files
COPY server/gleam.toml server/gleam.toml
COPY server/manifest.toml server/manifest.toml

# Build dependencies
RUN cd server && gleam deps download

# Copy source code
COPY server/src server/src

# Build the project
RUN cd server && gleam build

EXPOSE 8080

CMD ["gleam", "run", "--", "server"]
```

## Dependencies

### Server
- `gleam_stdlib` - Standard library
- `gleam_http` - HTTP types and utilities
- `mist` - HTTP server framework
- `gleam_json` - JSON encoding/decoding
- `gleam_erlang` - Erlang interop
- `gleam_otp` - OTP patterns (actors, supervisors)

### Client
- `gleam_stdlib` - Standard library
- `gleam_http` - HTTP types
- `gleam_httpc` - HTTP client
- `gleam_json` - JSON handling

## Advanced Features

### Custom Operators

```gleam
// Define custom operators for domain logic
pub fn task_is_urgent(task: Task) -> Bool {
  task.priority == Urgent && task.status == Pending
}

pub fn task_is_overdue(task: Task, now: Float) -> Bool {
  task.created_at + 86400.0 < now && task.status != Completed
}

// Use in filters
let urgent_tasks = list.filter(tasks, task_is_urgent)
let overdue_tasks = list.filter(tasks, fn(t) { task_is_overdue(t, now) })
```

### Type-Safe Builders

```gleam
// Builder pattern with phantom types
pub opaque type TaskBuilder(required, optional) {
  TaskBuilder(
    title: Option(String),
    description: Option(String),
    priority: TaskPriority,
    tags: List(String),
    assigned_to: Option(String)
  )
}

pub fn new_task() -> TaskBuilder(TitleRequired, DescOptional) {
  TaskBuilder(None, None, Medium, [], None)
}

pub fn with_title(
  builder: TaskBuilder(TitleRequired, a),
  title: String
) -> TaskBuilder(TitleSet, a) {
  TaskBuilder(..builder, title: Some(title))
}

pub fn build(builder: TaskBuilder(TitleSet, a)) -> Task {
  // Can only build when title is set (enforced by types)
  let assert Some(title) = builder.title
  Task(
    id: generate_id(),
    title: title,
    description: builder.description,
    // ...
  )
}
```