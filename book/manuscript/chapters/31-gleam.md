# Chapter 31: Gleam - Type-Safe BEAM Programming

## Introduction

Gleam represents a fresh approach to BEAM programming, bringing static typing and compile-time guarantees to the Erlang virtual machine ecosystem. Created by Louis Pilfold in 2016, Gleam combines the reliability and scalability of the BEAM with a modern type system inspired by languages like Elm, OCaml, and Rust. The result is a language that catches errors at compile time while leveraging decades of battle-tested runtime infrastructure.

The BEAM (Bogdan/Björn's Erlang Abstract Machine) powers some of the world's most reliable systems, from WhatsApp's messaging infrastructure to Discord's real-time communication platform. Gleam inherits this foundation while addressing common pain points in Erlang and Elixir development: the lack of static typing often leads to runtime errors that could have been caught during compilation. Gleam's type system eliminates entire classes of bugs while maintaining the BEAM's famous fault tolerance and concurrent programming model.

## Language Philosophy

### Type Safety on a Dynamic Platform

Gleam's fundamental innovation lies in bringing static typing to a platform traditionally associated with dynamic languages. While Erlang uses dynamic typing and Elixir adds gradual typing through Dialyzer, Gleam enforces types at compile time, ensuring that type errors never reach production.

The type system draws inspiration from ML-family languages but adapts their concepts to the BEAM's unique requirements:

**Sound Type System**: Unlike TypeScript or Python's type hints, Gleam's type system is sound - if the code compiles, the types are guaranteed to be correct at runtime. There are no escape hatches like `any` types or unchecked casts.

**Type Inference**: Gleam infers types throughout your program, reducing boilerplate while maintaining safety. You write less type annotations than in Java or C#, but get stronger guarantees than in dynamically typed languages.

**No Null References**: Gleam eliminates the billion-dollar mistake of null references through its Option type, making the presence or absence of values explicit in the type system.

### Functional Programming Made Accessible

Gleam embraces functional programming but prioritizes pragmatism and approachability:

```gleam
// Pattern matching feels natural
fn describe_status(task) {
  case task.status {
    Pending -> "Waiting to start"
    InProgress -> "Currently being worked on"
    Completed -> "All done!"
    Cancelled -> "No longer needed"
  }
}

// Pipe operator for readable data transformations
tasks
|> list.filter(is_urgent)
|> list.map(assign_to_team)
|> list.sort_by(fn(t) { t.priority })
```

The language avoids academic jargon and complex abstractions, making functional programming accessible to developers from imperative backgrounds while maintaining mathematical rigor under the hood.

## Type System

Gleam's type system combines simplicity with power, offering algebraic data types, pattern matching, and type inference:

### Custom Types (ADTs)

```gleam
// Sum types (enums) for modeling choices
pub type TaskStatus {
  Pending
  InProgress
  Completed
  Cancelled
}

// Product types (records) for grouping data
pub type Task {
  Task(
    id: String,
    title: String,
    description: Option(String),
    status: TaskStatus,
    priority: Int,
    tags: List(String),
    created_at: DateTime,
  )
}

// Type parameters for generic programming
pub type Result(ok, error) {
  Ok(ok)
  Error(error)
}

// Opaque types hide implementation details
pub opaque type TaskId {
  TaskId(String)
}
```

### Pattern Matching

Pattern matching in Gleam is exhaustive, meaning the compiler ensures you handle all possible cases:

```gleam
fn process_result(result) {
  case result {
    Ok(task) -> {
      io.println("Processing task: " <> task.title)
      handle_task(task)
    }
    Error(NotFound) -> {
      io.println("Task not found")
      create_default_task()
    }
    Error(NetworkError(reason)) -> {
      io.println("Network error: " <> reason)
      retry_later()
    }
    // Compiler error if any case is missing!
  }
}

// Guards add extra conditions
fn categorize_task(task) {
  case task {
    Task(priority: p, ..) if p > 3 -> UrgentTask(task)
    Task(status: Completed, ..) -> CompletedTask(task)
    Task(..) -> RegularTask(task)
  }
}
```

### Type Inference

Gleam's bidirectional type inference reduces annotation burden:

```gleam
// Types are inferred from usage
fn double(x) {
  x * 2  // Inferred as Int -> Int
}

// Generic types are inferred
fn first(items) {
  case items {
    [head, ..] -> Some(head)  // Return type: Option(a)
    [] -> None
  }
}

// Explicit annotations when helpful
fn parse_priority(text: String) -> Result(Int, String) {
  case int.parse(text) {
    Ok(n) if n >= 1 && n <= 5 -> Ok(n)
    Ok(_) -> Error("Priority must be between 1 and 5")
    Error(_) -> Error("Invalid number format")
  }
}
```

## REST API Implementation

Our REST API showcases Gleam's strengths in building concurrent, type-safe web services:

### Server Architecture

The server uses Mist for HTTP handling and OTP actors for state management:

```gleam
import mist
import gleam/otp/actor
import gleam/http/request
import gleam/http/response

pub fn main() {
  // Start the task store actor
  let assert Ok(store) = task_store.start()
  
  // Define the HTTP handler
  let handler = fn(req) {
    handle_request(req, store)
  }
  
  // Start the web server
  let assert Ok(_) = 
    mist.new(handler)
    |> mist.port(8080)
    |> mist.start_http()
  
  process.sleep_forever()
}

fn handle_request(req, store) {
  let method = req.method
  let path = request.path_segments(req)
  
  case method, path {
    http.Get, ["api", "tasks"] -> 
      handle_list_tasks(store, request.get_query(req))
    
    http.Get, ["api", "tasks", id] -> 
      handle_get_task(store, id)
    
    http.Post, ["api", "tasks"] -> 
      handle_create_task(store, req.body)
    
    http.Put, ["api", "tasks", id] -> 
      handle_update_task(store, id, req.body)
    
    http.Delete, ["api", "tasks", id] -> 
      handle_delete_task(store, id)
    
    _, _ -> 
      response.new(404)
      |> response.set_body(text("Not found"))
  }
}
```

### Actor-Based State Management

Gleam leverages OTP's actor model for concurrent state management:

```gleam
import gleam/otp/actor
import gleam/erlang/process

// Message protocol for the actor
pub type Message {
  ListTasks(
    reply_with: process.Subject(List(Task)),
    filter: Option(Filter)
  )
  GetTask(
    reply_with: process.Subject(Option(Task)),
    id: String
  )
  CreateTask(
    reply_with: process.Subject(Task),
    request: CreateTaskRequest
  )
  UpdateTask(
    reply_with: process.Subject(Option(Task)),
    id: String,
    updates: UpdateTaskRequest
  )
  DeleteTask(
    reply_with: process.Subject(Bool),
    id: String
  )
}

// Actor state
type State {
  State(
    tasks: Dict(String, Task),
    counter: Int
  )
}

// Start the actor
pub fn start() {
  actor.start(
    State(dict.new(), 0),
    handle_message
  )
}

// Message handler
fn handle_message(message, state) {
  case message {
    ListTasks(reply_with, filter) -> {
      let tasks = state.tasks
        |> dict.values()
        |> apply_filter(filter)
      
      process.send(reply_with, tasks)
      actor.continue(state)
    }
    
    CreateTask(reply_with, request) -> {
      let id = generate_id(state.counter)
      let task = create_task_from_request(id, request)
      
      let new_state = State(
        tasks: dict.insert(state.tasks, id, task),
        counter: state.counter + 1
      )
      
      process.send(reply_with, task)
      actor.continue(new_state)
    }
    
    UpdateTask(reply_with, id, updates) -> {
      case dict.get(state.tasks, id) {
        Ok(task) -> {
          let updated = apply_updates(task, updates)
          let new_tasks = dict.insert(state.tasks, id, updated)
          
          process.send(reply_with, Some(updated))
          actor.continue(State(..state, tasks: new_tasks))
        }
        Error(_) -> {
          process.send(reply_with, None)
          actor.continue(state)
        }
      }
    }
    
    // ... other message handlers
  }
}

// Type-safe RPC interface
pub fn list_tasks(store, filter) {
  process.call(
    store,
    fn(reply) { ListTasks(reply, filter) },
    timeout: 1000
  )
}

pub fn create_task(store, request) {
  process.call(
    store,
    fn(reply) { CreateTask(reply, request) },
    timeout: 1000
  )
}
```

### JSON Handling

Gleam's type system ensures safe JSON serialization and deserialization:

```gleam
import gleam/json
import gleam/dynamic

// Encoding tasks to JSON
fn task_to_json(task: Task) -> json.Json {
  json.object([
    #("id", json.string(task.id)),
    #("title", json.string(task.title)),
    #("description", case task.description {
      Some(desc) -> json.string(desc)
      None -> json.null()
    }),
    #("status", json.string(status_to_string(task.status))),
    #("priority", json.int(task.priority)),
    #("tags", json.array(task.tags, json.string)),
    #("created_at", json.string(datetime.to_iso8601(task.created_at)))
  ])
}

// Decoding JSON to tasks
fn decode_create_request(json_string: String) -> Result(CreateTaskRequest, String) {
  use decoded <- result.try(
    json.decode(json_string, dynamic.dynamic)
    |> result.map_error(fn(_) { "Invalid JSON" })
  )
  
  use title <- result.try(
    dynamic.field("title", dynamic.string)(decoded)
    |> result.map_error(fn(_) { "Missing title field" })
  )
  
  let description = 
    dynamic.field("description", dynamic.string)(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  let priority = 
    dynamic.field("priority", dynamic.int)(decoded)
    |> result.unwrap(2)  // Default to medium priority
  
  let tags = 
    dynamic.field("tags", dynamic.list(dynamic.string))(decoded)
    |> result.unwrap([])
  
  Ok(CreateTaskRequest(
    title: title,
    description: description,
    priority: priority,
    tags: tags
  ))
}
```

## Client Implementation

The client demonstrates Gleam's HTTP client capabilities and error handling:

```gleam
import gleam/http/request
import gleam/httpc
import gleam/result

pub type Client {
  Client(base_url: String)
}

pub fn new(base_url: String) -> Client {
  Client(base_url)
}

// List tasks with optional filtering
pub fn list_tasks(
  client: Client,
  status: Option(String),
  assigned_to: Option(String)
) -> Result(List(Task), ApiError) {
  let query_params = []
    |> add_param("status", status)
    |> add_param("assigned_to", assigned_to)
  
  let path = build_path("/api/tasks", query_params)
  
  request.new()
  |> request.set_method(http.Get)
  |> request.set_host(client.base_url)
  |> request.set_path(path)
  |> httpc.send()
  |> result.map_error(NetworkError)
  |> result.then(parse_tasks_response)
}

// Create a new task
pub fn create_task(
  client: Client,
  title: String,
  description: Option(String),
  priority: Int,
  tags: List(String)
) -> Result(Task, ApiError) {
  let body = json.object([
    #("title", json.string(title)),
    #("description", option.map(description, json.string) |> option.unwrap(json.null())),
    #("priority", json.int(priority)),
    #("tags", json.array(tags, json.string))
  ])
  
  request.new()
  |> request.set_method(http.Post)
  |> request.set_host(client.base_url)
  |> request.set_path("/api/tasks")
  |> request.set_header("content-type", "application/json")
  |> request.set_body(json.to_string(body))
  |> httpc.send()
  |> result.map_error(NetworkError)
  |> result.then(parse_task_response)
}

// Error handling with custom types
pub type ApiError {
  NetworkError(String)
  ParseError(String)
  NotFound
  ServerError(Int)
}

fn parse_response(response, decoder) {
  case response.status {
    200 | 201 -> 
      json.decode(response.body, decoder)
      |> result.map_error(fn(e) { ParseError(string.inspect(e)) })
    
    404 -> 
      Error(NotFound)
    
    status -> 
      Error(ServerError(status))
  }
}
```

## Concurrency Patterns

Gleam inherits the BEAM's powerful concurrency primitives:

### Supervisors

```gleam
import gleam/otp/supervisor
import gleam/otp/actor

pub fn start_supervisor() {
  supervisor.start(
    supervisor.OneForOne,
    fn(children) {
      children
      |> supervisor.add_worker(task_store.start)
      |> supervisor.add_worker(metrics_collector.start)
      |> supervisor.add_worker(notification_service.start)
    }
  )
}

// Supervised actor with restart strategy
pub fn supervised_actor() {
  supervisor.child_spec(
    id: "task_store",
    start: fn() { task_store.start() },
    restart: supervisor.Permanent,
    shutdown: 5000,
    type: supervisor.Worker
  )
}
```

### Parallel Processing

```gleam
import gleam/list
import gleam/erlang/process
import gleam/otp/task

// Process tasks in parallel
pub fn process_all_tasks(tasks: List(Task)) -> List(Result(Processed, Error)) {
  tasks
  |> list.map(fn(task) {
    task.async(fn() { process_task(task) })
  })
  |> list.map(task.await)
}

// Fan-out/fan-in pattern
pub fn bulk_operation(items: List(Item)) -> Summary {
  let results = 
    items
    |> list.map(fn(item) {
      process.start(fn() {
        expensive_operation(item)
      })
    })
    |> list.map(fn(pid) {
      process.receive(pid, timeout: 5000)
    })
  
  summarize_results(results)
}

// Concurrent map with controlled parallelism
pub fn concurrent_map(items: List(a), fun: fn(a) -> b, max_concurrent: Int) -> List(b) {
  items
  |> list.sized_chunks(max_concurrent)
  |> list.flat_map(fn(chunk) {
    chunk
    |> list.map(fn(item) { task.async(fn() { fun(item) }) })
    |> list.map(task.await)
  })
}
```

## Error Handling

Gleam uses the Result type for explicit, type-safe error handling:

```gleam
// Custom error types
pub type TaskError {
  NotFound(id: String)
  ValidationError(field: String, message: String)
  DatabaseError(reason: String)
  ConcurrencyError
}

// Functions return Result types
pub fn get_task(id: String) -> Result(Task, TaskError) {
  case database.find(id) {
    Ok(task) -> Ok(task)
    Error(NotFound) -> Error(NotFound(id))
    Error(reason) -> Error(DatabaseError(string.inspect(reason)))
  }
}

// Chaining operations with use syntax
pub fn complete_task(id: String) -> Result(Task, TaskError) {
  use task <- result.try(get_task(id))
  use validated <- result.try(validate_completion(task))
  use updated <- result.try(update_status(validated, Completed))
  use notified <- result.map(send_notifications(updated))
  
  Task(..notified, completed_at: Some(datetime.now()))
}

// Pattern matching on errors
fn handle_task_operation(id: String) {
  case complete_task(id) {
    Ok(task) -> 
      io.println("Task completed: " <> task.title)
    
    Error(NotFound(id)) -> 
      io.println("Task " <> id <> " not found")
    
    Error(ValidationError(field, msg)) -> 
      io.println("Validation failed for " <> field <> ": " <> msg)
    
    Error(DatabaseError(reason)) -> {
      log.error("Database error: " <> reason)
      retry_with_backoff()
    }
    
    Error(ConcurrencyError) -> 
      retry_with_random_delay()
  }
}
```

## Testing

Gleam's type system makes testing more reliable and refactoring safer:

```gleam
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Unit tests
pub fn create_task_test() {
  let request = CreateTaskRequest(
    title: "Test Task",
    description: Some("Test Description"),
    priority: 3,
    tags: ["test", "unit"]
  )
  
  let task = create_task_from_request("test-1", request)
  
  task.title
  |> should.equal("Test Task")
  
  task.status
  |> should.equal(Pending)
  
  task.tags
  |> should.contain("test")
}

// Property-based testing
pub fn task_serialization_test() {
  use task <- quick_check.forall(task_generator())
  
  task
  |> task_to_json()
  |> json.to_string()
  |> decode_task()
  |> should.equal(Ok(task))
}

// Integration tests with actors
pub fn actor_concurrent_test() {
  let assert Ok(store) = task_store.start()
  
  // Create tasks concurrently
  let tasks = 
    list.range(1, 100)
    |> list.map(fn(i) {
      process.start(fn() {
        task_store.create_task(
          store,
          CreateTaskRequest(
            title: "Task " <> int.to_string(i),
            description: None,
            priority: i % 5 + 1,
            tags: []
          )
        )
      })
    })
    |> list.map(process.receive)
  
  // Verify all tasks were created
  list.length(tasks)
  |> should.equal(100)
  
  // Verify unique IDs
  tasks
  |> list.map(fn(t) { t.id })
  |> list.unique()
  |> list.length()
  |> should.equal(100)
}

// HTTP endpoint tests
pub fn api_integration_test() {
  let assert Ok(_) = start_test_server()
  
  let client = task_client.new("http://localhost:8080")
  
  // Test task creation
  let assert Ok(task) = task_client.create_task(
    client,
    "Integration Test",
    Some("Testing the API"),
    4,
    ["test", "api"]
  )
  
  task.title
  |> should.equal("Integration Test")
  
  // Test task retrieval
  let assert Ok(retrieved) = task_client.get_task(client, task.id)
  
  retrieved
  |> should.equal(task)
  
  // Test task deletion
  let assert Ok(_) = task_client.delete_task(client, task.id)
  
  task_client.get_task(client, task.id)
  |> should.be_error()
}
```

## Best Practices

### Leverage the Type System

Make illegal states unrepresentable:

```gleam
// Bad: Using strings for status
type Task {
  Task(status: String)  // "pending", "completed", "whoops-typo"
}

// Good: Use custom types
type TaskStatus {
  Pending
  InProgress
  Completed
  Cancelled
}

type Task {
  Task(status: TaskStatus)  // Compiler enforces valid states
}

// Better: Model state transitions
type TodoTask {
  TodoTask(id: String, title: String)
}

type InProgressTask {
  InProgressTask(id: String, title: String, started_at: DateTime)
}

type CompletedTask {
  CompletedTask(id: String, title: String, started_at: DateTime, completed_at: DateTime)
}

type Task {
  Todo(TodoTask)
  InProgress(InProgressTask)
  Completed(CompletedTask)
}
```

### Use Opaque Types

Hide implementation details and enforce invariants:

```gleam
// Public API with opaque type
pub opaque type Email {
  Email(String)
}

// Constructor validates the email
pub fn parse_email(text: String) -> Result(Email, String) {
  case string.contains(text, "@") {
    True -> Ok(Email(string.lowercase(text)))
    False -> Error("Invalid email format")
  }
}

// Accessor functions maintain encapsulation
pub fn email_domain(email: Email) -> String {
  let Email(address) = email
  address
  |> string.split("@")
  |> list.last()
  |> result.unwrap("")
}
```

### Pipe Operator for Clarity

Use the pipe operator to make data transformations readable:

```gleam
// Without pipes (hard to read)
fn process_tasks(tasks) {
  list.sort_by(
    list.map(
      list.filter(tasks, is_active),
      calculate_priority
    ),
    fn(t) { t.priority }
  )
}

// With pipes (clear data flow)
fn process_tasks(tasks) {
  tasks
  |> list.filter(is_active)
  |> list.map(calculate_priority)
  |> list.sort_by(fn(t) { t.priority })
}

// Complex pipelines
fn analyze_tasks(tasks) {
  tasks
  |> list.filter(fn(t) { t.status != Cancelled })
  |> list.group_by(fn(t) { t.assigned_to })
  |> dict.map_values(fn(_, tasks) {
    tasks
    |> list.map(fn(t) { t.priority })
    |> list.fold(0, int.add)
  })
  |> dict.to_list()
  |> list.sort_by(fn(pair) { pair.1 })
  |> list.reverse()
}
```

## Conclusion

Gleam represents a significant evolution in BEAM programming, proving that type safety and developer ergonomics need not be sacrificed for the platform's legendary reliability and concurrency capabilities. By bringing static typing to the BEAM, Gleam eliminates entire categories of runtime errors while preserving the platform's strengths in building distributed, fault-tolerant systems.

The REST API implementation showcases how Gleam's features combine to create robust, maintainable services. The type system catches errors at compile time, pattern matching ensures all cases are handled, actors provide safe concurrent state management, and the pipe operator makes data transformations clear and composable. These features work together to create code that is both safer and more readable than equivalent implementations in dynamically typed languages.

Key takeaways from our Gleam implementation:

1. **Type Safety Without Verbosity**: Gleam's type inference provides safety without the boilerplate of languages like Java, making it accessible to developers from dynamic language backgrounds

2. **Exhaustive Pattern Matching**: The compiler ensures all cases are handled, preventing runtime errors from missed edge cases

3. **Actor Model Integration**: First-class support for OTP patterns makes concurrent programming safe and straightforward

4. **Functional but Practical**: Gleam embraces functional programming concepts while remaining pragmatic and avoiding academic complexity

5. **BEAM Benefits**: Full access to the Erlang ecosystem, including battle-tested libraries and production-grade runtime characteristics

6. **Modern Developer Experience**: Clear error messages, fast compilation, and excellent tooling make development enjoyable

Gleam is particularly well-suited for teams building critical systems that demand both reliability and maintainability. The type system acts as living documentation, making codebases easier to understand and modify. The compiler becomes a partner in development, catching errors early and enabling confident refactoring. Meanwhile, the BEAM runtime ensures that systems remain responsive and resilient under load.

As we implement REST APIs across 33 languages, Gleam stands out for successfully bridging two worlds: the reliability and concurrency of Erlang with the type safety and developer experience of modern statically-typed languages. It demonstrates that we can have both safety and productivity, both reliability and ease of use, making it an excellent choice for building the next generation of distributed systems.