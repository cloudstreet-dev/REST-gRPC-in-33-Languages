# V REST API Implementation

This directory contains a REST API implementation in V, demonstrating simple, fast, and safe web development.

## Features

- **vweb framework** for REST APIs
- **Zero dependencies** - everything built-in
- **Memory safety** without garbage collection
- **C-like performance** with Go-like syntax
- **Compile-time code generation** for JSON
- **Built-in ORM** (not used in this example for simplicity)

## Prerequisites

- V compiler (latest version)

Install V:
```bash
# Build from source
git clone https://github.com/vlang/v
cd v
make

# Add to PATH
sudo ./v symlink

# Verify installation
v version
```

## Server

The server implements a complete REST API using vweb.

### Running the Server

```bash
cd server
v run src/main.v
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
v run src/main.v
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```v
import net.http
import json

// Create client
client := TaskClient{
    base_url: 'http://localhost:8080'
}

// List all tasks
resp := client.list_tasks('', '') or {
    println('Error: $err')
    return
}
println('Found ${resp.count} tasks')

// Create a task
task := client.create_task(CreateTaskRequest{
    title: 'Learn V'
    description: 'Master the V programming language'
    priority: 'high'
    tags: ['v', 'learning']
    assigned_to: 'developer'
}) or {
    println('Error: $err')
    return
}

// Update task status
updated := client.update_task_status(task.id, 'in-progress') or {
    println('Error: $err')
    return
}

// Delete task
client.delete_task(task.id) or {
    println('Error: $err')
    return
}
```

## Architecture

### Type System

V's type system provides safety without overhead:

```v
// Enums for type safety
enum TaskStatus {
    pending
    in_progress
    completed
    cancelled
}

enum TaskPriority {
    low = 1
    medium = 2
    high = 3
    urgent = 4
}

// Structs with mutable fields
struct Task {
mut:
    id          string
    title       string
    description string
    status      TaskStatus
    priority    TaskPriority
    tags        []string
    assigned_to string
    created_at  i64
    updated_at  i64
}

// JSON attributes for serialization
struct TaskResponse {
    id          string   `json:"id"`
    title       string   `json:"title"`
    description string   `json:"description"`
    status      string   `json:"status"`
    priority    string   `json:"priority"`
    tags        []string `json:"tags"`
    assigned_to string   `json:"assigned_to"`
    created_at  f64      `json:"created_at"`
    updated_at  f64      `json:"updated_at"`
}
```

### vweb Framework

Route handlers with attributes:

```v
// Simple route
pub fn (mut app App) health() vweb.Result {
    return app.json({
        'status': 'healthy'
    })
}

// Route with HTTP method and path
['/api/tasks'; get]
pub fn (mut app App) list_tasks() vweb.Result {
    // Implementation
}

// Route with parameter
['/api/tasks/:id'; get]
pub fn (mut app App) get_task(id string) vweb.Result {
    // Implementation
}

// POST route
['/api/tasks'; post]
pub fn (mut app App) create_task() vweb.Result {
    body := app.req.data
    request := json.decode(CreateTaskRequest, body) or {
        // Error handling
    }
    // Implementation
}
```

### Error Handling

V uses result types and error propagation:

```v
// Function that can fail
fn get_task(id string) !Task {
    if task := tasks[id] {
        return task
    }
    return error('Task not found')
}

// Error propagation with !
fn process_task(id string) ! {
    task := get_task(id)!  // Propagates error
    validate_task(task)!
    save_task(task)!
}

// Error handling with or blocks
task := get_task(id) or {
    println('Error: $err')
    return
}

// Alternative error handling
task := get_task(id) or { 
    Task{} // Return default value
}
```

## V Features Demonstrated

### Option Types

```v
// Option type for nullable values
fn find_task(id string) ?Task {
    if task := tasks[id] {
        return task
    }
    return none
}

// Using option types
if task := find_task('123') {
    println('Found: ${task.title}')
} else {
    println('Not found')
}
```

### Match Expressions

```v
// Type-safe pattern matching
fn status_to_string(status TaskStatus) string {
    return match status {
        .pending { 'pending' }
        .in_progress { 'in-progress' }
        .completed { 'completed' }
        .cancelled { 'cancelled' }
    }
}

// Match with multiple patterns
fn categorize_priority(p int) string {
    return match p {
        1 { 'low' }
        2 { 'medium' }
        3 { 'high' }
        4, 5 { 'urgent' }
        else { 'unknown' }
    }
}
```

### Compile-Time Code Generation

```v
// JSON encoding/decoding is generated at compile time
struct User {
    name string
    age  int
}

user := User{name: 'Alice', age: 30}
json_str := json.encode(user)  // Generated at compile time

decoded := json.decode(User, json_str) or {
    panic(err)
}
```

### Memory Management

```v
// Automatic memory management without GC
fn process_large_data() {
    mut data := []int{len: 1000000}
    // Memory is automatically freed when data goes out of scope
    // No garbage collector needed
}

// Manual memory management when needed
unsafe {
    ptr := malloc(1024)
    // Use pointer
    free(ptr)
}
```

## Testing

```v
// test files end with _test.v
module main

fn test_create_task() {
    task := create_task('Test', 'Description')
    assert task.title == 'Test'
    assert task.status == .pending
}

fn test_update_status() {
    mut task := create_task('Test', 'Description')
    task.status = .completed
    assert task.status == .completed
}

// Run tests
// v test .
```

## Performance

V compiles to native code with no runtime overhead:

```v
// Benchmarking
import benchmark

mut b := benchmark.start()
// Code to benchmark
for i in 0 .. 1000000 {
    process_task(i)
}
b.measure('Process 1M tasks')
```

## Deployment

### Compile to native binary

```bash
cd server
v -prod src/main.v -o task-server
./task-server
```

### Cross-compilation

```bash
# Compile for Linux from macOS
v -os linux -prod src/main.v -o task-server-linux

# Compile for Windows
v -os windows -prod src/main.v -o task-server.exe
```

### Docker

```dockerfile
FROM thevlang/vlang:alpine

WORKDIR /app

COPY . .

RUN v -prod server/src/main.v -o task-server

EXPOSE 8080

CMD ["./task-server"]
```

## Best Practices

1. **Use result types** for error handling
2. **Leverage compile-time features** for performance
3. **Keep functions small and focused**
4. **Use immutability by default** (fields without `mut`)
5. **Prefer match over if-else chains**
6. **Use option types instead of null**
7. **Let V manage memory** unless you need manual control

## Dependencies

V's standard library includes everything needed:
- `vweb` - Web framework
- `json` - JSON encoding/decoding
- `net.http` - HTTP client
- `time` - Time handling

No external dependencies required!