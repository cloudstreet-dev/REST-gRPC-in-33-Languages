# Nim REST API Implementation

This directory contains a REST API implementation in Nim, demonstrating efficient systems programming with Python-like syntax and compile-time metaprogramming.

## Features

- **Jester web framework** for HTTP server
- **Compile-time execution** with macros and templates
- **Python-like syntax** with C-level performance
- **Option types** for null safety
- **Thread safety** with locks
- **Zero-overhead abstractions**

## Prerequisites

- Nim 2.0.0 or higher
- Nimble package manager

Install Nim:
```bash
# macOS
brew install nim

# Unix-like systems (choosenim installer)
curl https://nim-lang.org/choosenim/init.sh -sSf | sh

# Windows
# Download from https://nim-lang.org/install_windows.html

# Verify installation
nim --version
nimble --version
```

## Server

The server implements a complete REST API using the Jester web framework.

### Running the Server

```bash
cd server
nimble install -y
nimble run
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
nimble install -y
nimble run
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```nim
import task_client

let client = newTaskClient("http://localhost:8080")

# List all tasks
let tasks = client.listTasks()

# Create a task
let task = client.createTask(
  title = "Learn Nim",
  description = "Master systems programming",
  priority = "high",
  tags = @["nim", "learning"],
  assignedTo = "developer"
)

# Update task status
let updated = client.updateTaskStatus(task.id, "in-progress")

# Delete task
if client.deleteTask(task.id):
  echo "Task deleted"
```

## Architecture

### Type System

```nim
type
  TaskStatus* = enum
    tsPending = "pending"
    tsInProgress = "in-progress"
    tsCompleted = "completed"
    tsCancelled = "cancelled"

  Task* = object
    id*: string
    title*: string
    description*: Option[string]
    status*: TaskStatus
    priority*: TaskPriority
    tags*: seq[string]
    assignedTo*: Option[string]
    createdAt*: float
    updatedAt*: float
```

### Option Types for Null Safety

```nim
import options

type
  User = object
    name: string
    email: Option[string]

proc getEmail(user: User): string =
  if user.email.isSome:
    result = user.email.get()
  else:
    result = "No email"

# Using option utilities
let domain = user.email
  .filter(proc(e: string): bool = '@' in e)
  .map(proc(e: string): string = e.split('@')[1])
```

### Thread Safety

```nim
import locks

var 
  taskStore = initTable[string, Task]()
  taskLock: Lock

initLock(taskLock)

proc createTask(task: Task) =
  withLock taskLock:
    taskStore[task.id] = task

proc getTask(id: string): Option[Task] =
  withLock taskLock:
    if taskStore.hasKey(id):
      result = some(taskStore[id])
    else:
      result = none(Task)
```

## Nim Features Demonstrated

### Compile-Time Execution

```nim
# Compile-time function evaluation
proc fibonacci(n: static int): int =
  when n <= 1: n
  else: fibonacci(n - 1) + fibonacci(n - 2)

const fib10 = fibonacci(10)  # Computed at compile time

# Compile-time code generation
import macros

macro generateGetters(typeName: typed): untyped =
  result = newStmtList()
  for field in typeName.getType[2]:
    let getterName = ident("get" & field.strVal.capitalizeAscii)
    result.add quote do:
      proc `getterName`(self: `typeName`): auto =
        self.`field`
```

### Templates and Macros

```nim
# Template for common patterns
template withResource(resource, body: untyped) =
  let r = acquire(resource)
  try:
    body
  finally:
    release(r)

withResource myFile:
  # Use myFile here
  processFile(myFile)

# Macro for DSL creation
macro html(body: untyped): string =
  # Transform DSL to HTML string
  result = newLit("<html>") & body.toHtmlString & newLit("</html>")

let page = html:
  head:
    title: "My Page"
  body:
    h1: "Welcome"
    p: "Hello, World!"
```

### Method Call Syntax and UFCS

```nim
# Uniform Function Call Syntax (UFCS)
proc double(x: int): int = x * 2
proc add(x, y: int): int = x + y

# These are equivalent:
let result1 = add(double(5), 3)
let result2 = 5.double.add(3)
let result3 = 5.double().add(3)

# Chain operations
let processed = @[1, 2, 3, 4, 5]
  .filter(x => x > 2)
  .map(x => x * 2)
  .foldl(a + b)
```

### Iterators and Lazy Evaluation

```nim
iterator fibonacci(): int {.closure.} =
  var a = 0
  var b = 1
  while true:
    yield a
    swap(a, b)
    b = a + b

# Use only what you need
for i, fib in fibonacci():
  if i >= 10: break
  echo fib

# Custom iterator
iterator everyNth[T](s: seq[T], n: int): T =
  for i in countup(0, s.high, n):
    yield s[i]

for item in @[1, 2, 3, 4, 5, 6].everyNth(2):
  echo item  # Prints: 1, 3, 5
```

### Async/Await

```nim
import asyncdispatch

proc fetchTask(id: string): Future[Task] {.async.} =
  # Simulate async operation
  await sleepAsync(100)
  result = getTask(id).get()

proc processTasks() {.async.} =
  # Concurrent execution
  let futures = @["1", "2", "3"].mapIt(fetchTask(it))
  let tasks = await all(futures)
  
  for task in tasks:
    echo task.title

waitFor processTasks()
```

## Testing

### Unit Tests

```nim
import unittest

suite "Task Tests":
  test "task creation":
    let task = createTask("Test Task")
    check task.title == "Test Task"
    check task.status == tsPending
    check task.id.len > 0
  
  test "task update":
    var task = createTask("Test")
    task.status = tsCompleted
    check task.status == tsCompleted
  
  test "task validation":
    expect ValueError:
      discard createTask("")  # Empty title should fail
```

### Property-Based Testing

```nim
import random, sequtils

proc randomString(length: int): string =
  const chars = "abcdefghijklmnopqrstuvwxyz"
  result = newString(length)
  for i in 0..<length:
    result[i] = chars[rand(chars.high)]

proc testProperty(property: proc(x: string): bool, iterations = 100) =
  for _ in 0..<iterations:
    let input = randomString(rand(1..50))
    assert property(input), "Property failed for: " & input

testProperty(proc(s: string): bool =
  let task = createTask(s)
  task.title == s and task.id != ""
)
```

## Performance Optimization

### Compile-Time Optimizations

```nim
# Use const for compile-time constants
const
  MaxTasks = 10000
  DefaultTimeout = 30

# Static dispatch with generics
proc processItem[T](item: T) =
  when T is Task:
    echo "Processing task: ", item.title
  elif T is string:
    echo "Processing string: ", item
  else:
    echo "Processing: ", $item

# Inline procedures for performance
proc add(a, b: int): int {.inline.} =
  a + b
```

### Memory Management

```nim
# Stack allocation for performance
proc processArray() =
  var data: array[1000, int]  # Stack allocated
  for i in 0..<1000:
    data[i] = i * 2

# Manual memory management when needed
proc createBuffer(size: int): ptr UncheckedArray[byte] =
  cast[ptr UncheckedArray[byte]](alloc(size))

proc freeBuffer(buffer: ptr UncheckedArray[byte]) =
  dealloc(buffer)

# Destructors for RAII
type
  Resource = object
    handle: pointer

proc `=destroy`(r: var Resource) =
  if r.handle != nil:
    freeResource(r.handle)
    r.handle = nil
```

## Best Practices

1. **Use Option types**: Avoid null pointer exceptions
2. **Leverage compile-time execution**: Move computation to compile time
3. **Use iterators for large datasets**: Lazy evaluation saves memory
4. **Prefer value types**: Stack allocation is faster
5. **Use UFCS**: Makes code more readable
6. **Write tests**: Nim's unittest is built-in and easy
7. **Profile before optimizing**: Use nimprof for profiling

## Docker Support

```dockerfile
FROM nimlang/nim:2.0.0-alpine

RUN apk add --no-cache git

WORKDIR /app

# Copy nimble files first for caching
COPY server/*.nimble .
RUN nimble install -y

COPY server/ .
RUN nimble build -d:release

EXPOSE 8080
CMD ["./task_server"]
```

## Dependencies

### Server
- `jester` - Web framework
- `uuid4` - UUID generation

### Client
- `puppy` - HTTP client library

## Advanced Features

### Effect System

```nim
# Track side effects in the type system
proc pureFunction(x: int): int {.noSideEffect.} =
  x * 2

proc withIO(x: int): int {.tags: [IOEffect].} =
  echo "Processing: ", x
  x * 2

# Compile-time verification of effects
proc mustBePure() {.noSideEffect.} =
  discard pureFunction(5)  # OK
  # discard withIO(5)  # Compile error!
```

### Custom Pragmas

```nim
template cached(procDef: untyped): untyped =
  var cache = initTable[tuple[args: typeof(procDef.params)], typeof(procDef.returnType)]()
  
  proc procDef =
    let key = (args: procDef.params)
    if key in cache:
      return cache[key]
    else:
      let result = procDef.body
      cache[key] = result
      return result

proc expensiveComputation(n: int): int {.cached.} =
  # This will be cached automatically
  sleep(1000)
  n * n
```