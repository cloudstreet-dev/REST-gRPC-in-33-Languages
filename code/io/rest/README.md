# Io REST API Implementation

This directory contains a REST API implementation in Io, demonstrating prototype-based object-oriented programming with message passing.

## Features

- **Prototype-based OOP** without classes
- **Message passing** as the core mechanism
- **Dynamic typing** with late binding
- **Actor-based concurrency** model
- **Minimal syntax** with maximum expressiveness
- **Socket-based HTTP** implementation

## Prerequisites

- Io language interpreter

Install Io:
```bash
# macOS
brew install io

# Build from source
git clone https://github.com/IoLanguage/io.git
cd io
mkdir build && cd build
cmake ..
make
sudo make install

# Verify installation
io --version
```

## Server

The server implements a REST API using raw sockets and Io's object system.

### Running the Server

```bash
cd server
io server.io
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

- `status` - Filter by task status
- `assigned_to` - Filter by assignee

## Client

The client demonstrates HTTP communication using Io's socket library.

### Running the Client Demo

```bash
cd client
io client.io
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```io
// Create client
client := TaskClient clone init("http://localhost:8080")

// List all tasks
tasks := client listTasks(nil, nil)

// Create a task
task := client createTask(
    "Learn Io",
    "Master prototype-based programming",
    "high",
    nil,
    "developer"
)

// Update task status
updated := client updateTaskStatus(task at("id"), "in-progress")

// Delete task
client deleteTask(task at("id"))
```

## Architecture

### Prototype-Based Objects

Io uses prototypes instead of classes:

```io
// Create a prototype
Animal := Object clone do(
    name ::= nil
    sound ::= nil
    
    speak := method(
        writeln(name, " says ", sound)
    )
)

// Create instances by cloning
dog := Animal clone
dog setName("Rex")
dog setSound("woof")
dog speak  // "Rex says woof"

// Extend through cloning
Cat := Animal clone do(
    purr := method(
        writeln(name, " is purring")
    )
)

cat := Cat clone setName("Fluffy") setSound("meow")
cat speak  // "Fluffy says meow"
cat purr   // "Fluffy is purring"
```

### Message Passing

Everything in Io is done through message passing:

```io
// Messages are method calls
object message(args)

// Messages can be intercepted
MyObject := Object clone do(
    forward := method(
        writeln("Unknown message: ", call message name)
        writeln("Arguments: ", call message arguments)
    )
)

obj := MyObject clone
obj unknownMethod(1, 2, 3)  // Caught by forward
```

### Slots and Assignment

```io
// Create slots with ::=
Person := Object clone do(
    name ::= nil     // Creates setName and name methods
    age ::= 0
    
    // Manual slot creation
    nickname := nil  // Just a slot, no setter
    
    // Method slot
    greet := method(
        writeln("Hello, I'm ", name)
    )
)

// Use the generated setters
person := Person clone
person setName("Alice")
person setAge(30)
person greet
```

## Io Features Demonstrated

### Method Chaining

```io
// Methods return self by default, enabling chaining
Task := Object clone do(
    id ::= nil
    title ::= nil
    status ::= "pending"
    
    withTitle := method(t,
        setTitle(t)
        self  // Return self for chaining
    )
    
    withStatus := method(s,
        setStatus(s)
        self
    )
)

task := Task clone \
    withTitle("Learn Io") \
    withStatus("in-progress") \
    setId("task_1")
```

### Lazy Evaluation

```io
// Io evaluates messages lazily
LazyList := Object clone do(
    items ::= nil
    
    init := method(
        setItems(list())
        self
    )
    
    add := method(item,
        items append(block(item))  // Store as block
        self
    )
    
    evaluate := method(
        items map(b, b call)  // Evaluate when needed
    )
)

// Values computed only when evaluate is called
lazy := LazyList clone init
lazy add(expensiveComputation())
lazy add(anotherExpensiveOp())
results := lazy evaluate  // Computed here
```

### Concurrency with Actors

```io
// Actors for concurrent programming
Counter := Object clone do(
    count ::= 0
    
    increment := method(
        setCount(count + 1)
    )
    
    @@increment := method(
        // @@ makes it asynchronous
        increment
    )
)

counter := Counter clone
// Send async messages
counter @@increment
counter @@increment
counter @@increment

// Futures for async results
future := counter @@getCount
result := future // Blocks until ready
```

### DSL Creation

```io
// Io's minimal syntax enables DSLs
HTML := Object clone do(
    forward := method(
        tagName := call message name
        content := call evalArgAt(0)
        "<" .. tagName .. ">" .. content .. "</" .. tagName .. ">"
    )
)

// Use the DSL
page := HTML clone
html := page div(
    page h1("Welcome") .. 
    page p("This is Io")
)
writeln(html)  // <div><h1>Welcome</h1><p>This is Io</p></div>
```

### Introspection

```io
// Io provides powerful introspection
obj := Object clone do(
    name := "test"
    value := 42
    
    describe := method(
        writeln("Slots:")
        self slotNames foreach(slotName,
            writeln("  ", slotName, " = ", self getSlot(slotName))
        )
    )
)

obj describe

// Inspect message structure
message := block(foo bar(1, 2))
writeln("Name: ", message name)
writeln("Arguments: ", message arguments)
```

## HTTP Server Implementation

The server uses raw sockets for HTTP:

```io
// Simple HTTP server
HttpServer := Object clone do(
    port ::= 8080
    
    start := method(
        serverSocket := Socket clone
        serverSocket setHost("127.0.0.1")
        serverSocket setPort(port)
        serverSocket serverOpen
        
        writeln("Server listening on port ", port)
        
        loop(
            clientSocket := serverSocket serverAccept
            if(clientSocket != nil,
                handleClient(clientSocket)
                clientSocket close
            )
        )
    )
    
    handleClient := method(socket,
        request := socket readLine
        // Parse request
        parts := request split(" ")
        method := parts at(0)
        path := parts at(1)
        
        // Read headers
        loop(
            line := socket readLine
            if(line size == 0, break)
        )
        
        // Send response
        response := "HTTP/1.1 200 OK\r\n"
        response = response .. "Content-Type: text/plain\r\n"
        response = response .. "\r\n"
        response = response .. "Hello from Io!"
        
        socket write(response)
    )
)
```

## Testing

```io
// Test framework
TestCase := Object clone do(
    setUp := method()
    tearDown := method()
    
    run := method(
        setUp
        
        self slotNames select(name, name beginsWithSeq("test")) foreach(testName,
            writeln("Running ", testName, "...")
            
            e := try(
                self perform(testName)
                writeln("  ✓ passed")
            )
            
            e catch(Exception,
                writeln("  ✗ failed: ", e error)
            )
        )
        
        tearDown
    )
    
    assert := method(condition, message,
        if(condition not,
            Exception raise(message ifNil("Assertion failed"))
        )
    )
)

// Example test
TaskTest := TestCase clone do(
    testCreateTask := method(
        task := Task clone
        task setTitle("Test")
        assert(task title == "Test", "Title should be Test")
        assert(task status == "pending", "Status should be pending")
    )
    
    testUpdateStatus := method(
        task := Task clone
        task setStatus("completed")
        assert(task status == "completed", "Status should be completed")
    )
)

TaskTest run
```

## Best Practices

1. **Use prototypes effectively** - Clone and extend rather than inherit
2. **Leverage message passing** - Use forward for dynamic behavior
3. **Keep objects focused** - Small prototypes with single responsibilities
4. **Use method chaining** - Return self for fluent interfaces
5. **Embrace late binding** - Dynamic behavior is Io's strength
6. **Use actors for concurrency** - Avoid shared state
7. **Create DSLs** - Io's syntax is perfect for domain languages

## Performance Considerations

### Message Caching

```io
// Io caches message lookups
Object clone do(
    // First call does lookup
    someMethod  // Slow
    
    // Subsequent calls use cache
    someMethod  // Fast
    someMethod  // Fast
)
```

### Lazy Evaluation

```io
// Use blocks for lazy evaluation
expensive := block(performExpensiveOperation())

// Only evaluated when called
if(condition, expensive call)
```

## Deployment

### Standalone Script

```bash
#!/usr/bin/env io
# Make executable: chmod +x script.io
./script.io
```

### Docker

```dockerfile
FROM alpine:latest

RUN apk add --no-cache git cmake make g++ \
    && git clone https://github.com/IoLanguage/io.git \
    && cd io \
    && mkdir build && cd build \
    && cmake .. \
    && make \
    && make install \
    && cd / \
    && rm -rf io

WORKDIR /app
COPY . .

EXPOSE 8080

CMD ["io", "server/server.io"]
```

## Dependencies

Io's standard library includes:
- `Socket` - Network programming
- `Date` - Time handling
- `List` - Collection operations
- `Map` - Key-value storage
- `File` - File I/O

No external dependencies required for basic REST API!