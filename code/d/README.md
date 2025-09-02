# D Task Management API

This directory contains both REST and gRPC implementations of the Task Management API in D.

## Project Structure

```
d/
├── rest/
│   ├── server/       # REST API server using Vibe.d
│   │   ├── source/   # D source files
│   │   └── dub.json  # DUB package configuration
│   └── client/       # REST API client
│       ├── source/   # D source files
│       └── dub.json  # DUB package configuration
└── grpc/
    └── README.md     # gRPC implementation guide
```

## Prerequisites

### Installing D

#### macOS
```bash
# Using Homebrew
brew install dmd dub

# Or using the official installer
curl -fsS https://dlang.org/install.sh | bash -s dmd
```

#### Ubuntu/Debian
```bash
# Add D APT repository
wget https://netcologne.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
sudo apt-get update
sudo apt-get install dmd-compiler dub
```

#### Windows
Download the installer from [dlang.org](https://dlang.org/download.html)

### Verify Installation
```bash
dmd --version
dub --version
```

## Building and Running

### REST Server

```bash
cd rest/server
dub build
./task-rest-server

# Or run directly
dub run

# Run on different port
PORT=8081 dub run
```

### REST Client

```bash
cd rest/client
dub build
./task-rest-client help

# Or run directly
dub run -- help
dub run -- demo
dub run -- list
```

## Implementation Details

### D Language Features Used

1. **Modern Syntax**: Clean, C-like syntax with modern features
2. **Built-in Arrays**: Dynamic arrays with slicing
3. **Ranges and Algorithms**: Functional programming with std.algorithm
4. **Compile-time Function Execution (CTFE)**: Template metaprogramming
5. **Built-in Unit Testing**: unittest blocks
6. **Garbage Collection**: Automatic memory management
7. **Interfaces and Classes**: OOP support
8. **Synchronized Blocks**: Thread safety
9. **Properties**: Getter/setter syntax
10. **Attributes**: @safe, @nogc, @property

### Vibe.d Framework

Vibe.d is D's premier web framework, offering:

- **Fiber-based Async I/O**: High performance without callback hell
- **REST Interface Generation**: Automatic routing from interfaces
- **Built-in JSON Support**: Seamless JSON serialization
- **HTTP Client/Server**: Full-featured HTTP implementation
- **WebSocket Support**: Real-time communication
- **Database Drivers**: MongoDB, Redis, PostgreSQL

### Architecture

```
┌─────────────────┐
│   HTTP Client   │
└────────┬────────┘
         │
    REST Request
         │
         ▼
┌─────────────────┐
│    Vibe.d       │
│  HTTP Server    │
└────────┬────────┘
         │
    Route Handler
         │
         ▼
┌─────────────────┐
│  REST Interface │
│   (ITaskAPI)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│    TaskAPI      │
│ Implementation  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Task Repository │
│  (Thread-safe)  │
└─────────────────┘
```

### Key Components

1. **Task Model** (`Task` struct)
   - Immutable by default
   - JSON serialization methods
   - Enum types for status and priority

2. **Repository** (`TaskRepository` class)
   - Thread-safe with Mutex
   - In-memory storage
   - LINQ-style queries with ranges

3. **REST Interface** (`ITaskAPI`)
   - Declarative routing with attributes
   - Automatic parameter binding
   - Type-safe API definition

4. **REST Implementation** (`TaskAPI`)
   - Implements the interface
   - Error handling with exceptions
   - JSON response generation

## API Endpoints

All standard endpoints are implemented:

```
GET    /api/tasks          - List all tasks
GET    /api/tasks/{id}     - Get specific task
POST   /api/tasks          - Create new task
PUT    /api/tasks/{id}     - Update task
PATCH  /api/tasks/{id}/status - Update task status
DELETE /api/tasks/{id}     - Delete task
GET    /health             - Health check
```

### Query Parameters

- `status`: Filter by task status
- `assigned_to`: Filter by assignee
- `tags`: Comma-separated list of tags
- `page_size`: Number of results (default: 20)
- `page_token`: Pagination token
- `sort_by`: Sort field (created_at, updated_at, title)
- `sort_order`: asc or desc

## Testing

### Unit Tests

D has built-in unit testing support:

```d
unittest {
    auto task = Task("Test Task");
    assert(task.title == "Test Task");
    assert(task.status == TaskStatus.pending);
}
```

Run tests with:
```bash
dub test
```

### Integration Testing

```bash
# Start server
cd rest/server && dub run &

# Run client tests
cd rest/client
dub run -- demo
dub run -- health

# Manual testing
curl http://localhost:8080/api/tasks
curl -X POST http://localhost:8080/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"Test Task","priority":"high"}'
```

## D Language Highlights

### Advantages

1. **Performance**: Compiled to native code, comparable to C++
2. **Productivity**: High-level features with low-level control
3. **Safety**: Memory safety features (@safe, bounds checking)
4. **Metaprogramming**: Powerful compile-time features
5. **Standard Library**: Rich std library with algorithms and ranges
6. **Interoperability**: Easy C/C++ integration

### Unique Features

```d
// Uniform Function Call Syntax (UFCS)
auto result = array.filter!(x => x > 0).map!(x => x * 2).array;

// Template constraints
T max(T)(T a, T b) if (is(T : real)) {
    return a > b ? a : b;
}

// Compile-time evaluation
enum factorial(int n) = n <= 1 ? 1 : n * factorial!(n - 1);
enum fact5 = factorial!5; // Computed at compile time

// Scope guards
scope(exit) cleanup();
scope(success) commit();
scope(failure) rollback();
```

## Performance Characteristics

- **Concurrency**: Fiber-based async I/O (similar to Go's goroutines)
- **Memory**: GC with optional manual management
- **Compilation**: Fast compilation with DMD
- **Runtime**: Minimal runtime overhead
- **Optimization**: LDC and GDC for production builds

## Production Considerations

1. **Compiler Choice**:
   - DMD: Fast compilation, good for development
   - LDC: LLVM-based, best performance
   - GDC: GCC-based, good compatibility

2. **Memory Management**:
   - Use @nogc for real-time sections
   - Profile GC pauses with --DRT-gcopt=profile:1

3. **Deployment**:
   ```bash
   # Build optimized binary
   dub build --compiler=ldc2 --build=release
   ```

4. **Monitoring**: Integrate with Prometheus/Grafana

## Comparison with Other Languages

### D vs C++
- **Pros**: Cleaner syntax, faster compilation, built-in unit tests
- **Cons**: Smaller ecosystem, less mature tooling

### D vs Go
- **Pros**: More powerful type system, templates, UFCS
- **Cons**: Smaller community, fewer libraries

### D vs Rust
- **Pros**: Easier learning curve, optional GC, faster compilation
- **Cons**: Less memory safety guarantees, smaller ecosystem

## gRPC Support

For gRPC implementation details, see [grpc/README.md](grpc/README.md). D has limited gRPC support through:
- [grpc-d](https://github.com/huntlabs/grpc-d) - Experimental D bindings
- Protocol Buffers support via [protobuf-d](https://github.com/dcarp/protobuf-d)

## Resources

- [D Programming Language](https://dlang.org/)
- [Vibe.d Documentation](https://vibed.org/)
- [D Package Repository (DUB)](https://code.dlang.org/)
- [Programming in D (Book)](https://ddili.org/ders/d.en/)
- [D Language Tour](https://tour.dlang.org/)

## Troubleshooting

### Common Issues

1. **DUB dependency issues**:
   ```bash
   dub clean
   dub upgrade
   dub build --force
   ```

2. **Port already in use**:
   ```bash
   PORT=8081 dub run
   ```

3. **Memory leaks**:
   ```bash
   dub run -- --DRT-gcopt=profile:1
   ```

## License

This implementation is part of the "REST APIs and gRPC in 33 Languages" project.