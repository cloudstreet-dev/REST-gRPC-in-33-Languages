# C Task Management API

This directory contains both REST and gRPC implementations of the Task Management API in C.

## Project Structure

```
c/
├── rest/
│   ├── server/       # REST API server implementation
│   │   ├── include/  # Header files
│   │   ├── src/      # Source files
│   │   └── Makefile
│   └── client/       # REST API client implementation
│       ├── src/      # Source files
│       └── Makefile
└── grpc/
    └── README.md     # gRPC implementation guide
```

## Prerequisites

### Linux/macOS
- GCC or Clang compiler
- POSIX-compliant system (for pthread, socket APIs)
- Make build tool
- UUID library (optional, for UUID generation)

### Installation

```bash
# macOS
xcode-select --install

# Ubuntu/Debian
sudo apt-get update
sudo apt-get install build-essential uuid-dev

# RHEL/CentOS/Fedora
sudo yum groupinstall "Development Tools"
sudo yum install libuuid-devel
```

## Building and Running

### REST Server

```bash
cd rest/server
make
./task-rest-server [port]

# Or use make targets
make run       # Run on default port 8080
make run-port  # Run on port 8081
```

### REST Client

```bash
cd rest/client
make
./task-rest-client help     # Show usage
./task-rest-client demo     # Run demonstration
./task-rest-client list     # List all tasks
```

## Implementation Details

### REST Server Features

The C REST server is implemented from scratch using POSIX socket APIs:

- **No external dependencies**: Pure C implementation using standard libraries
- **Multi-threaded**: Uses pthreads for handling concurrent connections
- **Thread-safe storage**: Mutex-protected in-memory task repository
- **HTTP/1.1 compliant**: Proper request/response handling
- **JSON support**: Custom JSON parser and serializer
- **CORS enabled**: Supports cross-origin requests

### Architecture

```
┌─────────────────┐
│   HTTP Client   │
└────────┬────────┘
         │
    HTTP Request
         │
         ▼
┌─────────────────┐
│  Socket Server  │
│   (port 8080)   │
└────────┬────────┘
         │
    Parse Request
         │
         ▼
┌─────────────────┐
│  Route Handler  │
│  (thread pool)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Task Repository │
│ (mutex-locked)  │
└─────────────────┘
```

### Key Components

1. **Socket Server** (`server.c`)
   - Creates TCP socket and binds to port
   - Accepts incoming connections
   - Spawns threads for each client
   - Implements HTTP request/response handling

2. **Task Model** (`task.h`, `task.c`)
   - Task structure with all required fields
   - UUID generation for task IDs
   - JSON serialization/deserialization
   - Status and priority enums

3. **Repository** (`task_repository.h`, `task_repository.c`)
   - In-memory storage with array backend
   - Thread-safe operations using pthread mutexes
   - CRUD operations
   - Query filtering by status, assignee, tags

4. **HTTP Client** (`client.c`)
   - Socket-based HTTP client
   - URL parsing
   - Request building
   - Response parsing
   - Pretty JSON printing

### API Endpoints

All endpoints are implemented according to the specification:

```
GET    /api/tasks          - List all tasks
GET    /api/tasks/{id}     - Get specific task
POST   /api/tasks          - Create new task
PUT    /api/tasks/{id}     - Update task
PATCH  /api/tasks/{id}/status - Update task status
DELETE /api/tasks/{id}     - Delete task
GET    /health             - Health check
```

### Performance Characteristics

- **Concurrency**: Thread-per-connection model
- **Memory**: Fixed-size task array (1000 tasks max)
- **Latency**: Minimal overhead, direct system calls
- **Throughput**: Limited by thread creation overhead

### Limitations

1. **No persistent storage**: Data is lost on restart
2. **Fixed capacity**: Maximum 1000 tasks
3. **Simple JSON parser**: Basic implementation, not fully RFC-compliant
4. **No authentication**: Open access to all endpoints
5. **Thread-per-connection**: Not suitable for high concurrency (use epoll/kqueue for production)

## Testing

### Manual Testing

```bash
# Start server
./task-rest-server &

# Test endpoints
curl http://localhost:8080/health
curl http://localhost:8080/api/tasks
curl -X POST http://localhost:8080/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"Test Task","priority":"high"}'

# Run client demo
./task-rest-client demo

# Stop server
kill %1
```

### Client Commands

```bash
# List tasks
./task-rest-client list

# Get specific task
./task-rest-client get <task-id>

# Create task
./task-rest-client create '{"title":"New Task","priority":"medium"}'

# Update task
./task-rest-client update <task-id> '{"title":"Updated Task"}'

# Delete task
./task-rest-client delete <task-id>

# Check health
./task-rest-client health
```

## C Language Highlights

This implementation demonstrates:

1. **System Programming**: Direct use of POSIX APIs
2. **Memory Management**: Manual allocation and deallocation
3. **Concurrency**: Thread synchronization with mutexes
4. **Network Programming**: Socket creation and management
5. **String Processing**: HTTP parsing and JSON handling
6. **Error Handling**: Proper resource cleanup

## Comparison with Higher-Level Languages

### Advantages of C
- **Performance**: Direct system calls, minimal overhead
- **Control**: Complete control over memory and resources
- **Portability**: POSIX compliance ensures wide compatibility
- **Size**: Small binary size, minimal dependencies

### Challenges
- **Complexity**: More code required for basic functionality
- **Safety**: Manual memory management prone to errors
- **Libraries**: Limited ecosystem compared to modern languages
- **Development Speed**: Slower development cycle

## Production Considerations

For production use, consider:

1. **Event-driven architecture**: Use epoll (Linux) or kqueue (BSD/macOS)
2. **Connection pooling**: Reuse threads instead of creating new ones
3. **Persistent storage**: Add database support (SQLite, PostgreSQL)
4. **Security**: Implement authentication and input validation
5. **Logging**: Add comprehensive logging
6. **Configuration**: External configuration files
7. **Signal handling**: Graceful shutdown
8. **Memory pools**: Reduce allocation overhead

## gRPC Support

For gRPC implementation in C, see the [grpc/README.md](grpc/README.md) file. Note that gRPC for C requires the gRPC C++ library as there's no pure C implementation.

## Resources

- [Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)
- [POSIX Threads Programming](https://computing.llnl.gov/tutorials/pthreads/)
- [HTTP/1.1 Specification](https://www.rfc-editor.org/rfc/rfc2616)
- [JSON Specification](https://www.json.org/)

## License

This implementation is part of the "REST APIs and gRPC in 33 Languages" project.