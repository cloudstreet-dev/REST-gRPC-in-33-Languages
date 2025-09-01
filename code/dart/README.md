# Dart Implementation

This directory contains both REST and gRPC implementations of the Task Management API in Dart.

## Directory Structure

```
dart/
├── rest/
│   ├── server/      # Shelf-based REST API server
│   └── client/      # HTTP-based REST client
└── grpc/
    ├── server/      # gRPC server implementation
    ├── client/      # gRPC client implementation
    └── build.sh     # Proto compilation script
```

## Prerequisites

- Dart SDK 3.0 or higher
- Protocol Buffers compiler (protoc) for gRPC code generation

## REST API

### Server Setup

```bash
cd rest/server
dart pub get
dart run bin/server.dart
```

The REST API server will run on `http://localhost:8080`

### REST Client Usage

```bash
cd rest/client
dart pub get
dart run bin/client.dart
```

### REST API Endpoints

- `GET /health` - Health check
- `GET /api/v1/tasks` - List all tasks
- `GET /api/v1/tasks/:id` - Get a specific task
- `POST /api/v1/tasks` - Create a new task
- `PUT /api/v1/tasks/:id` - Update a task
- `PATCH /api/v1/tasks/:id/status` - Update task status
- `DELETE /api/v1/tasks/:id` - Delete a task

## gRPC API

### Generate Proto Files

```bash
cd grpc
./build.sh
```

### Server Setup

```bash
cd grpc/server
dart pub get
dart run bin/server.dart
```

The gRPC server will run on `localhost:50051`

### gRPC Client Usage

```bash
cd grpc/client
dart pub get
dart run bin/client.dart
```

### gRPC Service Methods

- `ListTasks` - Server streaming method to list tasks
- `GetTask` - Unary call to get a single task
- `CreateTask` - Unary call to create a task
- `UpdateTask` - Unary call to update a task
- `DeleteTask` - Unary call to delete a task
- `WatchTasks` - Bidirectional streaming for real-time updates

## Testing

### Test Both APIs

```bash
# Terminal 1 - Start REST server
cd rest/server && dart run bin/server.dart

# Terminal 2 - Start gRPC server
cd grpc/server && dart run bin/server.dart

# Terminal 3 - Run tests
cd rest/client && dart run bin/client.dart
cd grpc/client && dart run bin/client.dart
```

## Features Demonstrated

### REST Implementation
- Shelf framework for HTTP server
- Middleware pipeline (CORS, error handling, logging)
- Request validation
- JSON serialization
- Async/await patterns
- Type-safe models

### gRPC Implementation
- Protocol Buffers integration
- Server streaming (ListTasks)
- Unary calls (Get, Create, Update, Delete)
- Bidirectional streaming (WatchTasks)
- Error handling with gRPC status codes
- Real-time event notifications

## Flutter Integration

Both REST and gRPC clients can be easily integrated into Flutter applications:

```dart
// In your Flutter app's pubspec.yaml
dependencies:
  task_api_rest_client:
    path: ../rest/client
  # OR
  task_api_grpc_client:
    path: ../grpc/client
```

## Performance Considerations

The gRPC implementation typically shows:
- 25-35% lower latency for simple operations
- 40-50% smaller payload sizes
- Better performance for streaming operations
- Efficient binary serialization with Protocol Buffers

## Common Issues

### Proto Generation Issues
If proto generation fails, ensure protoc is installed:
```bash
# macOS
brew install protobuf

# Linux
apt-get install -y protobuf-compiler

# Install Dart plugin
dart pub global activate protoc_plugin
```

### Port Already in Use
```bash
# Find process using port 8080 or 50051
lsof -i :8080
lsof -i :50051

# Kill the process
kill -9 <PID>
```

## Next Steps

- Add database persistence (PostgreSQL, MongoDB)
- Implement authentication (JWT, OAuth2)
- Add comprehensive test suite
- Create Flutter mobile app
- Add WebSocket support for REST real-time updates
- Implement service discovery