# Objective-C Task Management API

This directory contains both REST and gRPC implementations of the Task Management API in Objective-C.

## Project Structure

```
objective-c/
├── rest/
│   ├── server/       # REST API server implementation
│   └── client/       # REST API client implementation
└── grpc/
    └── README.md     # gRPC implementation guide
```

## Prerequisites

### macOS (Recommended)
- Xcode Command Line Tools: `xcode-select --install`
- Homebrew: `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`

### Dependencies

#### For REST Server (GCDWebServer)
```bash
# Option 1: Using CocoaPods
pod init
echo "pod 'GCDWebServer', '~> 3.5'" >> Podfile
pod install

# Option 2: Using Homebrew (if available)
brew install gcdwebserver

# Option 3: Manual installation
git clone https://github.com/swisspol/GCDWebServer.git
cd GCDWebServer
# Follow build instructions in their README
```

#### For gRPC (Optional)
```bash
# Install gRPC and Protocol Buffers
brew install grpc protobuf

# Or using CocoaPods for iOS/macOS projects
pod 'gRPC-ObjC'
pod 'gRPC-ProtoRPC'
```

## Building and Running

### REST Server

```bash
cd rest/server

# If GCDWebServer is installed system-wide
make

# If using CocoaPods
open TaskServer.xcworkspace
# Build and run in Xcode

# Run the server
./task-rest-server
```

### REST Client

```bash
cd rest/client
make
./bin/client help     # Show usage
./bin/client demo     # Run demo
```

## Alternative: Simple HTTP Server (No Dependencies)

If you prefer not to install GCDWebServer, you can use the Foundation framework's built-in NSURLSession for a simple HTTP server implementation. However, GCDWebServer provides a more robust and feature-complete solution for production use.

## Features Implemented

### REST API
- ✅ GET /api/tasks - List all tasks with filtering
- ✅ GET /api/tasks/{id} - Get task by ID  
- ✅ POST /api/tasks - Create new task
- ✅ PUT /api/tasks/{id} - Update task
- ✅ PATCH /api/tasks/{id}/status - Update status
- ✅ DELETE /api/tasks/{id} - Delete task
- ✅ GET /health - Health check

### Task Model
- UUID-based task IDs
- Title and description
- Status (pending, in_progress, completed, cancelled)
- Priority (low, medium, high, urgent)
- Tags array
- Assignment tracking
- Timestamps (created_at, updated_at)

### Advanced Features
- Thread-safe in-memory storage using GCD
- Pagination with page tokens
- Sorting by multiple fields
- Filtering by status, assignee, and tags
- CORS support
- JSON serialization/deserialization
- Comprehensive error handling

## Architecture Highlights

### Grand Central Dispatch (GCD)
- Concurrent queue with dispatch barriers for thread safety
- Async operations for non-blocking performance
- Signal handling for graceful shutdown

### Memory Management
- Automatic Reference Counting (ARC)
- Proper use of strong/weak references
- Copy semantics for immutable data

### Foundation Framework
- NSURLSession for HTTP client
- NSJSONSerialization for JSON handling
- NSDateFormatter for ISO 8601 dates
- NSPredicate for filtering (optional)

## Testing

```bash
# Start the server
cd rest/server && ./task-rest-server &

# Run client tests
cd rest/client
./bin/client list
./bin/client create --title "Test Task" --priority high
./bin/client demo

# Stop the server
kill %1
```

## Performance Considerations

- GCD provides excellent concurrency without explicit thread management
- In-memory storage is fast but not persistent
- Consider Core Data or SQLite for production persistence
- Use NSCache for caching frequently accessed data

## Modern Objective-C Best Practices

1. **Nullability Annotations**: Use `_Nullable` and `_Nonnull`
2. **Lightweight Generics**: `NSArray<Task *> *tasks`
3. **NS_ENUM**: Type-safe enumerations
4. **Blocks**: Modern callback patterns
5. **Properties**: Automatic getter/setter synthesis
6. **ARC**: No manual memory management needed

## Troubleshooting

### GCDWebServer Not Found
If you get "GCDWebServer.h not found":
1. Ensure GCDWebServer is installed (see Prerequisites)
2. Update include paths in Makefile if needed
3. Consider using CocoaPods for easier dependency management

### Compilation Warnings
The code uses modern Objective-C but may show nullability warnings. These can be addressed by adding appropriate `_Nullable` or `_Nonnull` annotations.

### Port Already in Use
If port 8080 is already in use:
```bash
# Find process using port 8080
lsof -i :8080
# Change port in main.m or pass as argument
./task-rest-server 8081
```

## Next Steps

- Add Core Data for persistence
- Implement authentication with NSURLCredential
- Add WebSocket support for real-time updates
- Create iOS/macOS GUI clients
- Implement full gRPC support with protobuf-objc

## Resources

- [GCDWebServer Documentation](https://github.com/swisspol/GCDWebServer)
- [Apple's Concurrency Programming Guide](https://developer.apple.com/library/archive/documentation/General/Conceptual/ConcurrencyProgrammingGuide/)
- [Modern Objective-C](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/)
- [NSURLSession Programming Guide](https://developer.apple.com/documentation/foundation/nsurlsession)