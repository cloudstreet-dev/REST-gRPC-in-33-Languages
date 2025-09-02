# Zig REST API Implementation

This directory contains a REST API implementation in Zig, demonstrating modern systems programming with compile-time safety and performance.

## Features

- **Zero-cost abstractions** with compile-time optimization
- **Manual memory management** with explicit allocators
- **Thread-safe operations** using mutexes
- **No hidden control flow** - explicit error handling
- **Comptime code generation** capabilities
- **C ABI compatibility** for seamless integration

## Prerequisites

- Zig 0.11.0 or higher

Install Zig:
```bash
# macOS
brew install zig

# Linux
wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz
tar xf zig-linux-x86_64-0.11.0.tar.xz
export PATH=$PATH:$(pwd)/zig-linux-x86_64-0.11.0

# Windows
# Download from https://ziglang.org/download/
```

## Server

The server implements a complete REST API using Zig's built-in HTTP server capabilities.

### Running the Server

```bash
cd server
zig build run
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

The client provides a comprehensive SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
zig build run
```

### Using the Client Library

```zig
const std = @import("std");
const TaskClient = @import("task_client").TaskClient;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var client = TaskClient.init(allocator, "http://localhost:8080");
    defer client.deinit();

    // List tasks
    const response = try client.listTasks();
    defer allocator.free(response.tasks);

    // Create a task
    const task = try client.createTask(.{
        .title = "Learn Zig",
        .priority = "high",
        .tags = &[_][]const u8{"zig", "learning"},
    });

    // Update task status
    _ = try client.updateTaskStatus(task.id, "in-progress");

    // Delete task
    try client.deleteTask(task.id);
}
```

## Architecture

### Memory Management

```zig
// Explicit allocator pattern
const TaskStore = struct {
    allocator: std.mem.Allocator,
    tasks: std.StringHashMap(Task),
    
    pub fn init(allocator: std.mem.Allocator) TaskStore {
        return .{
            .allocator = allocator,
            .tasks = std.StringHashMap(Task).init(allocator),
        };
    }
    
    pub fn deinit(self: *TaskStore) void {
        // Explicit cleanup
        var iter = self.tasks.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            // Free all allocated fields...
        }
        self.tasks.deinit();
    }
};
```

### Error Handling

```zig
// Error union return types
fn getTask(id: []const u8) !Task {
    return self.tasks.get(id) orelse error.TaskNotFound;
}

// Explicit error handling
const task = getTask("123") catch |err| {
    switch (err) {
        error.TaskNotFound => {
            // Handle not found
        },
        else => return err,
    }
};
```

### Compile-Time Code

```zig
// Comptime function evaluation
fn typeNameOf(comptime T: type) []const u8 {
    return @typeName(T);
}

// Comptime struct generation
fn CreateApiClient(comptime base_url: []const u8) type {
    return struct {
        pub fn request(path: []const u8) !Response {
            const url = comptime base_url ++ path;
            // ...
        }
    };
}
```

## Zig Features Demonstrated

### Optionals and Error Unions

```zig
// Optional types
const description: ?[]const u8 = null;

// Error unions
fn parseJson(data: []const u8) !Task {
    return json.parse(Task, data) catch error.InvalidJson;
}
```

### Defer and Errdefer

```zig
fn processTask() !void {
    const resource = try allocateResource();
    defer freeResource(resource);  // Always runs
    
    const temp = try createTemp();
    errdefer deleteTemp(temp);  // Only runs on error
    
    try doWork(resource, temp);
}
```

### Comptime Reflection

```zig
fn printStructInfo(comptime T: type) void {
    inline for (@typeInfo(T).Struct.fields) |field| {
        std.debug.print("Field: {s}, Type: {s}\n", .{
            field.name,
            @typeName(field.type),
        });
    }
}
```

### Tagged Unions

```zig
const TaskEvent = union(enum) {
    created: Task,
    updated: struct { old: Task, new: Task },
    deleted: []const u8,
    
    pub fn process(self: TaskEvent) void {
        switch (self) {
            .created => |task| handleCreated(task),
            .updated => |data| handleUpdated(data.old, data.new),
            .deleted => |id| handleDeleted(id),
        }
    }
};
```

## Testing

### Unit Tests

```zig
test "task creation" {
    var store = TaskStore.init(testing.allocator);
    defer store.deinit();
    
    const task = try store.createTask("Test", null, null, null, null);
    try testing.expectEqualStrings("Test", task.title);
    try testing.expect(task.status == .pending);
}

test "task validation" {
    const task = Task{
        .id = "test-1",
        .title = "",
        // ...
    };
    
    try testing.expectError(error.InvalidTitle, validateTask(task));
}
```

Run tests:
```bash
zig build test
```

## Performance Optimizations

### SIMD Operations

```zig
// Vector operations for bulk processing
fn processTasksBatch(tasks: []Task) void {
    const Vector = @Vector(4, i32);
    
    var priorities: Vector = .{ 0, 0, 0, 0 };
    // Process 4 tasks at once...
}
```

### Async/Await (Experimental)

```zig
fn fetchTaskAsync(id: []const u8) !Task {
    const frame = async fetchFromDatabase(id);
    // Do other work...
    return await frame;
}
```

## Cross-Compilation

```bash
# Build for Linux from macOS
zig build -Dtarget=x86_64-linux

# Build for Windows
zig build -Dtarget=x86_64-windows

# Build for ARM64
zig build -Dtarget=aarch64-linux
```

## Best Practices

1. **Explicit allocator usage**: Always pass allocators explicitly
2. **Defer cleanup**: Use defer for resource cleanup
3. **Error handling**: Return error unions, handle all cases
4. **Comptime when possible**: Leverage compile-time evaluation
5. **No hidden allocations**: Be explicit about memory allocation
6. **Test everything**: Zig makes testing easy with built-in test blocks

## Dependencies

Zig has a minimal standard library and encourages self-contained implementations. For this REST API:

- Standard library HTTP server
- Built-in JSON parsing
- No external dependencies required

## Docker Support

```dockerfile
FROM alpine:latest

RUN apk add --no-cache wget xz

# Install Zig
RUN wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz && \
    tar xf zig-linux-x86_64-0.11.0.tar.xz && \
    mv zig-linux-x86_64-0.11.0 /usr/local/zig && \
    rm zig-linux-x86_64-0.11.0.tar.xz

ENV PATH="/usr/local/zig:${PATH}"

WORKDIR /app
COPY . .

RUN cd server && zig build

EXPOSE 8080
CMD ["./server/zig-out/bin/task-server"]
```