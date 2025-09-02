# Chapter 26: Zig - Modern Systems Programming

## Introduction

Zig represents a new generation of systems programming languages, designed to be a better C while maintaining simplicity and predictability. Created by Andrew Kelley in 2015, Zig offers manual memory management without hidden allocations, compile-time code execution, and cross-compilation as a first-class feature. It's designed for robustness, optimality, and maintainability.

In this chapter, we'll implement our task management REST API in Zig, exploring its unique approach to systems programming that emphasizes explicit control, compile-time safety, and zero-cost abstractions.

## Why Zig?

Zig brings several compelling features to systems programming:

1. **No Hidden Control Flow**: Function calls are visible, no operator overloading or hidden allocations
2. **Compile-Time Code Execution**: Run code at compile time for metaprogramming
3. **First-Class Cross-Compilation**: Target any platform from any platform
4. **Manual Memory Management**: Explicit allocator usage with defer-based cleanup
5. **Error Handling**: Explicit error unions instead of exceptions
6. **C ABI Compatibility**: Seamlessly integrate with C libraries

## Setting Up Zig

Install Zig (supports macOS, Linux, and Windows):

```bash
# macOS with Homebrew
brew install zig

# Linux
wget https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz
tar xf zig-linux-x86_64-0.11.0.tar.xz
export PATH=$PATH:$(pwd)/zig-linux-x86_64-0.11.0

# Verify installation
zig version
```

## Understanding Zig's Philosophy

Before diving into the implementation, let's understand Zig's core principles:

### Explicit Over Implicit

```zig
// Memory allocation is always explicit
const allocator = std.heap.page_allocator;
const memory = try allocator.alloc(u8, 1024);
defer allocator.free(memory);

// No hidden function calls
const result = add(a, b);  // Just a function call, nothing hidden
```

### Compile-Time Code Execution

```zig
// Functions can run at compile time
fn fibonacci(comptime n: u32) u32 {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// This is computed at compile time
const fib_10 = fibonacci(10);  // No runtime cost
```

### Error Handling

```zig
// Error unions make errors explicit
fn divide(a: f64, b: f64) !f64 {
    if (b == 0) return error.DivisionByZero;
    return a / b;
}

// Errors must be handled
const result = divide(10, 2) catch |err| {
    std.debug.print("Error: {}\n", .{err});
    return;
};
```

## Implementing the REST API Server

Let's build our task management server with Zig's HTTP capabilities:

### Task Model

```zig
const Task = struct {
    id: []const u8,
    title: []const u8,
    description: ?[]const u8 = null,
    status: TaskStatus = .pending,
    priority: TaskPriority = .medium,
    tags: [][]const u8 = &.{},
    assigned_to: ?[]const u8 = null,
    created_at: i64,
    updated_at: i64,
};

const TaskStatus = enum {
    pending,
    in_progress,
    completed,
    cancelled,

    pub fn fromString(str: []const u8) ?TaskStatus {
        if (std.mem.eql(u8, str, "pending")) return .pending;
        if (std.mem.eql(u8, str, "in-progress")) return .in_progress;
        if (std.mem.eql(u8, str, "completed")) return .completed;
        if (std.mem.eql(u8, str, "cancelled")) return .cancelled;
        return null;
    }
};
```

### Thread-Safe Task Store

```zig
const TaskStore = struct {
    tasks: std.StringHashMap(Task),
    allocator: std.mem.Allocator,
    mutex: std.Thread.Mutex,
    id_counter: u32,

    pub fn init(allocator: std.mem.Allocator) TaskStore {
        return .{
            .tasks = std.StringHashMap(Task).init(allocator),
            .allocator = allocator,
            .mutex = std.Thread.Mutex{},
            .id_counter = 0,
        };
    }

    pub fn deinit(self: *TaskStore) void {
        var iter = self.tasks.iterator();
        while (iter.next()) |entry| {
            // Free all allocated memory
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.title);
            if (entry.value_ptr.description) |desc| {
                self.allocator.free(desc);
            }
            // ... free other fields
        }
        self.tasks.deinit();
    }

    pub fn createTask(self: *TaskStore, title: []const u8) !Task {
        self.mutex.lock();
        defer self.mutex.unlock();

        self.id_counter += 1;
        const id = try std.fmt.allocPrint(
            self.allocator, 
            "task-{d}", 
            .{self.id_counter}
        );
        
        const task = Task{
            .id = id,
            .title = try self.allocator.dupe(u8, title),
            .created_at = std.time.timestamp(),
            .updated_at = std.time.timestamp(),
        };

        try self.tasks.put(id, task);
        return task;
    }
};
```

### HTTP Request Handling

```zig
fn handleListTasks(req: *httpz.Request, res: *httpz.Response) !void {
    const status = req.param("status");
    const assigned_to = req.param("assigned_to");
    
    const tasks = try task_store.listTasks(
        status, 
        assigned_to, 
        req.arena
    );
    
    res.status = 200;
    try res.json(.{
        .tasks = tasks,
        .total_count = tasks.len,
    }, .{});
}

fn handleCreateTask(req: *httpz.Request, res: *httpz.Response) !void {
    const body = try req.jsonObject() orelse {
        res.status = 400;
        try res.json(.{ .error = "Invalid JSON" }, .{});
        return;
    };
    
    const title = body.get("title") orelse {
        res.status = 400;
        try res.json(.{ .error = "Title required" }, .{});
        return;
    };
    
    const task = try task_store.createTask(title.string);
    res.status = 201;
    try res.json(task, .{});
}
```

## Memory Management Patterns

Zig's explicit memory management requires careful attention:

### Arena Allocator Pattern

```zig
fn processRequest(base_allocator: std.mem.Allocator) !void {
    // Create arena for request lifetime
    var arena = std.heap.ArenaAllocator.init(base_allocator);
    defer arena.deinit();  // Frees all allocations at once
    
    const allocator = arena.allocator();
    
    // All allocations use arena
    const data = try allocator.alloc(u8, 1024);
    const string = try allocator.dupe(u8, "hello");
    
    // No need to free individually
}
```

### Resource Management

```zig
fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();  // Always closes file
    
    const stat = try file.stat();
    const contents = try allocator.alloc(u8, stat.size);
    errdefer allocator.free(contents);  // Free on error
    
    _ = try file.read(contents);
    return contents;
}
```

## Compile-Time Features

Zig's compile-time execution enables powerful metaprogramming:

### Generic Data Structures

```zig
fn ArrayList(comptime T: type) type {
    return struct {
        items: []T,
        capacity: usize,
        allocator: std.mem.Allocator,
        
        const Self = @This();
        
        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .items = &[_]T{},
                .capacity = 0,
                .allocator = allocator,
            };
        }
        
        pub fn append(self: *Self, item: T) !void {
            if (self.items.len == self.capacity) {
                try self.ensureCapacity(self.capacity * 2 + 8);
            }
            self.items.len += 1;
            self.items[self.items.len - 1] = item;
        }
    };
}

// Usage
const TaskList = ArrayList(Task);
```

### Compile-Time Validation

```zig
fn validateConfig(comptime config: Config) void {
    if (config.port < 1024) {
        @compileError("Port must be >= 1024");
    }
    if (config.max_connections == 0) {
        @compileError("Max connections must be > 0");
    }
}

const config = Config{ .port = 8080, .max_connections = 100 };
comptime validateConfig(config);  // Validated at compile time
```

## Error Handling Patterns

Zig's error handling is explicit and composable:

### Error Sets

```zig
const TaskError = error{
    TaskNotFound,
    InvalidStatus,
    PermissionDenied,
};

const DatabaseError = error{
    ConnectionFailed,
    QueryTimeout,
};

// Combine error sets
const AppError = TaskError || DatabaseError;

fn updateTask(id: []const u8) AppError!Task {
    const task = getTask(id) catch |err| switch (err) {
        error.TaskNotFound => return error.TaskNotFound,
        else => return err,
    };
    
    return saveTask(task) catch |err| switch (err) {
        error.ConnectionFailed => return error.ConnectionFailed,
        else => return err,
    };
}
```

### Try Pattern

```zig
fn processTask(id: []const u8) !void {
    // try is shorthand for: x catch |err| return err
    const task = try getTask(id);
    try validateTask(task);
    try updateTask(task);
    try notifyUpdate(task);
}
```

## Testing in Zig

Zig has built-in testing support:

```zig
const testing = std.testing;

test "task creation" {
    var store = TaskStore.init(testing.allocator);
    defer store.deinit();
    
    const task = try store.createTask("Test Task");
    
    try testing.expectEqualStrings("Test Task", task.title);
    try testing.expect(task.status == .pending);
}

test "concurrent access" {
    var store = TaskStore.init(testing.allocator);
    defer store.deinit();
    
    var threads: [10]std.Thread = undefined;
    
    for (&threads, 0..) |*thread, i| {
        thread.* = try std.Thread.spawn(.{}, createTaskThread, .{
            &store,
            try std.fmt.allocPrint(testing.allocator, "Task {d}", .{i}),
        });
    }
    
    for (threads) |thread| {
        thread.join();
    }
    
    try testing.expect(store.tasks.count() == 10);
}
```

## Cross-Compilation

Zig makes cross-compilation trivial:

```zig
// build.zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    // Define targets
    const targets = [_]std.zig.CrossTarget{
        .{ .cpu_arch = .x86_64, .os_tag = .linux },
        .{ .cpu_arch = .aarch64, .os_tag = .linux },
        .{ .cpu_arch = .x86_64, .os_tag = .windows },
        .{ .cpu_arch = .aarch64, .os_tag = .macos },
    };
    
    for (targets) |target| {
        const exe = b.addExecutable(.{
            .name = "task-server",
            .root_source_file = .{ .path = "src/main.zig" },
            .target = target,
            .optimize = .ReleaseFast,
        });
        
        b.installArtifact(exe);
    }
}
```

Build for all targets:
```bash
zig build -Dtarget=x86_64-linux
zig build -Dtarget=aarch64-macos
zig build -Dtarget=x86_64-windows
```

## Performance Optimization

### SIMD Operations

```zig
fn sumArray(values: []const f32) f32 {
    const Vector = @Vector(4, f32);
    var sum: Vector = @splat(0);
    
    var i: usize = 0;
    while (i + 4 <= values.len) : (i += 4) {
        const vec: Vector = values[i..][0..4].*;
        sum += vec;
    }
    
    var total = @reduce(.Add, sum);
    
    // Handle remaining elements
    while (i < values.len) : (i += 1) {
        total += values[i];
    }
    
    return total;
}
```

### Custom Allocators

```zig
const FixedBufferAllocator = struct {
    buffer: []u8,
    pos: usize = 0,
    
    pub fn allocator(self: *@This()) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }
    
    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self = @ptrCast(*FixedBufferAllocator, ctx);
        const aligned_pos = std.mem.alignForward(self.pos, ptr_align);
        
        if (aligned_pos + len > self.buffer.len) return null;
        
        const result = self.buffer[aligned_pos..];
        self.pos = aligned_pos + len;
        return result.ptr;
    }
};
```

## Best Practices

1. **Explicit Allocator Usage**: Always pass allocators as parameters
2. **Defer for Cleanup**: Use defer and errdefer consistently
3. **Compile-Time When Possible**: Leverage comptime for zero-cost abstractions
4. **Error Handling**: Return error unions and handle all cases
5. **No Hidden Behavior**: Keep all operations visible and explicit
6. **Test Everything**: Use Zig's built-in testing framework

## Conclusion

Zig represents a fresh approach to systems programming, combining the control of C with modern language features and safety. Its emphasis on simplicity, explicit behavior, and compile-time computation makes it an excellent choice for systems where performance and reliability are critical.

The explicit nature of Zig might seem verbose initially, but it eliminates entire classes of bugs and makes code behavior completely predictable. As we've seen with our REST API implementation, Zig can handle complex applications while maintaining clarity and performance.

Zig's future looks promising as it continues to evolve toward its 1.0 release, with growing adoption in systems programming, embedded development, and as a better C compiler through its excellent C interoperability.