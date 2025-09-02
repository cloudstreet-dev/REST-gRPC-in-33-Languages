const std = @import("std");
const httpz = @import("httpz");

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
        if (std.mem.eql(u8, str, "in-progress") or std.mem.eql(u8, str, "in_progress")) return .in_progress;
        if (std.mem.eql(u8, str, "completed")) return .completed;
        if (std.mem.eql(u8, str, "cancelled")) return .cancelled;
        return null;
    }

    pub fn toString(self: TaskStatus) []const u8 {
        return switch (self) {
            .pending => "pending",
            .in_progress => "in-progress",
            .completed => "completed",
            .cancelled => "cancelled",
        };
    }
};

const TaskPriority = enum {
    low,
    medium,
    high,
    urgent,

    pub fn fromString(str: []const u8) ?TaskPriority {
        if (std.mem.eql(u8, str, "low")) return .low;
        if (std.mem.eql(u8, str, "medium")) return .medium;
        if (std.mem.eql(u8, str, "high")) return .high;
        if (std.mem.eql(u8, str, "urgent")) return .urgent;
        return null;
    }

    pub fn toString(self: TaskPriority) []const u8 {
        return switch (self) {
            .low => "low",
            .medium => "medium",
            .high => "high",
            .urgent => "urgent",
        };
    }
};

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
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.title);
            if (entry.value_ptr.description) |desc| {
                self.allocator.free(desc);
            }
            if (entry.value_ptr.assigned_to) |assignee| {
                self.allocator.free(assignee);
            }
            for (entry.value_ptr.tags) |tag| {
                self.allocator.free(tag);
            }
            if (entry.value_ptr.tags.len > 0) {
                self.allocator.free(entry.value_ptr.tags);
            }
        }
        self.tasks.deinit();
    }

    pub fn createTask(self: *TaskStore, title: []const u8, description: ?[]const u8, priority: ?[]const u8, tags: ?[][]const u8, assigned_to: ?[]const u8) !Task {
        self.mutex.lock();
        defer self.mutex.unlock();

        self.id_counter += 1;
        const id = try std.fmt.allocPrint(self.allocator, "task-{d}", .{self.id_counter});
        
        const title_copy = try self.allocator.dupe(u8, title);
        const desc_copy = if (description) |d| try self.allocator.dupe(u8, d) else null;
        const assignee_copy = if (assigned_to) |a| try self.allocator.dupe(u8, a) else null;
        
        var tags_copy: [][]const u8 = &.{};
        if (tags) |t| {
            tags_copy = try self.allocator.alloc([]const u8, t.len);
            for (t, 0..) |tag, i| {
                tags_copy[i] = try self.allocator.dupe(u8, tag);
            }
        }

        const priority_enum = if (priority) |p| TaskPriority.fromString(p) orelse .medium else .medium;
        
        const now = std.time.timestamp();
        const task = Task{
            .id = id,
            .title = title_copy,
            .description = desc_copy,
            .status = .pending,
            .priority = priority_enum,
            .tags = tags_copy,
            .assigned_to = assignee_copy,
            .created_at = now,
            .updated_at = now,
        };

        try self.tasks.put(id, task);
        return task;
    }

    pub fn getTask(self: *TaskStore, id: []const u8) ?Task {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.tasks.get(id);
    }

    pub fn updateTask(self: *TaskStore, id: []const u8, title: ?[]const u8, description: ?[]const u8, status: ?[]const u8, priority: ?[]const u8, tags: ?[][]const u8, assigned_to: ?[]const u8) !?Task {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.tasks.getPtr(id)) |task| {
            if (title) |t| {
                self.allocator.free(task.title);
                task.title = try self.allocator.dupe(u8, t);
            }
            if (description) |d| {
                if (task.description) |old_desc| {
                    self.allocator.free(old_desc);
                }
                task.description = try self.allocator.dupe(u8, d);
            }
            if (status) |s| {
                if (TaskStatus.fromString(s)) |status_enum| {
                    task.status = status_enum;
                }
            }
            if (priority) |p| {
                if (TaskPriority.fromString(p)) |priority_enum| {
                    task.priority = priority_enum;
                }
            }
            if (tags) |t| {
                for (task.tags) |tag| {
                    self.allocator.free(tag);
                }
                if (task.tags.len > 0) {
                    self.allocator.free(task.tags);
                }
                task.tags = try self.allocator.alloc([]const u8, t.len);
                for (t, 0..) |tag, i| {
                    task.tags[i] = try self.allocator.dupe(u8, tag);
                }
            }
            if (assigned_to) |a| {
                if (task.assigned_to) |old_assignee| {
                    self.allocator.free(old_assignee);
                }
                task.assigned_to = try self.allocator.dupe(u8, a);
            }
            task.updated_at = std.time.timestamp();
            return task.*;
        }
        return null;
    }

    pub fn deleteTask(self: *TaskStore, id: []const u8) bool {
        self.mutex.lock();
        defer self.mutex.unlock();

        if (self.tasks.fetchRemove(id)) |entry| {
            self.allocator.free(entry.key);
            self.allocator.free(entry.value.title);
            if (entry.value.description) |desc| {
                self.allocator.free(desc);
            }
            if (entry.value.assigned_to) |assignee| {
                self.allocator.free(assignee);
            }
            for (entry.value.tags) |tag| {
                self.allocator.free(tag);
            }
            if (entry.value.tags.len > 0) {
                self.allocator.free(entry.value.tags);
            }
            return true;
        }
        return false;
    }

    pub fn listTasks(self: *TaskStore, status_filter: ?[]const u8, assigned_to_filter: ?[]const u8, allocator: std.mem.Allocator) ![]Task {
        self.mutex.lock();
        defer self.mutex.unlock();

        var filtered = std.ArrayList(Task).init(allocator);
        defer filtered.deinit();

        var iter = self.tasks.iterator();
        while (iter.next()) |entry| {
            var include = true;
            
            if (status_filter) |sf| {
                if (TaskStatus.fromString(sf)) |status_enum| {
                    if (entry.value_ptr.status != status_enum) {
                        include = false;
                    }
                }
            }
            
            if (assigned_to_filter) |af| {
                if (entry.value_ptr.assigned_to) |assignee| {
                    if (!std.mem.eql(u8, assignee, af)) {
                        include = false;
                    }
                } else {
                    include = false;
                }
            }
            
            if (include) {
                try filtered.append(entry.value_ptr.*);
            }
        }

        return filtered.toOwnedSlice();
    }
};

var task_store: TaskStore = undefined;

// Handlers
fn handleHealth(req: *httpz.Request, res: *httpz.Response) !void {
    _ = req;
    res.status = 200;
    try res.json(.{ .status = "healthy", .service = "task-api", .version = "1.0.0" }, .{});
}

fn handleListTasks(req: *httpz.Request, res: *httpz.Response) !void {
    const status = req.param("status");
    const assigned_to = req.param("assigned_to");
    
    const tasks = try task_store.listTasks(status, assigned_to, req.arena);
    
    var json_tasks = try req.arena.alloc(std.json.Value, tasks.len);
    for (tasks, 0..) |task, i| {
        var task_obj = std.json.ObjectMap.init(req.arena);
        try task_obj.put("id", .{ .string = task.id });
        try task_obj.put("title", .{ .string = task.title });
        if (task.description) |desc| {
            try task_obj.put("description", .{ .string = desc });
        }
        try task_obj.put("status", .{ .string = task.status.toString() });
        try task_obj.put("priority", .{ .string = task.priority.toString() });
        
        var tags_array = try req.arena.alloc(std.json.Value, task.tags.len);
        for (task.tags, 0..) |tag, j| {
            tags_array[j] = .{ .string = tag };
        }
        try task_obj.put("tags", .{ .array = .{ .items = tags_array } });
        
        if (task.assigned_to) |assignee| {
            try task_obj.put("assigned_to", .{ .string = assignee });
        }
        try task_obj.put("created_at", .{ .integer = task.created_at });
        try task_obj.put("updated_at", .{ .integer = task.updated_at });
        
        json_tasks[i] = .{ .object = task_obj };
    }
    
    res.status = 200;
    try res.json(.{
        .tasks = json_tasks,
        .total_count = tasks.len,
    }, .{});
}

fn handleGetTask(req: *httpz.Request, res: *httpz.Response) !void {
    const id = req.param("id") orelse {
        res.status = 400;
        try res.json(.{ .error = "Missing task ID" }, .{});
        return;
    };
    
    if (task_store.getTask(id)) |task| {
        var task_obj = std.json.ObjectMap.init(req.arena);
        try task_obj.put("id", .{ .string = task.id });
        try task_obj.put("title", .{ .string = task.title });
        if (task.description) |desc| {
            try task_obj.put("description", .{ .string = desc });
        }
        try task_obj.put("status", .{ .string = task.status.toString() });
        try task_obj.put("priority", .{ .string = task.priority.toString() });
        
        var tags_array = try req.arena.alloc(std.json.Value, task.tags.len);
        for (task.tags, 0..) |tag, i| {
            tags_array[i] = .{ .string = tag };
        }
        try task_obj.put("tags", .{ .array = .{ .items = tags_array } });
        
        if (task.assigned_to) |assignee| {
            try task_obj.put("assigned_to", .{ .string = assignee });
        }
        try task_obj.put("created_at", .{ .integer = task.created_at });
        try task_obj.put("updated_at", .{ .integer = task.updated_at });
        
        res.status = 200;
        try res.json(.{ .object = task_obj }, .{});
    } else {
        res.status = 404;
        try res.json(.{ .error = "Task not found" }, .{});
    }
}

fn handleCreateTask(req: *httpz.Request, res: *httpz.Response) !void {
    const body = try req.jsonObject() orelse {
        res.status = 400;
        try res.json(.{ .error = "Invalid JSON body" }, .{});
        return;
    };
    
    const title = body.get("title") orelse {
        res.status = 400;
        try res.json(.{ .error = "Missing required field: title" }, .{});
        return;
    };
    
    const description = body.get("description");
    const priority = body.get("priority");
    const assigned_to = body.get("assigned_to");
    
    // Handle tags array
    var tags: ?[][]const u8 = null;
    if (body.get("tags")) |tags_value| {
        if (tags_value == .array) {
            var tags_list = try req.arena.alloc([]const u8, tags_value.array.items.len);
            for (tags_value.array.items, 0..) |tag, i| {
                if (tag == .string) {
                    tags_list[i] = tag.string;
                }
            }
            tags = tags_list;
        }
    }
    
    const task = try task_store.createTask(
        title.string,
        if (description) |d| d.string else null,
        if (priority) |p| p.string else null,
        tags,
        if (assigned_to) |a| a.string else null,
    );
    
    var task_obj = std.json.ObjectMap.init(req.arena);
    try task_obj.put("id", .{ .string = task.id });
    try task_obj.put("title", .{ .string = task.title });
    if (task.description) |desc| {
        try task_obj.put("description", .{ .string = desc });
    }
    try task_obj.put("status", .{ .string = task.status.toString() });
    try task_obj.put("priority", .{ .string = task.priority.toString() });
    
    var tags_array = try req.arena.alloc(std.json.Value, task.tags.len);
    for (task.tags, 0..) |tag, i| {
        tags_array[i] = .{ .string = tag };
    }
    try task_obj.put("tags", .{ .array = .{ .items = tags_array } });
    
    if (task.assigned_to) |assignee| {
        try task_obj.put("assigned_to", .{ .string = assignee });
    }
    try task_obj.put("created_at", .{ .integer = task.created_at });
    try task_obj.put("updated_at", .{ .integer = task.updated_at });
    
    res.status = 201;
    try res.json(.{ .object = task_obj }, .{});
}

fn handleUpdateTask(req: *httpz.Request, res: *httpz.Response) !void {
    const id = req.param("id") orelse {
        res.status = 400;
        try res.json(.{ .error = "Missing task ID" }, .{});
        return;
    };
    
    const body = try req.jsonObject() orelse {
        res.status = 400;
        try res.json(.{ .error = "Invalid JSON body" }, .{});
        return;
    };
    
    const title = if (body.get("title")) |t| t.string else null;
    const description = if (body.get("description")) |d| d.string else null;
    const status = if (body.get("status")) |s| s.string else null;
    const priority = if (body.get("priority")) |p| p.string else null;
    const assigned_to = if (body.get("assigned_to")) |a| a.string else null;
    
    // Handle tags array
    var tags: ?[][]const u8 = null;
    if (body.get("tags")) |tags_value| {
        if (tags_value == .array) {
            var tags_list = try req.arena.alloc([]const u8, tags_value.array.items.len);
            for (tags_value.array.items, 0..) |tag, i| {
                if (tag == .string) {
                    tags_list[i] = tag.string;
                }
            }
            tags = tags_list;
        }
    }
    
    if (try task_store.updateTask(id, title, description, status, priority, tags, assigned_to)) |task| {
        var task_obj = std.json.ObjectMap.init(req.arena);
        try task_obj.put("id", .{ .string = task.id });
        try task_obj.put("title", .{ .string = task.title });
        if (task.description) |desc| {
            try task_obj.put("description", .{ .string = desc });
        }
        try task_obj.put("status", .{ .string = task.status.toString() });
        try task_obj.put("priority", .{ .string = task.priority.toString() });
        
        var tags_array = try req.arena.alloc(std.json.Value, task.tags.len);
        for (task.tags, 0..) |tag, i| {
            tags_array[i] = .{ .string = tag };
        }
        try task_obj.put("tags", .{ .array = .{ .items = tags_array } });
        
        if (task.assigned_to) |assignee| {
            try task_obj.put("assigned_to", .{ .string = assignee });
        }
        try task_obj.put("created_at", .{ .integer = task.created_at });
        try task_obj.put("updated_at", .{ .integer = task.updated_at });
        
        res.status = 200;
        try res.json(.{ .object = task_obj }, .{});
    } else {
        res.status = 404;
        try res.json(.{ .error = "Task not found" }, .{});
    }
}

fn handleUpdateTaskStatus(req: *httpz.Request, res: *httpz.Response) !void {
    const id = req.param("id") orelse {
        res.status = 400;
        try res.json(.{ .error = "Missing task ID" }, .{});
        return;
    };
    
    const body = try req.jsonObject() orelse {
        res.status = 400;
        try res.json(.{ .error = "Invalid JSON body" }, .{});
        return;
    };
    
    const status = body.get("status") orelse {
        res.status = 400;
        try res.json(.{ .error = "Missing required field: status" }, .{});
        return;
    };
    
    if (try task_store.updateTask(id, null, null, status.string, null, null, null)) |task| {
        var task_obj = std.json.ObjectMap.init(req.arena);
        try task_obj.put("id", .{ .string = task.id });
        try task_obj.put("title", .{ .string = task.title });
        try task_obj.put("status", .{ .string = task.status.toString() });
        try task_obj.put("updated_at", .{ .integer = task.updated_at });
        
        res.status = 200;
        try res.json(.{ .object = task_obj }, .{});
    } else {
        res.status = 404;
        try res.json(.{ .error = "Task not found" }, .{});
    }
}

fn handleDeleteTask(req: *httpz.Request, res: *httpz.Response) !void {
    const id = req.param("id") orelse {
        res.status = 400;
        try res.json(.{ .error = "Missing task ID" }, .{});
        return;
    };
    
    if (task_store.deleteTask(id)) {
        res.status = 204;
    } else {
        res.status = 404;
        try res.json(.{ .error = "Task not found" }, .{});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    task_store = TaskStore.init(allocator);
    defer task_store.deinit();

    // Add some sample tasks
    _ = try task_store.createTask("Learn Zig", "Master the Zig programming language", "high", &[_][]const u8{ "zig", "learning" }, "developer");
    _ = try task_store.createTask("Build REST API", "Create a REST API server in Zig", "urgent", &[_][]const u8{ "zig", "api", "rest" }, "backend-team");
    _ = try task_store.createTask("Write Tests", "Add unit tests for the API", "medium", &[_][]const u8{ "testing", "quality" }, null);

    var server = try httpz.Server().init(allocator, .{ .port = 8080 });
    defer server.deinit();

    var router = server.router();
    
    // Define routes
    router.get("/health", handleHealth);
    router.get("/api/tasks", handleListTasks);
    router.get("/api/tasks/:id", handleGetTask);
    router.post("/api/tasks", handleCreateTask);
    router.put("/api/tasks/:id", handleUpdateTask);
    router.patch("/api/tasks/:id/status", handleUpdateTaskStatus);
    router.delete("/api/tasks/:id", handleDeleteTask);

    std.debug.print("🚀 Zig Task REST API Server\n", .{});
    std.debug.print("📍 Listening on http://localhost:8080\n", .{});
    std.debug.print("🔍 Health check: http://localhost:8080/health\n\n", .{});

    try server.listen();
}