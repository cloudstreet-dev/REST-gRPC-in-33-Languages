const std = @import("std");
const http = std.http;
const json = std.json;

const Task = struct {
    id: []const u8,
    title: []const u8,
    description: ?[]const u8 = null,
    status: []const u8,
    priority: []const u8,
    tags: [][]const u8,
    assigned_to: ?[]const u8 = null,
    created_at: i64,
    updated_at: i64,
};

const ListTasksResponse = struct {
    tasks: []Task,
    total_count: usize,
};

const CreateTaskRequest = struct {
    title: []const u8,
    description: ?[]const u8 = null,
    priority: ?[]const u8 = null,
    tags: ?[][]const u8 = null,
    assigned_to: ?[]const u8 = null,
};

const UpdateTaskRequest = struct {
    title: ?[]const u8 = null,
    description: ?[]const u8 = null,
    status: ?[]const u8 = null,
    priority: ?[]const u8 = null,
    tags: ?[][]const u8 = null,
    assigned_to: ?[]const u8 = null,
};

const UpdateStatusRequest = struct {
    status: []const u8,
};

const TaskClient = struct {
    allocator: std.mem.Allocator,
    base_url: []const u8,
    client: http.Client,

    pub fn init(allocator: std.mem.Allocator, base_url: []const u8) TaskClient {
        return .{
            .allocator = allocator,
            .base_url = base_url,
            .client = http.Client{ .allocator = allocator },
        };
    }

    pub fn deinit(self: *TaskClient) void {
        self.client.deinit();
    }

    pub fn listTasks(self: *TaskClient) !ListTasksResponse {
        const url = try std.fmt.allocPrint(self.allocator, "{s}/api/tasks", .{self.base_url});
        defer self.allocator.free(url);

        const uri = try std.Uri.parse(url);
        var request = try self.client.request(.GET, uri, .{ .allocator = self.allocator }, .{});
        defer request.deinit();

        try request.start();
        try request.wait();

        if (request.response.status != .ok) {
            return error.RequestFailed;
        }

        const body = try request.reader().readAllAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(body);

        const parsed = try json.parseFromSlice(ListTasksResponse, self.allocator, body, .{});
        return parsed.value;
    }

    pub fn getTask(self: *TaskClient, task_id: []const u8) !Task {
        const url = try std.fmt.allocPrint(self.allocator, "{s}/api/tasks/{s}", .{ self.base_url, task_id });
        defer self.allocator.free(url);

        const uri = try std.Uri.parse(url);
        var request = try self.client.request(.GET, uri, .{ .allocator = self.allocator }, .{});
        defer request.deinit();

        try request.start();
        try request.wait();

        if (request.response.status == .not_found) {
            return error.TaskNotFound;
        }
        if (request.response.status != .ok) {
            return error.RequestFailed;
        }

        const body = try request.reader().readAllAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(body);

        const parsed = try json.parseFromSlice(Task, self.allocator, body, .{});
        return parsed.value;
    }

    pub fn createTask(self: *TaskClient, task_request: CreateTaskRequest) !Task {
        const url = try std.fmt.allocPrint(self.allocator, "{s}/api/tasks", .{self.base_url});
        defer self.allocator.free(url);

        const body_json = try json.stringifyAlloc(self.allocator, task_request, .{});
        defer self.allocator.free(body_json);

        const uri = try std.Uri.parse(url);
        var request = try self.client.request(.POST, uri, .{ .allocator = self.allocator }, .{});
        defer request.deinit();

        request.transfer_encoding = .{ .content_length = body_json.len };
        try request.headers.append("Content-Type", "application/json");

        try request.start();
        try request.writeAll(body_json);
        try request.finish();
        try request.wait();

        if (request.response.status != .created and request.response.status != .ok) {
            return error.RequestFailed;
        }

        const response_body = try request.reader().readAllAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(response_body);

        const parsed = try json.parseFromSlice(Task, self.allocator, response_body, .{});
        return parsed.value;
    }

    pub fn updateTask(self: *TaskClient, task_id: []const u8, update_request: UpdateTaskRequest) !Task {
        const url = try std.fmt.allocPrint(self.allocator, "{s}/api/tasks/{s}", .{ self.base_url, task_id });
        defer self.allocator.free(url);

        const body_json = try json.stringifyAlloc(self.allocator, update_request, .{});
        defer self.allocator.free(body_json);

        const uri = try std.Uri.parse(url);
        var request = try self.client.request(.PUT, uri, .{ .allocator = self.allocator }, .{});
        defer request.deinit();

        request.transfer_encoding = .{ .content_length = body_json.len };
        try request.headers.append("Content-Type", "application/json");

        try request.start();
        try request.writeAll(body_json);
        try request.finish();
        try request.wait();

        if (request.response.status == .not_found) {
            return error.TaskNotFound;
        }
        if (request.response.status != .ok) {
            return error.RequestFailed;
        }

        const response_body = try request.reader().readAllAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(response_body);

        const parsed = try json.parseFromSlice(Task, self.allocator, response_body, .{});
        return parsed.value;
    }

    pub fn updateTaskStatus(self: *TaskClient, task_id: []const u8, status: []const u8) !Task {
        const url = try std.fmt.allocPrint(self.allocator, "{s}/api/tasks/{s}/status", .{ self.base_url, task_id });
        defer self.allocator.free(url);

        const status_request = UpdateStatusRequest{ .status = status };
        const body_json = try json.stringifyAlloc(self.allocator, status_request, .{});
        defer self.allocator.free(body_json);

        const uri = try std.Uri.parse(url);
        var request = try self.client.request(.PATCH, uri, .{ .allocator = self.allocator }, .{});
        defer request.deinit();

        request.transfer_encoding = .{ .content_length = body_json.len };
        try request.headers.append("Content-Type", "application/json");

        try request.start();
        try request.writeAll(body_json);
        try request.finish();
        try request.wait();

        if (request.response.status == .not_found) {
            return error.TaskNotFound;
        }
        if (request.response.status != .ok) {
            return error.RequestFailed;
        }

        const response_body = try request.reader().readAllAlloc(self.allocator, 1024 * 1024);
        defer self.allocator.free(response_body);

        const parsed = try json.parseFromSlice(Task, self.allocator, response_body, .{});
        return parsed.value;
    }

    pub fn deleteTask(self: *TaskClient, task_id: []const u8) !void {
        const url = try std.fmt.allocPrint(self.allocator, "{s}/api/tasks/{s}", .{ self.base_url, task_id });
        defer self.allocator.free(url);

        const uri = try std.Uri.parse(url);
        var request = try self.client.request(.DELETE, uri, .{ .allocator = self.allocator }, .{});
        defer request.deinit();

        try request.start();
        try request.wait();

        if (request.response.status == .not_found) {
            return error.TaskNotFound;
        }
        if (request.response.status != .no_content and request.response.status != .ok) {
            return error.RequestFailed;
        }
    }
};

fn printBanner() void {
    std.debug.print("╔════════════════════════════════════════════════╗\n", .{});
    std.debug.print("║         Zig Task Management REST Client        ║\n", .{});
    std.debug.print("║            Testing API Operations              ║\n", .{});
    std.debug.print("╚════════════════════════════════════════════════╝\n\n", .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    printBanner();

    var client = TaskClient.init(allocator, "http://localhost:8080");
    defer client.deinit();

    // 1. List all tasks
    std.debug.print("1. Listing all tasks...\n", .{});
    if (client.listTasks()) |response| {
        defer allocator.free(response.tasks);
        std.debug.print("   Found {d} tasks\n", .{response.total_count});
        for (response.tasks) |task| {
            std.debug.print("   - [{s}] {s} ({s})\n", .{ task.id, task.title, task.status });
        }
    } else |err| {
        std.debug.print("   Error: {}\n", .{err});
    }

    // 2. Create a new task
    std.debug.print("\n2. Creating a new task...\n", .{});
    const new_task_request = CreateTaskRequest{
        .title = "Learn Zig comptime",
        .description = "Master compile-time code execution in Zig",
        .priority = "high",
        .tags = &[_][]const u8{ "zig", "comptime", "metaprogramming" },
        .assigned_to = "zig-team",
    };

    if (client.createTask(new_task_request)) |task| {
        std.debug.print("   Created task: {s}\n", .{task.title});
        std.debug.print("   ID: {s}\n", .{task.id});
        std.debug.print("   Priority: {s}\n", .{task.priority});
        std.debug.print("   Tags: ", .{});
        for (task.tags, 0..) |tag, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{tag});
        }
        std.debug.print("\n", .{});

        const task_id = try allocator.dupe(u8, task.id);
        defer allocator.free(task_id);

        // 3. Get task details
        std.debug.print("\n3. Getting task details...\n", .{});
        if (client.getTask(task_id)) |task_detail| {
            std.debug.print("   Title: {s}\n", .{task_detail.title});
            if (task_detail.description) |desc| {
                std.debug.print("   Description: {s}\n", .{desc});
            }
            std.debug.print("   Status: {s}\n", .{task_detail.status});
            if (task_detail.assigned_to) |assignee| {
                std.debug.print("   Assigned to: {s}\n", .{assignee});
            }
        } else |err| {
            std.debug.print("   Error: {}\n", .{err});
        }

        // 4. Update task status
        std.debug.print("\n4. Updating task status to 'in-progress'...\n", .{});
        if (client.updateTaskStatus(task_id, "in-progress")) |updated| {
            std.debug.print("   Updated status to: {s}\n", .{updated.status});
        } else |err| {
            std.debug.print("   Error: {}\n", .{err});
        }

        // 5. Update task details
        std.debug.print("\n5. Updating task details...\n", .{});
        const update_request = UpdateTaskRequest{
            .title = "Master Zig async/await",
            .priority = "urgent",
        };
        if (client.updateTask(task_id, update_request)) |updated| {
            std.debug.print("   Updated title: {s}\n", .{updated.title});
            std.debug.print("   Updated priority: {s}\n", .{updated.priority});
        } else |err| {
            std.debug.print("   Error: {}\n", .{err});
        }

        // 6. Delete the task
        std.debug.print("\n6. Deleting the task...\n", .{});
        if (client.deleteTask(task_id)) {
            std.debug.print("   Task deleted successfully\n", .{});
        } else |err| {
            std.debug.print("   Error: {}\n", .{err});
        }

        // 7. Verify deletion
        std.debug.print("\n7. Verifying deletion...\n", .{});
        if (client.getTask(task_id)) |_| {
            std.debug.print("   Error: Task still exists\n", .{});
        } else |err| {
            if (err == error.TaskNotFound) {
                std.debug.print("   Task not found (as expected)\n", .{});
            } else {
                std.debug.print("   Error: {}\n", .{err});
            }
        }
    } else |err| {
        std.debug.print("   Error: {}\n", .{err});
    }

    std.debug.print("\n✅ Demo completed successfully!\n", .{});
}