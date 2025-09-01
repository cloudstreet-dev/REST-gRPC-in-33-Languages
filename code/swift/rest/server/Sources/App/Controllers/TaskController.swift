import Fluent
import Vapor

struct TaskController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tasks = routes.grouped("api", "v1", "tasks")
        tasks.get(use: list)
        tasks.post(use: create)
        tasks.group(":taskID") { task in
            task.get(use: get)
            task.put(use: update)
            task.delete(use: delete)
            task.patch("status", use: updateStatus)
        }
    }

    func list(req: Request) async throws -> TasksResponse {
        // Parse query parameters
        let pageSize = min(req.query[Int.self, at: "pageSize"] ?? 20, 100)
        let pageToken = req.query[String.self, at: "pageToken"]
        let statusString = req.query[String.self, at: "status"]
        let assignedTo = req.query[String.self, at: "assignedTo"]
        let tagsString = req.query[String.self, at: "tags"]
        let sortOrder = req.query[String.self, at: "sortOrder"]

        // Build query
        var query = Task.query(on: req.db)

        // Apply filters
        if let statusString = statusString,
           let status = TaskStatus(rawValue: statusString.uppercased()) {
            query = query.filter(\.$status == status)
        }

        if let assignedTo = assignedTo {
            query = query.filter(\.$assignedTo == assignedTo)
        }

        if let tagsString = tagsString {
            let requiredTags = tagsString.split(separator: ",").map(String.init)
            for tag in requiredTags {
                query = query.filter(\.$tags ~~ [tag])
            }
        }

        // Apply sorting
        switch sortOrder {
        case "created_desc":
            query = query.sort(\.$createdAt, .descending)
        case "updated_desc":
            query = query.sort(\.$updatedAt, .descending)
        case "priority_desc":
            query = query.sort(\.$priority, .descending).sort(\.$createdAt)
        default:
            query = query.sort(\.$createdAt)
        }

        // Get total count before pagination
        let totalCount = try await query.count()

        // Apply pagination
        let offset = pageToken.flatMap(Int.init) ?? 0
        let tasks = try await query.offset(offset).limit(pageSize).all()

        let nextPageToken = (offset + pageSize < totalCount) ? String(offset + pageSize) : nil

        return TasksResponse(
            items: tasks,
            pageSize: pageSize,
            nextPageToken: nextPageToken,
            totalCount: totalCount
        )
    }

    func get(req: Request) async throws -> Task {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound, reason: "Task not found")
        }
        return task
    }

    func create(req: Request) async throws -> Response {
        let createRequest = try req.content.decode(CreateTaskRequest.self)

        // Validate title
        guard !createRequest.title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
            throw Abort(.badRequest, reason: "Title is required")
        }

        guard createRequest.title.count <= 200 else {
            throw Abort(.badRequest, reason: "Title must be 200 characters or less")
        }

        let task = Task(
            title: createRequest.title,
            description: createRequest.description,
            priority: createRequest.priority ?? .medium,
            tags: createRequest.tags ?? [],
            assignedTo: createRequest.assignedTo,
            dueDate: createRequest.dueDate
        )

        try await task.save(on: req.db)

        let response = Response(status: .created)
        try response.content.encode(task)
        return response
    }

    func update(req: Request) async throws -> Task {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound, reason: "Task not found")
        }

        let updateRequest = try req.content.decode(UpdateTaskRequest.self)

        // Update fields if provided
        if let title = updateRequest.title {
            guard !title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
                throw Abort(.badRequest, reason: "Title cannot be empty")
            }
            guard title.count <= 200 else {
                throw Abort(.badRequest, reason: "Title must be 200 characters or less")
            }
            task.title = title
        }

        if let description = updateRequest.description {
            task.description = description
        }

        if let status = updateRequest.status {
            task.status = status
        }

        if let priority = updateRequest.priority {
            task.priority = priority
        }

        if let tags = updateRequest.tags {
            task.tags = tags
        }

        if let assignedTo = updateRequest.assignedTo {
            task.assignedTo = assignedTo
        }

        if let dueDate = updateRequest.dueDate {
            task.dueDate = dueDate
        }

        try await task.save(on: req.db)
        return task
    }

    func updateStatus(req: Request) async throws -> Task {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound, reason: "Task not found")
        }

        let statusRequest = try req.content.decode(UpdateStatusRequest.self)
        task.status = statusRequest.status

        try await task.save(on: req.db)
        return task
    }

    func delete(req: Request) async throws -> HTTPStatus {
        guard let task = try await Task.find(req.parameters.get("taskID"), on: req.db) else {
            throw Abort(.notFound, reason: "Task not found")
        }

        try await task.delete(on: req.db)
        return .noContent
    }
}