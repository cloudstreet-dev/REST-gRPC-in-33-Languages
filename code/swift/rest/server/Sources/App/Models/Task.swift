import Fluent
import Foundation
import Vapor

enum TaskStatus: String, Codable, CaseIterable {
    case pending = "PENDING"
    case inProgress = "IN_PROGRESS"
    case completed = "COMPLETED"
    case cancelled = "CANCELLED"
    case archived = "ARCHIVED"
}

enum TaskPriority: String, Codable, CaseIterable {
    case low = "LOW"
    case medium = "MEDIUM"
    case high = "HIGH"
    case critical = "CRITICAL"
}

final class Task: Model, Content {
    static let schema = "tasks"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "title")
    var title: String

    @OptionalField(key: "description")
    var description: String?

    @Enum(key: "status")
    var status: TaskStatus

    @Enum(key: "priority")
    var priority: TaskPriority

    @Field(key: "tags")
    var tags: [String]

    @OptionalField(key: "assigned_to")
    var assignedTo: String?

    @OptionalField(key: "due_date")
    var dueDate: Date?

    @Timestamp(key: "created_at", on: .create)
    var createdAt: Date?

    @Timestamp(key: "updated_at", on: .update)
    var updatedAt: Date?

    @OptionalField(key: "created_by")
    var createdBy: String?

    @OptionalField(key: "updated_by")
    var updatedBy: String?

    init() { }

    init(id: UUID? = nil,
         title: String,
         description: String? = nil,
         status: TaskStatus = .pending,
         priority: TaskPriority = .medium,
         tags: [String] = [],
         assignedTo: String? = nil,
         dueDate: Date? = nil,
         createdBy: String? = nil,
         updatedBy: String? = nil) {
        self.id = id
        self.title = title
        self.description = description
        self.status = status
        self.priority = priority
        self.tags = tags
        self.assignedTo = assignedTo
        self.dueDate = dueDate
        self.createdBy = createdBy
        self.updatedBy = updatedBy
    }
}

struct CreateTaskRequest: Content {
    let title: String
    let description: String?
    let priority: TaskPriority?
    let tags: [String]?
    let assignedTo: String?
    let dueDate: Date?
}

struct UpdateTaskRequest: Content {
    let title: String?
    let description: String?
    let status: TaskStatus?
    let priority: TaskPriority?
    let tags: [String]?
    let assignedTo: String?
    let dueDate: Date?
}

struct UpdateStatusRequest: Content {
    let status: TaskStatus
}

struct TasksResponse: Content {
    let items: [Task]
    let pageSize: Int
    let nextPageToken: String?
    let totalCount: Int
}

struct ErrorResponse: Content {
    let error: String
    let message: String
}