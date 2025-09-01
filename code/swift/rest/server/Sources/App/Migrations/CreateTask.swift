import Fluent

struct CreateTask: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("tasks")
            .id()
            .field("title", .string, .required)
            .field("description", .string)
            .field("status", .string, .required)
            .field("priority", .string, .required)
            .field("tags", .array(of: .string), .required)
            .field("assigned_to", .string)
            .field("due_date", .datetime)
            .field("created_at", .datetime)
            .field("updated_at", .datetime)
            .field("created_by", .string)
            .field("updated_by", .string)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("tasks").delete()
    }
}