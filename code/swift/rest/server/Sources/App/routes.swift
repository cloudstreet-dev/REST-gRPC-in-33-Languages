import Fluent
import Vapor

func routes(_ app: Application) throws {
    // Health check endpoint
    app.get("health") { req async in
        return ["status": "ok"]
    }
    
    // Initialize sample data
    app.get("init") { req async throws -> String in
        // Check if we already have tasks
        let existingCount = try await Task.query(on: req.db).count()
        if existingCount > 0 {
            return "Sample data already exists"
        }
        
        let sampleTasks = [
            Task(
                title: "Complete project documentation",
                description: "Write comprehensive documentation for the REST API",
                status: .inProgress,
                priority: .high,
                tags: ["documentation", "api"],
                assignedTo: "dev-team"
            ),
            Task(
                title: "Review pull requests",
                description: "Review and approve pending pull requests",
                status: .pending,
                priority: .medium,
                tags: ["review", "code"],
                assignedTo: "senior-dev"
            ),
            Task(
                title: "Deploy to production",
                description: "Deploy the latest version to production environment",
                status: .pending,
                priority: .critical,
                tags: ["deployment", "production"],
                assignedTo: "devops"
            )
        ]
        
        for task in sampleTasks {
            try await task.save(on: req.db)
        }
        
        return "Sample data initialized"
    }

    // Register task routes
    try app.register(collection: TaskController())
}