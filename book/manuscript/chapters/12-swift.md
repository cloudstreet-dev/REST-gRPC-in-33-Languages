# Chapter 12: Swift

Swift, Apple's modern programming language, brings type safety, performance, and expressive syntax to API development. Originally designed for iOS and macOS development, Swift has evolved into a powerful server-side language with frameworks like Vapor enabling robust REST API development.

## Language Overview

Swift combines the best of modern language design with powerful features:

- **Type Safety**: Strong static typing with type inference
- **Memory Management**: Automatic Reference Counting (ARC)
- **Concurrency**: Native async/await support with structured concurrency
- **Performance**: Compiled to native code with optimizations
- **Interoperability**: Seamless C and Objective-C integration
- **Pattern Matching**: Powerful switch statements and optional handling

### Key Swift Features for API Development

Swift's modern features make it excellent for API development:

```swift
// Optional handling with nil coalescing
let title = request.title ?? "Default Title"

// Pattern matching with enums
enum TaskStatus: String, CaseIterable {
    case pending = "pending"
    case inProgress = "in_progress"
    case completed = "completed"
}

// Async/await for concurrent operations
func fetchTasks() async throws -> [Task] {
    return try await database.query(Task.self).all()
}
```

## Development Environment Setup

### Installing Swift

**macOS (recommended):**
```bash
# Swift comes with Xcode
xcode-select --install

# Or install via Homebrew
brew install swift
```

**Linux:**
```bash
# Ubuntu/Debian
wget -qO- https://swift.org/keys/all-keys.asc | sudo apt-key add -
echo "deb https://archive.swiftlang.xyz/ubuntu/ focal main" | sudo tee /etc/apt/sources.list.d/swift.list
sudo apt update && sudo apt install swiftlang
```

**Docker:**
```bash
docker run -it swift:5.9
```

### Project Structure with Swift Package Manager

Swift Package Manager (SPM) is Swift's native dependency management and build system:

```
swift-project/
├── Package.swift              # Package manifest
├── Sources/
│   └── MyApp/
│       └── main.swift        # Application entry point
├── Tests/
│   └── MyAppTests/
│       └── MyAppTests.swift
└── .gitignore
```

## REST API Implementation with Vapor

Vapor is Swift's most popular web framework, providing a comprehensive toolkit for building REST APIs.

### Server Implementation

Our Vapor server uses Fluent ORM for database operations and structured async/await patterns:

```swift
// Package.swift
let package = Package(
    name: "task-rest-server",
    platforms: [.macOS(.v13)],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "4.77.0"),
        .package(url: "https://github.com/vapor/fluent.git", from: "4.8.0"),
        .package(url: "https://github.com/vapor/fluent-sqlite-driver.git", from: "4.0.0"),
    ],
    targets: [
        .executableTarget(name: "App", dependencies: [
            .product(name: "Fluent", package: "fluent"),
            .product(name: "FluentSQLiteDriver", package: "fluent-sqlite-driver"),
            .product(name: "Vapor", package: "vapor")
        ])
    ]
)
```

The Task model uses Fluent's property wrappers for database mapping:

```swift
final class Task: Model, Content {
    static let schema = "tasks"
    
    @ID(key: .id)
    var id: UUID?
    
    @Field(key: "title")
    var title: String
    
    @Field(key: "description")
    var description: String?
    
    @Enum(key: "status")
    var status: TaskStatus
    
    @Enum(key: "priority")
    var priority: TaskPriority
    
    @Field(key: "tags")
    var tags: [String]
    
    @Field(key: "assigned_to")
    var assignedTo: String?
    
    @Field(key: "created_by")
    var createdBy: String
    
    @Timestamp(key: "created_at", on: .create)
    var createdAt: Date?
    
    @Timestamp(key: "updated_at", on: .update)
    var updatedAt: Date?
    
    @Timestamp(key: "due_date", on: .none)
    var dueDate: Date?
}
```

The TaskController implements all REST endpoints:

```swift
struct TaskController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tasks = routes.grouped("tasks")
        
        tasks.get(use: list)
        tasks.post(use: create)
        tasks.group(":taskID") { task in
            task.get(use: get)
            task.put(use: update)
            task.delete(use: delete)
        }
    }
    
    func create(req: Request) async throws -> Response {
        let createRequest = try req.content.decode(CreateTaskRequest.self)
        
        // Validation
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
            createdBy: createRequest.createdBy ?? "system"
        )
        
        try await task.save(on: req.db)
        
        let response = Response(status: .created)
        try response.content.encode(task)
        return response
    }
}
```

### Client Implementation

The Swift REST client uses URLSession with modern async/await patterns:

```swift
class TaskRestClient {
    private let baseURL: URL
    private let session: URLSession
    private let encoder = JSONEncoder()
    private let decoder = JSONDecoder()
    
    init(baseURL: URL) {
        self.baseURL = baseURL
        
        let config = URLSessionConfiguration.default
        config.timeoutIntervalForRequest = 30
        config.timeoutIntervalForResource = 60
        self.session = URLSession(configuration: config)
        
        // Configure JSON encoding/decoding
        encoder.keyEncodingStrategy = .convertToSnakeCase
        decoder.keyDecodingStrategy = .convertFromSnakeCase
        
        let formatter = ISO8601DateFormatter()
        encoder.dateEncodingStrategy = .custom { date, encoder in
            var container = encoder.singleValueContainer()
            try container.encode(formatter.string(from: date))
        }
        decoder.dateDecodingStrategy = .custom { decoder in
            let container = try decoder.singleValueContainer()
            let string = try container.decode(String.self)
            guard let date = formatter.date(from: string) else {
                throw DecodingError.dataCorruptedError(in: container, debugDescription: "Invalid date format")
            }
            return date
        }
    }
    
    func createTask(title: String, description: String? = nil, 
                   priority: TaskPriority = .medium, tags: [String] = [],
                   assignedTo: String? = nil, createdBy: String? = nil) async throws -> Task {
        let url = baseURL.appendingPathComponent("tasks")
        
        let createRequest = CreateTaskRequest(
            title: title,
            description: description,
            priority: priority,
            tags: tags.isEmpty ? nil : tags,
            assignedTo: assignedTo,
            createdBy: createdBy
        )
        
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try encoder.encode(createRequest)
        
        let (data, response) = try await session.data(for: request)
        
        guard let httpResponse = response as? HTTPURLResponse else {
            throw TaskClientError.invalidResponse
        }
        
        guard httpResponse.statusCode == 201 else {
            throw TaskClientError.serverError(httpResponse.statusCode)
        }
        
        return try decoder.decode(Task.self, from: data)
    }
}
```

## gRPC Implementation with SwiftNIO

Swift's gRPC implementation uses SwiftNIO for high-performance networking and supports modern Swift concurrency.

### Server Implementation

The gRPC server implements the TaskService with proper async patterns:

```swift
extension TaskServiceProvider: Task_TaskServiceAsyncProvider {
    func listTasks(
        request: Task_ListTasksRequest,
        context: GRPCAsyncServerCallContext
    ) -> GRPCAsyncResponseStream<Task_Task> {
        return GRPCAsyncResponseStream { writer in
            await queue.sync {
                // Filter and sort tasks
                var filteredTasks = Array(tasks.values)
                
                if request.status != .unspecified {
                    filteredTasks = filteredTasks.filter { $0.status == request.status }
                }
                
                if !request.assignedTo.isEmpty {
                    filteredTasks = filteredTasks.filter { $0.assignedTo == request.assignedTo }
                }
                
                // Apply pagination and send results
                let pageSize = request.pageSize > 0 ? min(Int(request.pageSize), 100) : 20
                let startIndex = request.pageToken.isEmpty ? 0 : (Int(request.pageToken) ?? 0)
                let endIndex = min(startIndex + pageSize, filteredTasks.count)
                
                if startIndex < filteredTasks.count {
                    let paginatedTasks = Array(filteredTasks[startIndex..<endIndex])
                    
                    for task in paginatedTasks {
                        try await writer.send(task)
                    }
                }
            }
        }
    }
    
    func createTask(
        request: Task_CreateTaskRequest,
        context: GRPCAsyncServerCallContext
    ) async throws -> Task_Task {
        return try await queue.sync {
            var task = request.task
            
            // Validation
            guard !task.title.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
                throw GRPCStatus(code: .invalidArgument, message: "Title is required")
            }
            
            // Generate ID and set timestamps
            task.id = UUID().uuidString
            let now = Google_Protobuf_Timestamp.with {
                $0.seconds = Int64(Date().timeIntervalSince1970)
            }
            task.createdAt = now
            task.updatedAt = now
            
            tasks[task.id] = task
            return task
        }
    }
}
```

### Client Implementation

The gRPC client uses AsyncThrowingStream for handling streaming responses:

```swift
func watchTasks(watchAll: Bool = false,
                taskIds: [String] = [],
                assignedTo: String = "") -> AsyncThrowingStream<Task_TaskEvent, Error> {
    
    return AsyncThrowingStream { continuation in
        Task {
            do {
                let requestStream = AsyncStream<Task_WatchTasksRequest> { streamContinuation in
                    let request = Task_WatchTasksRequest.with {
                        $0.watchAll = watchAll
                        $0.taskIds = taskIds
                        $0.assignedTo = assignedTo
                    }
                    streamContinuation.yield(request)
                    streamContinuation.finish()
                }
                
                let responseStream = client.watchTasks(requestStream: requestStream.map { $0 })
                
                for try await event in responseStream {
                    continuation.yield(event)
                }
                
                continuation.finish()
            } catch {
                continuation.finish(throwing: error)
            }
        }
    }
}
```

## Swift-Specific Features

### Error Handling

Swift uses typed error handling with `do-catch` blocks and `Result` types:

```swift
enum TaskClientError: LocalizedError {
    case invalidURL
    case invalidResponse
    case serverError(Int)
    case decodingError(Error)
    
    var errorDescription: String? {
        switch self {
        case .invalidURL:
            return "Invalid URL"
        case .invalidResponse:
            return "Invalid response from server"
        case .serverError(let code):
            return "Server error with code: \(code)"
        case .decodingError(let error):
            return "Failed to decode response: \(error.localizedDescription)"
        }
    }
}

// Usage with Result type
func fetchTask(id: String) -> Result<Task, TaskClientError> {
    // Implementation
}
```

### Concurrency and Async/Await

Swift's structured concurrency provides safe async operations:

```swift
// Task groups for concurrent operations
func fetchMultipleTasks(ids: [String]) async throws -> [Task] {
    return try await withThrowingTaskGroup(of: Task.self) { group in
        for id in ids {
            group.addTask {
                try await self.getTask(id: id)
            }
        }
        
        var tasks: [Task] = []
        for try await task in group {
            tasks.append(task)
        }
        return tasks
    }
}

// Actors for thread-safe state management
actor TaskCache {
    private var cache: [String: Task] = [:]
    
    func get(_ id: String) -> Task? {
        return cache[id]
    }
    
    func set(_ task: Task) {
        cache[task.id] = task
    }
}
```

### Property Wrappers

Swift's property wrappers enable clean, declarative code:

```swift
// Custom validation wrapper
@propertyWrapper
struct Validated<Value> {
    private var value: Value
    private let validator: (Value) -> Bool
    
    init(wrappedValue: Value, _ validator: @escaping (Value) -> Bool) {
        self.value = wrappedValue
        self.validator = validator
    }
    
    var wrappedValue: Value {
        get { value }
        set {
            if validator(newValue) {
                value = newValue
            }
        }
    }
}

// Usage in models
struct TaskRequest {
    @Validated({ !$0.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty })
    var title: String = ""
    
    @Validated({ $0.count <= 1000 })
    var description: String = ""
}
```

## Testing

Swift provides excellent testing capabilities with XCTest:

```swift
import XCTest
@testable import App

final class TaskControllerTests: XCTestCase {
    var app: Application!
    
    override func setUp() async throws {
        app = Application(.testing)
        try configure(app)
        try await app.autoMigrate()
    }
    
    override func tearDown() async throws {
        app.shutdown()
    }
    
    func testCreateTask() async throws {
        let createRequest = CreateTaskRequest(
            title: "Test Task",
            description: "Test Description",
            priority: .high
        )
        
        try await app.test(.POST, "tasks") { req in
            try req.content.encode(createRequest)
        } afterResponse: { res in
            XCTAssertEqual(res.status, .created)
            let task = try res.content.decode(Task.self)
            XCTAssertEqual(task.title, "Test Task")
            XCTAssertEqual(task.priority, .high)
        }
    }
}
```

## Performance Considerations

### Memory Management

Swift uses Automatic Reference Counting (ARC) with additional tools for optimization:

```swift
// Weak references to avoid retain cycles
class TaskManager {
    weak var delegate: TaskManagerDelegate?
    private var tasks: [Task] = []
    
    // Use unowned for non-optional references that should never be nil
    private unowned let database: Database
    
    init(database: Database) {
        self.database = database
    }
}

// Value types (structs) for better performance
struct TaskSummary {
    let id: String
    let title: String
    let status: TaskStatus
    let priority: TaskPriority
}
```

### Vapor Performance Tips

```swift
// Use connection pooling
app.databases.use(.sqlite(.file("tasks.db")), as: .sqlite)
app.databases.middleware.use(DatabaseReportMiddleware())

// Implement caching
struct TaskCacheMiddleware: AsyncMiddleware {
    func respond(to request: Request, chainingTo next: AsyncResponder) async throws -> Response {
        // Check cache first
        if let cached = try await request.cache.get("task_\(id)", as: Task.self) {
            return try await cached.encodeResponse(for: request)
        }
        
        let response = try await next.respond(to: request)
        
        // Cache successful responses
        if response.status == .ok {
            try await request.cache.set("task_\(id)", to: task, expiresIn: .seconds(300))
        }
        
        return response
    }
}
```

## Production Deployment

### Docker Deployment

```dockerfile
# Dockerfile
FROM swift:5.9-slim

WORKDIR /app

# Copy package files
COPY Package.swift Package.resolved ./

# Resolve dependencies
RUN swift package resolve

# Copy source code
COPY Sources ./Sources

# Build for production
RUN swift build --configuration release --static-swift-stdlib

# Create runtime image
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/.build/release/App /app/

EXPOSE 8080

CMD ["./App", "serve", "--env", "production", "--hostname", "0.0.0.0", "--port", "8080"]
```

### Production Configuration

```swift
// configure.swift
public func configure(_ app: Application) throws {
    // Production database configuration
    if app.environment == .production {
        try app.databases.use(.postgres(
            hostname: Environment.get("DATABASE_HOST") ?? "localhost",
            port: Environment.get("DATABASE_PORT").flatMap(Int.init(_:)) ?? 5432,
            username: Environment.get("DATABASE_USERNAME") ?? "vapor",
            password: Environment.get("DATABASE_PASSWORD") ?? "",
            database: Environment.get("DATABASE_NAME") ?? "tasks"
        ), as: .psql)
    } else {
        app.databases.use(.sqlite(.file("tasks.db")), as: .sqlite)
    }
    
    // Middleware configuration
    app.middleware.use(ErrorMiddleware.default(environment: app.environment))
    app.middleware.use(CORSMiddleware(configuration: .init(
        allowedOrigin: .originBased,
        allowedMethods: [.GET, .POST, .PUT, .OPTIONS, .DELETE, .PATCH],
        allowedHeaders: [.accept, .authorization, .contentType, .origin, .xRequestedWith]
    )))
    
    // Register routes
    try routes(app)
}
```

## Swift Ecosystem and Tools

### Package Management

Swift Package Manager integrates deeply with the ecosystem:

```swift
// Package.swift with version pinning
let package = Package(
    name: "TaskAPI",
    platforms: [
        .macOS(.v13),
        .iOS(.v16)
    ],
    products: [
        .library(name: "TaskAPI", targets: ["TaskAPI"]),
        .executable(name: "TaskServer", targets: ["TaskServer"])
    ],
    dependencies: [
        .package(url: "https://github.com/vapor/vapor.git", from: "4.77.0"),
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
        .package(url: "https://github.com/swift-server/swift-service-lifecycle.git", from: "2.0.0")
    ]
)
```

### Logging and Monitoring

```swift
import Logging

// Structured logging
let logger = Logger(label: "task-api")

func createTask(_ request: CreateTaskRequest) async throws -> Task {
    logger.info("Creating task", metadata: [
        "title": .string(request.title),
        "priority": .string(request.priority.rawValue)
    ])
    
    do {
        let task = try await database.create(task)
        logger.info("Task created successfully", metadata: ["task_id": .string(task.id)])
        return task
    } catch {
        logger.error("Failed to create task", metadata: ["error": .string(error.localizedDescription)])
        throw error
    }
}
```

### Development Tools

Swift provides excellent tooling for development:

```bash
# Format code
swift-format --in-place Sources/

# Lint code
swiftlint

# Generate documentation
swift package generate-documentation

# Run tests with coverage
swift test --enable-code-coverage

# Profile performance
swift run --configuration release TaskServer &
instruments -t "Time Profiler" -D profile.trace $(pgrep TaskServer)
```

## Best Practices

### Code Organization

```swift
// Organize code with clear separation of concerns
protocol TaskRepository {
    func create(_ task: Task) async throws -> Task
    func findById(_ id: String) async throws -> Task?
    func findAll(filters: TaskFilters) async throws -> [Task]
}

struct DatabaseTaskRepository: TaskRepository {
    private let database: Database
    
    func create(_ task: Task) async throws -> Task {
        try await task.save(on: database)
        return task
    }
}

// Use dependency injection
struct TaskService {
    private let repository: TaskRepository
    private let logger: Logger
    
    init(repository: TaskRepository, logger: Logger) {
        self.repository = repository
        self.logger = logger
    }
}
```

### Error Handling

```swift
// Use Swift's Result type for composable error handling
extension TaskService {
    func createTaskSafely(_ request: CreateTaskRequest) async -> Result<Task, TaskError> {
        do {
            let task = try await createTask(request)
            return .success(task)
        } catch let error as ValidationError {
            return .failure(.validationFailed(error.message))
        } catch {
            logger.error("Unexpected error creating task", metadata: ["error": .string(error.localizedDescription)])
            return .failure(.internalError)
        }
    }
}
```

### Configuration Management

```swift
// Environment-based configuration
struct AppConfig {
    let databaseURL: String
    let port: Int
    let logLevel: Logger.Level
    
    init() {
        databaseURL = Environment.get("DATABASE_URL") ?? "sqlite:///tasks.db"
        port = Environment.get("PORT").flatMap(Int.init) ?? 8080
        logLevel = Environment.get("LOG_LEVEL").flatMap(Logger.Level.init) ?? .info
    }
}
```

## Conclusion

Swift provides a powerful, modern foundation for API development with its combination of type safety, performance, and expressive syntax. The Vapor framework offers comprehensive tools for REST API development, while SwiftNIO provides high-performance gRPC capabilities.

Key advantages of Swift for API development:

- **Type Safety**: Compile-time error detection reduces runtime issues
- **Performance**: Native compilation and memory efficiency
- **Modern Concurrency**: Structured async/await prevents common concurrency bugs
- **Package Manager**: Integrated dependency management and build system
- **Ecosystem**: Growing server-side Swift community and libraries

Swift's emphasis on safety and performance makes it an excellent choice for production APIs, especially when you need the benefits of both strong typing and modern language features. The active development of server-side Swift frameworks continues to expand its capabilities for backend development.

Next, we'll explore **Chapter 13: TypeScript**, examining how JavaScript's typed superset approaches API development with its rich type system and extensive ecosystem.