# Chapter 11: Kotlin - Modern JVM Language with Ktor and gRPC

## Introduction

Kotlin, introduced by JetBrains in 2011 and reaching stable release in 2016, has rapidly become one of the most beloved programming languages. Officially endorsed by Google as a first-class language for Android development and running on the JVM with full Java interoperability, Kotlin combines the best of object-oriented and functional programming. With its concise syntax, null safety, and coroutines for asynchronous programming, Kotlin offers a modern approach to API development using frameworks like Ktor.

In this chapter, we'll implement our Task Management API using Ktor for REST services and native gRPC support, showcasing Kotlin's strengths in building maintainable, expressive APIs.

## Why Kotlin for APIs?

Kotlin provides compelling advantages for API development:

1. **Java Interoperability**: 100% compatible with Java, leveraging existing JVM ecosystem
2. **Null Safety**: Eliminates null pointer exceptions at compile time
3. **Coroutines**: Built-in asynchronous programming with structured concurrency
4. **Concise Syntax**: Reduces boilerplate code while maintaining readability
5. **Type Inference**: Smart compiler reduces explicit type declarations
6. **Extension Functions**: Extend existing classes without inheritance
7. **Data Classes**: Built-in support for immutable data structures
8. **Modern Language Features**: Sealed classes, when expressions, and more

## Setting Up the Development Environment

### Installing Kotlin

Kotlin comes bundled with IntelliJ IDEA and can be installed separately:

```bash
# Using SDKMAN!
curl -s "https://get.sdkman.io" | bash
sdk install kotlin

# Using Homebrew (macOS)
brew install kotlin

# Verify installation
kotlin -version
```

### Installing Gradle

Gradle is our preferred build tool for Kotlin projects:

```bash
# Using SDKMAN!
sdk install gradle

# Using Homebrew (macOS)
brew install gradle

# Verify installation
gradle --version
```

### Project Structure

```
code/kotlin/
├── rest/
│   ├── server/
│   │   ├── build.gradle.kts
│   │   ├── gradle.properties
│   │   └── src/main/kotlin/com/taskapi/
│   │       ├── Application.kt
│   │       ├── model/Task.kt
│   │       ├── service/TaskService.kt
│   │       ├── controller/TaskController.kt
│   │       └── config/
│   │           ├── HTTP.kt
│   │           └── Serialization.kt
│   └── client/
│       ├── build.gradle.kts
│       └── src/main/kotlin/com/taskapi/client/
│           └── TaskApiClient.kt
└── grpc/
    ├── server/
    │   ├── build.gradle.kts
    │   └── src/main/kotlin/com/taskapi/grpc/
    │       └── TaskGrpcServer.kt
    └── client/
        ├── build.gradle.kts
        └── src/main/kotlin/com/taskapi/grpc/client/
            └── TaskGrpcClient.kt
```

## Implementing the REST API with Ktor

### Gradle Configuration

Ktor provides a lightweight, flexible framework for building web applications:

```kotlin
val ktor_version: String by project
val kotlin_version: String by project

plugins {
    kotlin("jvm") version "1.9.22"
    id("io.ktor.plugin") version "2.3.7"
    id("org.jetbrains.kotlin.plugin.serialization") version "1.9.22"
}

dependencies {
    implementation("io.ktor:ktor-server-core-jvm")
    implementation("io.ktor:ktor-server-netty-jvm")
    implementation("io.ktor:ktor-server-content-negotiation-jvm")
    implementation("io.ktor:ktor-serialization-kotlinx-json-jvm")
    implementation("io.ktor:ktor-server-cors-jvm")
    implementation("io.ktor:ktor-server-swagger-jvm")
    implementation("io.ktor:ktor-server-openapi")
    implementation("org.jetbrains.kotlinx:kotlinx-datetime:0.5.0")
}
```

### Domain Models with Kotlin Data Classes

Kotlin's data classes provide excellent support for immutable data structures:

```kotlin
@Serializable
enum class TaskStatus {
    PENDING, IN_PROGRESS, COMPLETED, CANCELLED, ARCHIVED
}

@Serializable
enum class TaskPriority {
    LOW, MEDIUM, HIGH, CRITICAL
}

@Serializable
data class Task(
    val id: String = UUID.randomUUID().toString(),
    val title: String,
    val description: String? = null,
    val status: TaskStatus = TaskStatus.PENDING,
    val priority: TaskPriority = TaskPriority.MEDIUM,
    val tags: List<String> = emptyList(),
    val assignedTo: String? = null,
    val dueDate: Instant? = null,
    val createdAt: Instant,
    val updatedAt: Instant,
    val createdBy: String? = null,
    val updatedBy: String? = null
) {
    companion object {
        fun create(
            title: String,
            description: String? = null,
            priority: TaskPriority = TaskPriority.MEDIUM,
            tags: List<String> = emptyList(),
            assignedTo: String? = null,
            dueDate: Instant? = null
        ): Task {
            val now = Clock.System.now()
            return Task(
                title = title,
                description = description,
                priority = priority,
                tags = tags,
                assignedTo = assignedTo,
                dueDate = dueDate,
                createdAt = now,
                updatedAt = now
            )
        }
    }
}

@Serializable
data class CreateTaskRequest(
    val title: String,
    val description: String? = null,
    val priority: TaskPriority = TaskPriority.MEDIUM,
    val tags: List<String> = emptyList(),
    val assignedTo: String? = null,
    val dueDate: Instant? = null
)

@Serializable
data class TasksResponse(
    val items: List<Task>,
    val pageSize: Int,
    val nextPageToken: String? = null,
    val totalCount: Int
)
```

### Service Layer

Business logic with thread-safe collections:

```kotlin
class TaskService {
    private val tasks = ConcurrentHashMap<String, Task>()

    init {
        initializeSampleData()
    }

    fun listTasks(params: QueryParams): TasksResponse {
        var filteredTasks = tasks.values.filter { task ->
            val statusMatch = params.status?.let { task.status == it } ?: true
            val assignedToMatch = params.assignedTo?.let { 
                task.assignedTo == it 
            } ?: true
            val tagsMatch = params.tags?.let { tagStr ->
                val requiredTags = tagStr.split(",").map { it.trim() }
                requiredTags.all { tag -> task.tags.contains(tag) }
            } ?: true
            
            statusMatch && assignedToMatch && tagsMatch
        }.toList()

        // Sort tasks with when expression
        filteredTasks = when (params.sortOrder) {
            "created_desc" -> filteredTasks.sortedByDescending { it.createdAt }
            "updated_desc" -> filteredTasks.sortedByDescending { it.updatedAt }
            "priority_desc" -> filteredTasks.sortedWith(
                compareByDescending<Task> { it.priority }
                    .thenBy { it.createdAt }
            )
            else -> filteredTasks.sortedBy { it.createdAt }
        }

        // Apply pagination
        val pageSize = minOf(params.pageSize, 100)
        val startIndex = params.pageToken?.toIntOrNull() ?: 0
        val endIndex = minOf(startIndex + pageSize, filteredTasks.size)
        
        val paginatedTasks = if (startIndex < filteredTasks.size) {
            filteredTasks.subList(startIndex, endIndex)
        } else {
            emptyList()
        }
        
        val nextPageToken = if (endIndex < filteredTasks.size) {
            endIndex.toString()
        } else {
            null
        }

        return TasksResponse(
            items = paginatedTasks,
            pageSize = pageSize,
            nextPageToken = nextPageToken,
            totalCount = filteredTasks.size
        )
    }

    fun createTask(request: CreateTaskRequest): Task {
        val task = Task.create(
            title = request.title,
            description = request.description,
            priority = request.priority,
            tags = request.tags,
            assignedTo = request.assignedTo,
            dueDate = request.dueDate
        )
        
        tasks[task.id] = task
        return task
    }

    fun updateTask(id: String, request: UpdateTaskRequest): Task? {
        val existingTask = tasks[id] ?: return null
        
        // Using copy method for immutable updates
        val updatedTask = existingTask.copy(
            title = request.title ?: existingTask.title,
            description = request.description ?: existingTask.description,
            status = request.status ?: existingTask.status,
            priority = request.priority ?: existingTask.priority,
            tags = request.tags ?: existingTask.tags,
            assignedTo = request.assignedTo ?: existingTask.assignedTo,
            dueDate = request.dueDate ?: existingTask.dueDate,
            updatedAt = Clock.System.now()
        )
        
        tasks[id] = updatedTask
        return updatedTask
    }
}
```

### Ktor Routing

Type-safe routing with extension functions:

```kotlin
fun Application.configureRouting() {
    val taskService = TaskService()

    routing {
        route("/api/v1") {
            taskRoutes(taskService)
        }
    }
}

fun Route.taskRoutes(taskService: TaskService) {
    route("/tasks") {
        get {
            try {
                val params = QueryParams(
                    pageSize = call.request.queryParameters["pageSize"]
                        ?.toIntOrNull() ?: 20,
                    pageToken = call.request.queryParameters["pageToken"],
                    status = call.request.queryParameters["status"]?.let { 
                        try { 
                            TaskStatus.valueOf(it.uppercase()) 
                        } catch (e: Exception) { 
                            null 
                        }
                    },
                    assignedTo = call.request.queryParameters["assignedTo"],
                    tags = call.request.queryParameters["tags"],
                    sortOrder = call.request.queryParameters["sortOrder"]
                )
                
                val response = taskService.listTasks(params)
                call.respond(HttpStatusCode.OK, response)
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.InternalServerError,
                    ErrorResponse("internal_error", "Error listing tasks")
                )
            }
        }

        post {
            try {
                val request = call.receive<CreateTaskRequest>()
                
                // Kotlin's when expression for validation
                when {
                    request.title.isBlank() -> {
                        call.respond(
                            HttpStatusCode.BadRequest,
                            ErrorResponse("validation_error", "Title is required")
                        )
                        return@post
                    }
                    request.title.length > 200 -> {
                        call.respond(
                            HttpStatusCode.BadRequest,
                            ErrorResponse("validation_error", 
                                "Title must be 200 characters or less")
                        )
                        return@post
                    }
                    else -> {
                        val task = taskService.createTask(request)
                        call.respond(HttpStatusCode.Created, task)
                    }
                }
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Invalid request data")
                )
            }
        }

        put("/{id}") {
            val id = call.parameters["id"]
            if (id.isNullOrBlank()) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Task ID is required")
                )
                return@put
            }

            try {
                val request = call.receive<UpdateTaskRequest>()
                
                val task = taskService.updateTask(id, request)
                if (task != null) {
                    call.respond(HttpStatusCode.OK, task)
                } else {
                    call.respond(
                        HttpStatusCode.NotFound,
                        ErrorResponse("not_found", "Task with ID $id not found")
                    )
                }
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Invalid request data")
                )
            }
        }
    }
}
```

### Application Configuration

Modular configuration with extension functions:

```kotlin
fun Application.module() {
    configureSerialization()
    configureHTTP()
    configureRouting()
}

fun Application.configureSerialization() {
    install(ContentNegotiation) {
        json(Json {
            prettyPrint = true
            isLenient = true
            ignoreUnknownKeys = true
        })
    }
}

fun Application.configureHTTP() {
    install(CORS) {
        allowMethod(HttpMethod.Options)
        allowMethod(HttpMethod.Put)
        allowMethod(HttpMethod.Delete)
        allowMethod(HttpMethod.Patch)
        allowHeader(HttpHeaders.Authorization)
        allowHeader(HttpHeaders.ContentType)
        anyHost()
    }
    
    routing {
        swaggerUI(path = "swagger-ui", swaggerFile = "openapi/documentation.yaml")
        openAPI(path = "openapi", swaggerFile = "openapi/documentation.yaml")
    }
}
```

### Running the REST Server

```bash
cd code/kotlin/rest/server
./gradlew run

# Or build and run
./gradlew build
java -jar build/libs/task-rest-server-1.0.0.jar

# Access Swagger UI
open http://localhost:8080/swagger-ui
```

## Implementing the REST Client

### Ktor HTTP Client

Ktor's HTTP client with coroutines support:

```kotlin
class TaskApiClient(private val baseUrl: String) {
    private val client = HttpClient(CIO) {
        install(ContentNegotiation) {
            json(Json {
                prettyPrint = true
                isLenient = true
                ignoreUnknownKeys = true
            })
        }
        install(Logging) {
            level = LogLevel.INFO
        }
        install(HttpTimeout) {
            requestTimeoutMillis = 30000
            connectTimeoutMillis = 10000
            socketTimeoutMillis = 30000
        }
        defaultRequest {
            header(HttpHeaders.ContentType, ContentType.Application.Json)
        }
    }

    suspend fun listTasks(
        pageSize: Int = 20,
        pageToken: String? = null,
        status: TaskStatus? = null,
        assignedTo: String? = null,
        tags: List<String>? = null,
        sortOrder: String? = null
    ): TasksResponse {
        val response: HttpResponse = client.get("$baseUrl/api/v1/tasks") {
            parameter("pageSize", pageSize)
            pageToken?.let { parameter("pageToken", it) }
            status?.let { parameter("status", it.name) }
            assignedTo?.let { parameter("assignedTo", it) }
            tags?.let { parameter("tags", it.joinToString(",")) }
            sortOrder?.let { parameter("sortOrder", it) }
        }
        return response.body()
    }

    suspend fun createTask(
        title: String,
        description: String? = null,
        priority: TaskPriority = TaskPriority.MEDIUM,
        tags: List<String> = emptyList(),
        assignedTo: String? = null
    ): Task {
        val request = CreateTaskRequest(
            title = title,
            description = description,
            priority = priority,
            tags = tags,
            assignedTo = assignedTo
        )

        val response: HttpResponse = client.post("$baseUrl/api/v1/tasks") {
            setBody(request)
        }
        return response.body()
    }

    suspend fun updateTaskStatus(taskId: String, status: TaskStatus): Task? {
        return try {
            val request = UpdateStatusRequest(status)
            val response: HttpResponse = client.patch(
                "$baseUrl/api/v1/tasks/$taskId/status"
            ) {
                setBody(request)
            }
            response.body()
        } catch (e: ClientRequestException) {
            if (e.response.status == HttpStatusCode.NotFound) {
                null
            } else {
                throw e
            }
        }
    }

    fun close() {
        client.close()
    }
}

suspend fun main() {
    val client = TaskApiClient("http://localhost:8080")

    try {
        // Create a task
        println("Creating a new task...")
        val task = client.createTask(
            title = "Test Kotlin REST Client",
            description = "Testing the Kotlin REST client implementation",
            priority = TaskPriority.HIGH,
            tags = listOf("test", "kotlin", "rest"),
            assignedTo = "dev-team"
        )
        println("Created task: ${task.id} - ${task.title}")

        // Update status
        println("\nUpdating task status to IN_PROGRESS...")
        val updated = client.updateTaskStatus(task.id, TaskStatus.IN_PROGRESS)
        updated?.let {
            println("Updated task status: ${it.status}")
        }

        // List tasks
        println("\nListing all tasks...")
        val tasks = client.listTasks(pageSize = 10, sortOrder = "created_desc")
        tasks.items.forEach { t ->
            println("[${t.status}] ${t.title} - ${t.id}")
        }

    } finally {
        client.close()
    }
}
```

## Implementing gRPC Services

### Gradle Configuration for gRPC

Kotlin gRPC with coroutines support:

```kotlin
val grpcVersion = "1.60.0"
val grpcKotlinVersion = "1.4.1"

dependencies {
    implementation("io.grpc:grpc-kotlin-stub:$grpcKotlinVersion")
    implementation("io.grpc:grpc-protobuf:$grpcVersion")
    implementation("io.grpc:grpc-netty-shaded:$grpcVersion")
    implementation("com.google.protobuf:protobuf-kotlin:$protobufVersion")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
}

protobuf {
    protoc {
        artifact = "com.google.protobuf:protoc:$protobufVersion"
    }
    plugins {
        id("grpc") {
            artifact = "io.grpc:protoc-gen-grpc-java:$grpcVersion"
        }
        id("grpckt") {
            artifact = "io.grpc:protoc-gen-grpc-kotlin:$grpcKotlinVersion:jdk8@jar"
        }
    }
    generateProtoTasks {
        all().forEach {
            it.plugins {
                id("grpc")
                id("grpckt")
            }
            it.builtins {
                id("kotlin")
            }
        }
    }
}
```

### gRPC Service Implementation

Coroutine-based gRPC service:

```kotlin
class TaskServiceImpl : TaskServiceGrpcKt.TaskServiceCoroutineImplBase() {
    
    private val tasks = ConcurrentHashMap<String, Task>()
    
    override suspend fun listTasks(request: ListTasksRequest): Flow<Task> = flow {
        // Filter tasks
        var filteredTasks = tasks.values.filter { task ->
            val statusMatch = request.status == TaskStatus.UNSPECIFIED || 
                task.status == request.status
            val assignedToMatch = request.assignedTo.isEmpty() || 
                task.assignedTo == request.assignedTo
            val tagsMatch = request.tagsList.isEmpty() || 
                request.tagsList.all { tag -> task.tagsList.contains(tag) }
            
            statusMatch && assignedToMatch && tagsMatch
        }
        
        // Sort with when expression
        filteredTasks = when (request.sortOrder) {
            SortOrder.CREATED_DESC -> filteredTasks.sortedByDescending { 
                Timestamps.toMillis(it.createdAt) 
            }
            SortOrder.UPDATED_DESC -> filteredTasks.sortedByDescending { 
                Timestamps.toMillis(it.updatedAt) 
            }
            SortOrder.PRIORITY_DESC -> filteredTasks.sortedWith(
                compareByDescending<Task> { it.priority.number }
                    .thenBy { Timestamps.toMillis(it.createdAt) }
            )
            else -> filteredTasks.sortedBy { 
                Timestamps.toMillis(it.createdAt) 
            }
        }
        
        // Apply pagination
        val pageSize = if (request.pageSize > 0) {
            minOf(request.pageSize, 100) 
        } else { 
            20 
        }
        val startIndex = if (request.pageToken.isNotEmpty()) {
            try {
                request.pageToken.toInt()
            } catch (e: NumberFormatException) {
                0
            }
        } else {
            0
        }
        
        val endIndex = minOf(startIndex + pageSize, filteredTasks.size)
        val paginatedTasks = if (startIndex < filteredTasks.size) {
            filteredTasks.subList(startIndex, endIndex)
        } else {
            emptyList()
        }
        
        // Emit tasks
        paginatedTasks.forEach { task ->
            emit(task)
        }
    }
    
    override suspend fun createTask(request: CreateTaskRequest): Task {
        val taskRequest = request.task
            ?: throw StatusException(
                Status.INVALID_ARGUMENT.withDescription("Task data is required")
            )
        
        // Validation with when
        when {
            taskRequest.title.isBlank() -> {
                throw StatusException(
                    Status.INVALID_ARGUMENT.withDescription("Title is required")
                )
            }
            taskRequest.title.length > 200 -> {
                throw StatusException(
                    Status.INVALID_ARGUMENT.withDescription(
                        "Title must be 200 characters or less"
                    )
                )
            }
        }
        
        val now = Timestamps.fromMillis(System.currentTimeMillis())
        val task = taskRequest.toBuilder()
            .setId(UUID.randomUUID().toString())
            .setCreatedAt(now)
            .setUpdatedAt(now)
            .apply {
                if (createdBy.isEmpty()) {
                    createdBy = "system"
                }
            }
            .build()
        
        tasks[task.id] = task
        return task
    }

    override suspend fun watchTasks(
        requests: Flow<WatchTasksRequest>
    ): Flow<TaskEvent> = flow {
        requests.collect { request ->
            val now = Timestamps.fromMillis(System.currentTimeMillis())
            
            when {
                request.watchAll -> {
                    tasks.values.forEach { task ->
                        val event = TaskEvent.newBuilder()
                            .setEventType(TaskEvent.EventType.UPDATED)
                            .setTask(task)
                            .setTimestamp(now)
                            .build()
                        emit(event)
                    }
                }
                request.taskIdsList.isNotEmpty() -> {
                    request.taskIdsList.forEach { taskId ->
                        tasks[taskId]?.let { task ->
                            val event = TaskEvent.newBuilder()
                                .setEventType(TaskEvent.EventType.UPDATED)
                                .setTask(task)
                                .setTimestamp(now)
                                .build()
                            emit(event)
                        }
                    }
                }
            }
        }
    }
}
```

### gRPC Client

Coroutine-based gRPC client:

```kotlin
class TaskGrpcClient(host: String, port: Int) {
    private val channel: ManagedChannel = ManagedChannelBuilder
        .forAddress(host, port)
        .usePlaintext()
        .build()
    
    private val stub = TaskServiceGrpcKt.TaskServiceCoroutineStub(channel)
    
    suspend fun listTasks(
        pageSize: Int = 20,
        pageToken: String = "",
        status: TaskStatus = TaskStatus.UNSPECIFIED,
        assignedTo: String = "",
        tags: List<String> = emptyList(),
        sortOrder: SortOrder = SortOrder.UNSPECIFIED
    ): List<Task> {
        val request = ListTasksRequest.newBuilder()
            .setPageSize(pageSize)
            .setPageToken(pageToken)
            .setStatus(status)
            .setAssignedTo(assignedTo)
            .addAllTags(tags)
            .setSortOrder(sortOrder)
            .build()
        
        return stub.listTasks(request).toList()
    }
    
    suspend fun createTask(
        title: String,
        description: String = "",
        priority: TaskPriority = TaskPriority.MEDIUM,
        tags: List<String> = emptyList(),
        assignedTo: String = ""
    ): Task {
        val task = Task.newBuilder()
            .setTitle(title)
            .setDescription(description)
            .setPriority(priority)
            .addAllTags(tags)
            .setAssignedTo(assignedTo)
            .build()
        
        val request = CreateTaskRequest.newBuilder()
            .setTask(task)
            .build()
        
        return stub.createTask(request)
    }
    
    suspend fun watchTasks(
        watchAll: Boolean = false,
        taskIds: List<String> = emptyList(),
        assignedTo: String = ""
    ): Flow<TaskEvent> {
        val requestFlow = flow {
            emit(
                WatchTasksRequest.newBuilder()
                    .setWatchAll(watchAll)
                    .addAllTaskIds(taskIds)
                    .setAssignedTo(assignedTo)
                    .build()
            )
        }
        
        return stub.watchTasks(requestFlow)
    }
}
```

## Advanced Features

### Extension Functions

Kotlin's extension functions enhance existing types:

```kotlin
// Extend HttpResponse for convenient error handling
suspend inline fun <reified T> HttpResponse.bodyOrNull(): T? {
    return if (status.isSuccess()) {
        body<T>()
    } else {
        null
    }
}

// Extend Task for business logic
fun Task.isOverdue(): Boolean {
    return dueDate?.let { due ->
        Clock.System.now() > due
    } ?: false
}

fun Task.calculateDaysRemaining(): Long? {
    return dueDate?.let { due ->
        val now = Clock.System.now()
        (due - now).inWholeDays
    }
}

// Extension for validation
fun CreateTaskRequest.validate(): ValidationResult {
    return when {
        title.isBlank() -> ValidationResult.Error("Title is required")
        title.length > 200 -> ValidationResult.Error("Title too long")
        tags.size > 10 -> ValidationResult.Error("Too many tags")
        else -> ValidationResult.Valid
    }
}

sealed class ValidationResult {
    object Valid : ValidationResult()
    data class Error(val message: String) : ValidationResult()
}
```

### Sealed Classes

Type-safe error handling:

```kotlin
sealed class TaskResult<out T> {
    data class Success<T>(val data: T) : TaskResult<T>()
    data class Error(val message: String, val cause: Throwable? = null) : TaskResult<Nothing>()
    object Loading : TaskResult<Nothing>()
}

class TaskRepository {
    suspend fun getTask(id: String): TaskResult<Task> {
        return try {
            val task = findTaskById(id)
            if (task != null) {
                TaskResult.Success(task)
            } else {
                TaskResult.Error("Task not found")
            }
        } catch (e: Exception) {
            TaskResult.Error("Database error", e)
        }
    }
}

// Usage with when expression
suspend fun handleGetTask(id: String) {
    when (val result = repository.getTask(id)) {
        is TaskResult.Success -> {
            println("Task: ${result.data.title}")
        }
        is TaskResult.Error -> {
            println("Error: ${result.message}")
            result.cause?.printStackTrace()
        }
        is TaskResult.Loading -> {
            println("Loading...")
        }
    }
}
```

### Coroutines and Flow

Structured concurrency with Kotlin coroutines:

```kotlin
class TaskService {
    private val scope = CoroutineScope(Dispatchers.IO + SupervisorJob())
    
    suspend fun processTasksBatch(tasks: List<Task>): List<TaskResult<Task>> {
        return tasks.map { task ->
            async(scope.coroutineContext) {
                processTask(task)
            }
        }.awaitAll()
    }
    
    fun watchTaskUpdates(): Flow<TaskEvent> = flow {
        while (currentCoroutineContext().isActive) {
            // Simulate task updates
            delay(1000)
            emit(TaskEvent("Task updated"))
        }
    }.flowOn(Dispatchers.IO)
    
    // Combine multiple flows
    fun getTaskMetrics(): Flow<TaskMetrics> {
        return combine(
            taskCountFlow(),
            completedTasksFlow(),
            overdueTasks()
        ) { total, completed, overdue ->
            TaskMetrics(
                totalTasks = total,
                completedTasks = completed,
                overdueTasks = overdue
            )
        }
    }
}

// Usage with collectors
suspend fun monitorTasks() {
    taskService.watchTaskUpdates()
        .catch { e -> println("Error: ${e.message}") }
        .collect { event ->
            println("Received: $event")
        }
}
```

### Database Integration

Using Exposed ORM with Kotlin:

```kotlin
object Tasks : Table() {
    val id = varchar("id", 36)
    val title = varchar("title", 200)
    val description = text("description").nullable()
    val status = enumerationByName("status", 50, TaskStatus::class)
    val priority = enumerationByName("priority", 50, TaskPriority::class)
    val assignedTo = varchar("assigned_to", 100).nullable()
    val createdAt = timestamp("created_at")
    val updatedAt = timestamp("updated_at")
    
    override val primaryKey = PrimaryKey(id)
}

class DatabaseTaskRepository : TaskRepository {
    override suspend fun findAll(): List<Task> = newSuspendedTransaction {
        Tasks.selectAll().map { row ->
            Task(
                id = row[Tasks.id],
                title = row[Tasks.title],
                description = row[Tasks.description],
                status = row[Tasks.status],
                priority = row[Tasks.priority],
                assignedTo = row[Tasks.assignedTo],
                createdAt = row[Tasks.createdAt].toKotlinInstant(),
                updatedAt = row[Tasks.updatedAt].toKotlinInstant()
            )
        }
    }
    
    override suspend fun create(task: Task): Task = newSuspendedTransaction {
        Tasks.insert {
            it[id] = task.id
            it[title] = task.title
            it[description] = task.description
            it[status] = task.status
            it[priority] = task.priority
            it[assignedTo] = task.assignedTo
            it[createdAt] = task.createdAt.toJavaInstant()
            it[updatedAt] = task.updatedAt.toJavaInstant()
        }
        task
    }
}
```

## Testing

### Unit Testing with Kotlin

```kotlin
class TaskServiceTest {
    private val taskService = TaskService()
    
    @Test
    fun `should create task with valid data`() {
        // Given
        val request = CreateTaskRequest(
            title = "Test Task",
            description = "Test Description",
            priority = TaskPriority.HIGH
        )
        
        // When
        val task = taskService.createTask(request)
        
        // Then
        assertEquals("Test Task", task.title)
        assertEquals(TaskStatus.PENDING, task.status)
        assertNotNull(task.id)
    }
    
    @Test
    fun `should filter tasks by status`() {
        // Given
        taskService.createTask(CreateTaskRequest(title = "Task 1"))
        val task2 = taskService.createTask(CreateTaskRequest(title = "Task 2"))
        taskService.updateTaskStatus(task2.id, TaskStatus.IN_PROGRESS)
        
        // When
        val params = QueryParams(status = TaskStatus.IN_PROGRESS)
        val result = taskService.listTasks(params)
        
        // Then
        assertEquals(1, result.items.size)
        assertEquals("Task 2", result.items.first().title)
    }
}
```

### Integration Testing with Ktor

```kotlin
class TaskControllerTest {
    private val testApp = testApplication {
        module {
            configureSerialization()
            configureRouting()
        }
    }
    
    @Test
    fun `should create and retrieve task`() = testApp.test {
        // Create task
        val createResponse = client.post("/api/v1/tasks") {
            contentType(ContentType.Application.Json)
            setBody(Json.encodeToString(CreateTaskRequest(
                title = "Integration Test Task",
                priority = TaskPriority.MEDIUM
            )))
        }
        
        assertEquals(HttpStatusCode.Created, createResponse.status)
        val task = Json.decodeFromString<Task>(createResponse.bodyAsText())
        
        // Retrieve task
        val getResponse = client.get("/api/v1/tasks/${task.id}")
        assertEquals(HttpStatusCode.OK, getResponse.status)
        
        val retrievedTask = Json.decodeFromString<Task>(getResponse.bodyAsText())
        assertEquals(task.id, retrievedTask.id)
        assertEquals("Integration Test Task", retrievedTask.title)
    }
    
    @Test
    fun `should handle pagination`() = testApp.test {
        // Create multiple tasks
        repeat(25) { i ->
            client.post("/api/v1/tasks") {
                contentType(ContentType.Application.Json)
                setBody(Json.encodeToString(CreateTaskRequest(
                    title = "Task $i"
                )))
            }
        }
        
        // Test pagination
        val response = client.get("/api/v1/tasks?pageSize=10")
        assertEquals(HttpStatusCode.OK, response.status)
        
        val result = Json.decodeFromString<TasksResponse>(response.bodyAsText())
        assertEquals(10, result.items.size)
        assertNotNull(result.nextPageToken)
    }
}
```

## Performance and Best Practices

### Coroutine Best Practices

```kotlin
class OptimizedTaskService {
    private val scope = CoroutineScope(
        Dispatchers.IO + SupervisorJob() + 
        CoroutineName("TaskService")
    )
    
    // Use structured concurrency
    suspend fun processTasks(tasks: List<Task>): List<Task> {
        return withContext(Dispatchers.IO) {
            tasks.map { task ->
                async {
                    processTask(task)
                }
            }.awaitAll()
        }
    }
    
    // Proper resource management
    fun close() {
        scope.cancel()
    }
}

// Use Flow for streaming data
fun watchTaskChanges(): Flow<Task> = channelFlow {
    val listener = object : TaskChangeListener {
        override fun onTaskChanged(task: Task) {
            trySend(task)
        }
    }
    
    registerListener(listener)
    
    awaitClose {
        unregisterListener(listener)
    }
}.flowOn(Dispatchers.IO)
```

### Kotlin DSL for Configuration

```kotlin
// Create a DSL for task creation
class TaskBuilder {
    var title: String = ""
    var description: String? = null
    var priority: TaskPriority = TaskPriority.MEDIUM
    var tags: MutableList<String> = mutableListOf()
    var assignedTo: String? = null
    
    fun tag(tag: String) {
        tags.add(tag)
    }
    
    fun tags(vararg tags: String) {
        this.tags.addAll(tags)
    }
}

fun task(block: TaskBuilder.() -> Unit): Task {
    val builder = TaskBuilder()
    builder.block()
    return Task.create(
        title = builder.title,
        description = builder.description,
        priority = builder.priority,
        tags = builder.tags,
        assignedTo = builder.assignedTo
    )
}

// Usage
val task = task {
    title = "Implement new feature"
    description = "Add user authentication"
    priority = TaskPriority.HIGH
    tags("auth", "security", "backend")
    assignedTo = "dev-team"
}
```

## Deployment

### Building for Production

```bash
# Build application
./gradlew build

# Create fat JAR
./gradlew shadowJar

# Run application
java -jar build/libs/task-api-kotlin-1.0.0-all.jar
```

### Dockerfile

```dockerfile
# Multi-stage build
FROM gradle:8.5-jdk17 AS builder
WORKDIR /app
COPY build.gradle.kts gradle.properties ./
COPY src ./src
RUN gradle build --no-daemon

# Runtime stage
FROM openjdk:17-jdk-slim
WORKDIR /app
COPY --from=builder /app/build/libs/*.jar app.jar

EXPOSE 8080 50051

HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

ENTRYPOINT ["java", "-jar", "app.jar"]
```

## Conclusion

Kotlin brings modern language features and excellent Java interoperability to API development. The combination of null safety, coroutines, and expressive syntax with frameworks like Ktor makes it an excellent choice for building maintainable, performant APIs. The seamless integration with the JVM ecosystem while providing modern language constructs positions Kotlin as a compelling option for both new projects and Java migrations.

Key takeaways:
- 100% Java interoperability leverages existing ecosystem
- Null safety eliminates entire class of runtime errors
- Coroutines provide structured asynchronous programming
- Concise syntax reduces boilerplate without sacrificing clarity
- Extension functions enable clean API design
- Sealed classes provide type-safe error handling
- Data classes simplify immutable data modeling
- Strong IDE support enhances developer productivity

Kotlin successfully bridges the gap between Java's ecosystem and modern language design, making it an excellent choice for API development in environments where JVM compatibility is important but modern language features are desired.