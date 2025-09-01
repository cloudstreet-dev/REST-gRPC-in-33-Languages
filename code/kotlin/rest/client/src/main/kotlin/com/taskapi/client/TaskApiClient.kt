package com.taskapi.client

import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.engine.cio.*
import io.ktor.client.plugins.*
import io.ktor.client.plugins.contentnegotiation.*
import io.ktor.client.plugins.logging.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import io.ktor.serialization.kotlinx.json.*
import kotlinx.coroutines.runBlocking
import kotlinx.datetime.Instant
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json

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
    val id: String,
    val title: String,
    val description: String? = null,
    val status: TaskStatus,
    val priority: TaskPriority,
    val tags: List<String> = emptyList(),
    val assignedTo: String? = null,
    val dueDate: Instant? = null,
    val createdAt: Instant,
    val updatedAt: Instant,
    val createdBy: String? = null,
    val updatedBy: String? = null
)

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
data class UpdateTaskRequest(
    val title: String? = null,
    val description: String? = null,
    val status: TaskStatus? = null,
    val priority: TaskPriority? = null,
    val tags: List<String>? = null,
    val assignedTo: String? = null,
    val dueDate: Instant? = null
)

@Serializable
data class UpdateStatusRequest(
    val status: TaskStatus
)

@Serializable
data class TasksResponse(
    val items: List<Task>,
    val pageSize: Int,
    val nextPageToken: String? = null,
    val totalCount: Int
)

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

    suspend fun getTask(taskId: String): Task? {
        return try {
            val response: HttpResponse = client.get("$baseUrl/api/v1/tasks/$taskId")
            if (response.status == HttpStatusCode.NotFound) {
                null
            } else {
                response.body()
            }
        } catch (e: ClientRequestException) {
            if (e.response.status == HttpStatusCode.NotFound) {
                null
            } else {
                throw e
            }
        }
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

    suspend fun updateTask(
        taskId: String,
        updates: UpdateTaskRequest
    ): Task? {
        return try {
            val response: HttpResponse = client.put("$baseUrl/api/v1/tasks/$taskId") {
                setBody(updates)
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

    suspend fun updateTaskStatus(taskId: String, status: TaskStatus): Task? {
        return try {
            val request = UpdateStatusRequest(status)
            val response: HttpResponse = client.patch("$baseUrl/api/v1/tasks/$taskId/status") {
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

    suspend fun deleteTask(taskId: String): Boolean {
        return try {
            val response: HttpResponse = client.delete("$baseUrl/api/v1/tasks/$taskId")
            response.status == HttpStatusCode.NoContent
        } catch (e: ClientRequestException) {
            if (e.response.status == HttpStatusCode.NotFound) {
                false
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

        // Get the task
        println("\nRetrieving task ${task.id}...")
        val retrieved = client.getTask(task.id)
        if (retrieved != null) {
            println("Retrieved task: ${retrieved.title} - Status: ${retrieved.status}")
        }

        // Update task status
        println("\nUpdating task status to IN_PROGRESS...")
        val updated = client.updateTaskStatus(task.id, TaskStatus.IN_PROGRESS)
        if (updated != null) {
            println("Updated task status: ${updated.status}")
        }

        // List all tasks
        println("\nListing all tasks...")
        val tasks = client.listTasks(pageSize = 10, sortOrder = "created_desc")
        tasks.items.forEach { t ->
            println("[${t.status}] ${t.title} - ${t.id}")
        }
        println("Total tasks: ${tasks.totalCount}")

        // Update task details
        println("\nUpdating task details...")
        val updateRequest = UpdateTaskRequest(
            description = "Updated: Testing the Kotlin REST client implementation with Ktor",
            priority = TaskPriority.MEDIUM,
            tags = listOf("test", "kotlin", "rest", "ktor")
        )
        val updatedTask = client.updateTask(task.id, updateRequest)
        if (updatedTask != null) {
            println("Updated task: ${updatedTask.description}")
        }

        // Delete the task
        println("\nDeleting task ${task.id}...")
        val deleted = client.deleteTask(task.id)
        println("Task deleted: $deleted")

    } catch (e: Exception) {
        println("Error: ${e.message}")
        e.printStackTrace()
    } finally {
        client.close()
    }
}