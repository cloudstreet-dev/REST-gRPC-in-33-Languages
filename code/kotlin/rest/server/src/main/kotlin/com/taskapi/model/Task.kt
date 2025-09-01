package com.taskapi.model

import kotlinx.datetime.Instant
import kotlinx.serialization.Serializable
import java.util.UUID

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
            val now = kotlinx.datetime.Clock.System.now()
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

@Serializable
data class ErrorResponse(
    val error: String,
    val message: String
)

data class QueryParams(
    val pageSize: Int = 20,
    val pageToken: String? = null,
    val status: TaskStatus? = null,
    val assignedTo: String? = null,
    val tags: String? = null,
    val sortOrder: String? = null
)