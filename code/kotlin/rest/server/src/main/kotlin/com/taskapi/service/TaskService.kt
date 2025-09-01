package com.taskapi.service

import com.taskapi.model.*
import kotlinx.datetime.Clock
import java.util.concurrent.ConcurrentHashMap

class TaskService {
    private val tasks = ConcurrentHashMap<String, Task>()

    init {
        initializeSampleData()
    }

    private fun initializeSampleData() {
        val task1 = Task.create(
            title = "Complete project documentation",
            description = "Write comprehensive documentation for the REST API",
            priority = TaskPriority.HIGH,
            tags = listOf("documentation", "api"),
            assignedTo = "dev-team"
        ).copy(status = TaskStatus.IN_PROGRESS)
        
        val task2 = Task.create(
            title = "Review pull requests",
            description = "Review and approve pending pull requests",
            priority = TaskPriority.MEDIUM,
            tags = listOf("review", "code"),
            assignedTo = "senior-dev"
        )
        
        val task3 = Task.create(
            title = "Deploy to production",
            description = "Deploy the latest version to production environment",
            priority = TaskPriority.CRITICAL,
            tags = listOf("deployment", "production"),
            assignedTo = "devops"
        )

        tasks[task1.id] = task1
        tasks[task2.id] = task2
        tasks[task3.id] = task3
    }

    fun listTasks(params: QueryParams): TasksResponse {
        var filteredTasks = tasks.values.filter { task ->
            val statusMatch = params.status?.let { task.status == it } ?: true
            val assignedToMatch = params.assignedTo?.let { task.assignedTo == it } ?: true
            val tagsMatch = params.tags?.let { tagStr ->
                val requiredTags = tagStr.split(",").map { it.trim() }
                requiredTags.all { tag -> task.tags.contains(tag) }
            } ?: true
            
            statusMatch && assignedToMatch && tagsMatch
        }.toList()

        // Sort tasks
        filteredTasks = when (params.sortOrder) {
            "created_desc" -> filteredTasks.sortedByDescending { it.createdAt }
            "updated_desc" -> filteredTasks.sortedByDescending { it.updatedAt }
            "priority_desc" -> filteredTasks.sortedWith(compareByDescending<Task> { it.priority }.thenBy { it.createdAt })
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

    fun getTask(id: String): Task? = tasks[id]

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

    fun updateTaskStatus(id: String, status: TaskStatus): Task? {
        val existingTask = tasks[id] ?: return null
        
        val updatedTask = existingTask.copy(
            status = status,
            updatedAt = Clock.System.now()
        )
        
        tasks[id] = updatedTask
        return updatedTask
    }

    fun deleteTask(id: String): Boolean {
        return tasks.remove(id) != null
    }
}