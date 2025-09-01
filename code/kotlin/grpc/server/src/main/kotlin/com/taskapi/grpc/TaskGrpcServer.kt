package com.taskapi.grpc

import com.google.protobuf.Empty
import com.google.protobuf.Timestamp
import com.google.protobuf.util.Timestamps
import io.grpc.Server
import io.grpc.ServerBuilder
import io.grpc.Status
import io.grpc.StatusException
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.flow
import java.time.Instant
import java.util.*
import java.util.concurrent.ConcurrentHashMap

// Note: These imports will be available after proto compilation
// import com.taskapi.grpc.*

class TaskServiceImpl : TaskServiceGrpcKt.TaskServiceCoroutineImplBase() {
    
    private val tasks = ConcurrentHashMap<String, Task>()
    
    init {
        initializeSampleData()
    }
    
    private fun initializeSampleData() {
        val now = Timestamps.fromMillis(System.currentTimeMillis())
        
        val task1 = Task.newBuilder()
            .setId(UUID.randomUUID().toString())
            .setTitle("Complete project documentation")
            .setDescription("Write comprehensive documentation for the gRPC API")
            .setStatus(TaskStatus.IN_PROGRESS)
            .setPriority(TaskPriority.HIGH)
            .addTags("documentation")
            .addTags("api")
            .setAssignedTo("dev-team")
            .setCreatedAt(now)
            .setUpdatedAt(now)
            .build()
        
        val task2 = Task.newBuilder()
            .setId(UUID.randomUUID().toString())
            .setTitle("Review pull requests")
            .setDescription("Review and approve pending pull requests")
            .setStatus(TaskStatus.PENDING)
            .setPriority(TaskPriority.MEDIUM)
            .addTags("review")
            .addTags("code")
            .setAssignedTo("senior-dev")
            .setCreatedAt(now)
            .setUpdatedAt(now)
            .build()
        
        val task3 = Task.newBuilder()
            .setId(UUID.randomUUID().toString())
            .setTitle("Deploy to production")
            .setDescription("Deploy the latest version to production environment")
            .setStatus(TaskStatus.PENDING)
            .setPriority(TaskPriority.CRITICAL)
            .addTags("deployment")
            .addTags("production")
            .setAssignedTo("devops")
            .setCreatedAt(now)
            .setUpdatedAt(now)
            .build()
        
        tasks[task1.id] = task1
        tasks[task2.id] = task2
        tasks[task3.id] = task3
    }
    
    override suspend fun listTasks(request: ListTasksRequest): Flow<Task> = flow {
        // Filter tasks
        var filteredTasks = tasks.values.filter { task ->
            val statusMatch = request.status == TaskStatus.UNSPECIFIED || task.status == request.status
            val assignedToMatch = request.assignedTo.isEmpty() || task.assignedTo == request.assignedTo
            val tagsMatch = request.tagsList.isEmpty() || 
                request.tagsList.all { tag -> task.tagsList.contains(tag) }
            
            statusMatch && assignedToMatch && tagsMatch
        }
        
        // Sort tasks
        filteredTasks = when (request.sortOrder) {
            SortOrder.CREATED_DESC -> filteredTasks.sortedByDescending { 
                Timestamps.toMillis(it.createdAt) 
            }
            SortOrder.UPDATED_DESC -> filteredTasks.sortedByDescending { 
                Timestamps.toMillis(it.updatedAt) 
            }
            SortOrder.PRIORITY_DESC -> filteredTasks.sortedWith(
                compareByDescending<Task> { it.priority.number }.thenBy { 
                    Timestamps.toMillis(it.createdAt) 
                }
            )
            else -> filteredTasks.sortedBy { Timestamps.toMillis(it.createdAt) }
        }
        
        // Apply pagination
        val pageSize = if (request.pageSize > 0) minOf(request.pageSize, 100) else 20
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
    
    override suspend fun getTask(request: GetTaskRequest): Task {
        val task = tasks[request.id]
            ?: throw StatusException(Status.NOT_FOUND.withDescription("Task with ID ${request.id} not found"))
        return task
    }
    
    override suspend fun createTask(request: CreateTaskRequest): Task {
        val taskRequest = request.task
            ?: throw StatusException(Status.INVALID_ARGUMENT.withDescription("Task data is required"))
        
        // Validate
        if (taskRequest.title.isBlank()) {
            throw StatusException(Status.INVALID_ARGUMENT.withDescription("Title is required"))
        }
        
        if (taskRequest.title.length > 200) {
            throw StatusException(Status.INVALID_ARGUMENT.withDescription("Title must be 200 characters or less"))
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
    
    override suspend fun updateTask(request: UpdateTaskRequest): Task {
        val taskId = request.task?.id ?: ""
        
        if (taskId.isEmpty()) {
            throw StatusException(Status.INVALID_ARGUMENT.withDescription("Task ID is required"))
        }
        
        val existingTask = tasks[taskId]
            ?: throw StatusException(Status.NOT_FOUND.withDescription("Task with ID $taskId not found"))
        
        val taskBuilder = existingTask.toBuilder()
        val now = Timestamps.fromMillis(System.currentTimeMillis())
        
        // Apply updates based on update mask
        request.updateMaskList.forEach { field ->
            when (field) {
                "title" -> taskBuilder.title = request.task?.title ?: ""
                "description" -> taskBuilder.description = request.task?.description ?: ""
                "status" -> taskBuilder.status = request.task?.status ?: TaskStatus.UNSPECIFIED
                "priority" -> taskBuilder.priority = request.task?.priority ?: TaskPriority.UNSPECIFIED
                "tags" -> {
                    taskBuilder.clearTags()
                    request.task?.tagsList?.forEach { tag ->
                        taskBuilder.addTags(tag)
                    }
                }
                "assigned_to" -> taskBuilder.assignedTo = request.task?.assignedTo ?: ""
                "due_date" -> taskBuilder.dueDate = request.task?.dueDate ?: Timestamp.getDefaultInstance()
            }
        }
        
        taskBuilder.updatedAt = now
        val updatedTask = taskBuilder.build()
        
        tasks[taskId] = updatedTask
        return updatedTask
    }
    
    override suspend fun deleteTask(request: DeleteTaskRequest): Empty {
        if (!tasks.containsKey(request.id)) {
            throw StatusException(Status.NOT_FOUND.withDescription("Task with ID ${request.id} not found"))
        }
        
        tasks.remove(request.id)
        return Empty.getDefaultInstance()
    }
    
    override suspend fun watchTasks(requests: Flow<WatchTasksRequest>): Flow<TaskEvent> = flow {
        requests.collect { request ->
            val now = Timestamps.fromMillis(System.currentTimeMillis())
            
            when {
                request.watchAll -> {
                    // Return all tasks as events
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
                    // Return specific tasks
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
                request.assignedTo.isNotEmpty() -> {
                    // Return tasks assigned to specific user
                    tasks.values
                        .filter { it.assignedTo == request.assignedTo }
                        .forEach { task ->
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

suspend fun main() {
    val server = ServerBuilder.forPort(50051)
        .addService(TaskServiceImpl())
        .build()

    println("Kotlin gRPC server starting on port 50051...")
    server.start()

    Runtime.getRuntime().addShutdownHook(Thread {
        println("Shutting down gRPC server...")
        server.shutdown()
        println("Server shut down")
    })

    server.awaitTermination()
}