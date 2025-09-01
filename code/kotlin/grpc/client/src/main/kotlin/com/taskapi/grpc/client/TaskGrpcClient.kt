package com.taskapi.grpc.client

import com.google.protobuf.util.Timestamps
import io.grpc.ManagedChannel
import io.grpc.ManagedChannelBuilder
import io.grpc.StatusException
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.toList
import java.util.concurrent.TimeUnit

// Note: These imports will be available after proto compilation
// import com.taskapi.grpc.*

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
    
    suspend fun getTask(taskId: String): Task? {
        return try {
            val request = GetTaskRequest.newBuilder()
                .setId(taskId)
                .build()
            
            stub.getTask(request)
        } catch (e: StatusException) {
            if (e.status.code == io.grpc.Status.Code.NOT_FOUND) {
                null
            } else {
                throw e
            }
        }
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
    
    suspend fun updateTask(
        taskId: String,
        title: String? = null,
        description: String? = null,
        status: TaskStatus? = null,
        priority: TaskPriority? = null,
        tags: List<String>? = null,
        assignedTo: String? = null
    ): Task? {
        return try {
            val taskBuilder = Task.newBuilder().setId(taskId)
            val updateMask = mutableListOf<String>()
            
            title?.let {
                taskBuilder.title = it
                updateMask.add("title")
            }
            description?.let {
                taskBuilder.description = it
                updateMask.add("description")
            }
            status?.let {
                taskBuilder.status = it
                updateMask.add("status")
            }
            priority?.let {
                taskBuilder.priority = it
                updateMask.add("priority")
            }
            tags?.let {
                taskBuilder.addAllTags(it)
                updateMask.add("tags")
            }
            assignedTo?.let {
                taskBuilder.assignedTo = it
                updateMask.add("assigned_to")
            }
            
            val request = UpdateTaskRequest.newBuilder()
                .setTask(taskBuilder.build())
                .addAllUpdateMask(updateMask)
                .build()
            
            stub.updateTask(request)
        } catch (e: StatusException) {
            if (e.status.code == io.grpc.Status.Code.NOT_FOUND) {
                null
            } else {
                throw e
            }
        }
    }
    
    suspend fun deleteTask(taskId: String): Boolean {
        return try {
            val request = DeleteTaskRequest.newBuilder()
                .setId(taskId)
                .build()
            
            stub.deleteTask(request)
            true
        } catch (e: StatusException) {
            if (e.status.code == io.grpc.Status.Code.NOT_FOUND) {
                false
            } else {
                throw e
            }
        }
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
    
    fun shutdown() {
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
    }
}

suspend fun main() {
    val client = TaskGrpcClient("localhost", 50051)
    
    try {
        println("Note: Run './gradlew build' first to generate proto classes.")
        
        // Example usage (will work after proto generation):
        
        /*
        // Create a task
        println("Creating a new task...")
        val task = client.createTask(
            title = "Test Kotlin gRPC Client",
            description = "Testing the Kotlin gRPC client implementation",
            priority = TaskPriority.HIGH,
            tags = listOf("test", "kotlin", "grpc"),
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
        val updated = client.updateTask(
            taskId = task.id,
            status = TaskStatus.IN_PROGRESS
        )
        if (updated != null) {
            println("Updated task status: ${updated.status}")
        }
        
        // List all tasks
        println("\nListing all tasks...")
        val tasks = client.listTasks(
            pageSize = 10,
            sortOrder = SortOrder.CREATED_DESC
        )
        tasks.forEach { t ->
            println("[${t.status}] ${t.title} - ${t.id}")
        }
        
        // Watch for task changes
        println("\nWatching for task changes...")
        var eventCount = 0
        client.watchTasks(watchAll = true).collect { event ->
            println("Event: ${event.eventType} - Task: ${event.task.title}")
            eventCount++
            if (eventCount >= 3) {
                return@collect
            }
        }
        
        // Delete the task
        println("\nDeleting task ${task.id}...")
        val deleted = client.deleteTask(task.id)
        println("Task deleted: $deleted")
        */
        
    } catch (e: Exception) {
        println("Error: ${e.message}")
        e.printStackTrace()
    } finally {
        client.shutdown()
    }
}