package com.example.taskapi.grpc.server

import com.google.protobuf.timestamp.Timestamp
import io.grpc.Status
import tasks.tasks._

import java.time.Instant
import java.util.UUID
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

class TaskServiceImpl(implicit ec: ExecutionContext) extends TaskServiceGrpc.TaskService {
  
  // In-memory storage using a thread-safe TrieMap
  private val tasks = TrieMap[String, Task]()
  
  // Initialize with sample data
  def initialize(): Unit = {
    val now = Instant.now()
    
    val task1 = Task(
      id = "1",
      title = "Implement Scala gRPC server",
      description = "Build a gRPC server using ScalaPB",
      status = TaskStatus.IN_PROGRESS,
      priority = TaskPriority.HIGH,
      tags = Seq("scala", "grpc", "protobuf"),
      assignedTo = "scala-team",
      createdAt = Some(Timestamp(now.minusSeconds(3600).getEpochSecond)),
      updatedAt = Some(Timestamp(now.minusSeconds(1800).getEpochSecond))
    )
    
    val task2 = Task(
      id = "2",
      title = "Add streaming support",
      description = "Implement server-side streaming for real-time updates",
      status = TaskStatus.PENDING,
      priority = TaskPriority.MEDIUM,
      tags = Seq("grpc", "streaming"),
      assignedTo = "backend-team",
      createdAt = Some(Timestamp(now.minusSeconds(7200).getEpochSecond)),
      updatedAt = Some(Timestamp(now.minusSeconds(7200).getEpochSecond))
    )
    
    val task3 = Task(
      id = "3",
      title = "Write integration tests",
      description = "Add comprehensive test coverage for gRPC service",
      status = TaskStatus.PENDING,
      priority = TaskPriority.HIGH,
      tags = Seq("testing", "quality"),
      assignedTo = "qa-team",
      createdAt = Some(Timestamp(now.minusSeconds(1800).getEpochSecond)),
      updatedAt = Some(Timestamp(now.minusSeconds(900).getEpochSecond))
    )
    
    tasks.put(task1.id, task1)
    tasks.put(task2.id, task2)
    tasks.put(task3.id, task3)
  }
  
  override def listTasks(request: ListTasksRequest): Future[ListTasksResponse] = Future {
    val allTasks = tasks.values.toSeq
    
    // Apply filters
    val filtered = allTasks.filter { task =>
      val statusMatch = request.status.forall(_ == task.status)
      val assignedMatch = request.assignedTo.forall(_ == task.assignedTo)
      val tagsMatch = request.tags.isEmpty || request.tags.forall(task.tags.contains)
      
      statusMatch && assignedMatch && tagsMatch
    }
    
    // Sort tasks (simplified - by ID for now)
    val sorted = request.sortBy match {
      case "title" => 
        if (request.sortOrder == "asc") filtered.sortBy(_.title)
        else filtered.sortBy(_.title).reverse
      case "created_at" | _ =>
        if (request.sortOrder == "asc") filtered.sortBy(_.id)
        else filtered.sortBy(_.id).reverse
    }
    
    // Apply pagination
    val pageSize = if (request.pageSize > 0 && request.pageSize <= 100) request.pageSize else 20
    val offset = request.pageToken.map(_.toIntOption.getOrElse(0)).getOrElse(0)
    val paginated = sorted.slice(offset, offset + pageSize)
    
    val nextToken = if (offset + pageSize < sorted.length) {
      Some((offset + pageSize).toString)
    } else {
      None
    }
    
    ListTasksResponse(
      tasks = paginated,
      totalCount = filtered.length,
      nextPageToken = nextToken.getOrElse("")
    )
  }
  
  override def getTask(request: GetTaskRequest): Future[Task] = Future {
    tasks.get(request.id) match {
      case Some(task) => task
      case None => 
        throw Status.NOT_FOUND
          .withDescription(s"Task with ID ${request.id} not found")
          .asException()
    }
  }
  
  override def createTask(request: CreateTaskRequest): Future[Task] = Future {
    if (request.title.trim.isEmpty) {
      throw Status.INVALID_ARGUMENT
        .withDescription("Title is required and cannot be empty")
        .asException()
    }
    
    val now = Instant.now()
    val task = Task(
      id = UUID.randomUUID().toString,
      title = request.title,
      description = request.description,
      status = TaskStatus.PENDING,
      priority = request.priority.getOrElse(TaskPriority.MEDIUM),
      tags = request.tags,
      assignedTo = request.assignedTo,
      createdAt = Some(Timestamp(now.getEpochSecond)),
      updatedAt = Some(Timestamp(now.getEpochSecond))
    )
    
    tasks.put(task.id, task)
    task
  }
  
  override def updateTask(request: UpdateTaskRequest): Future[Task] = Future {
    tasks.get(request.id) match {
      case Some(existing) =>
        val now = Instant.now()
        val updated = existing.copy(
          title = if (request.title.nonEmpty) request.title else existing.title,
          description = if (request.description.nonEmpty) request.description else existing.description,
          status = request.status.getOrElse(existing.status),
          priority = request.priority.getOrElse(existing.priority),
          tags = if (request.tags.nonEmpty) request.tags else existing.tags,
          assignedTo = if (request.assignedTo.nonEmpty) request.assignedTo else existing.assignedTo,
          updatedAt = Some(Timestamp(now.getEpochSecond))
        )
        
        tasks.put(request.id, updated)
        updated
        
      case None =>
        throw Status.NOT_FOUND
          .withDescription(s"Task with ID ${request.id} not found")
          .asException()
    }
  }
  
  override def deleteTask(request: DeleteTaskRequest): Future[DeleteTaskResponse] = Future {
    val removed = tasks.remove(request.id)
    if (removed.isDefined) {
      DeleteTaskResponse(success = true)
    } else {
      throw Status.NOT_FOUND
        .withDescription(s"Task with ID ${request.id} not found")
        .asException()
    }
  }
  
  override def updateTaskStatus(request: UpdateTaskStatusRequest): Future[Task] = Future {
    tasks.get(request.id) match {
      case Some(existing) =>
        val now = Instant.now()
        val updated = existing.copy(
          status = request.status,
          updatedAt = Some(Timestamp(now.getEpochSecond))
        )
        
        tasks.put(request.id, updated)
        updated
        
      case None =>
        throw Status.NOT_FOUND
          .withDescription(s"Task with ID ${request.id} not found")
          .asException()
    }
  }
  
  // Server streaming for watching task updates (simplified implementation)
  override def watchTasks(request: WatchTasksRequest, responseObserver: io.grpc.stub.StreamObserver[TaskUpdate]): Unit = {
    // This is a simplified implementation
    // In production, you would implement proper change detection
    
    val initialTasks = tasks.values.filter { task =>
      request.taskIds.isEmpty || request.taskIds.contains(task.id)
    }
    
    // Send initial state
    initialTasks.foreach { task =>
      responseObserver.onNext(TaskUpdate(
        updateType = UpdateType.CREATED,
        task = Some(task)
      ))
    }
    
    // In a real implementation, you would:
    // 1. Set up change listeners on the task store
    // 2. Send updates as tasks change
    // 3. Handle client disconnection properly
    
    // For demo purposes, we'll just complete after sending initial state
    Thread.sleep(1000) // Simulate some activity
    responseObserver.onCompleted()
  }
}