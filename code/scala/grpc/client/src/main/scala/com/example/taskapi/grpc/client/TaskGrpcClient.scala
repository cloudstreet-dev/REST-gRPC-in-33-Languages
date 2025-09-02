package com.example.taskapi.grpc.client

import com.google.protobuf.timestamp.Timestamp
import io.grpc.{ManagedChannel, ManagedChannelBuilder, StatusRuntimeException}
import tasks.tasks._

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class TaskGrpcClient(host: String, port: Int)(implicit ec: ExecutionContext) {
  
  private val channel: ManagedChannel = ManagedChannelBuilder
    .forAddress(host, port)
    .usePlaintext()
    .build()
  
  private val stub = TaskServiceGrpc.stub(channel)
  private val blockingStub = TaskServiceGrpc.blockingStub(channel)
  
  def shutdown(): Unit = {
    channel.shutdown()
  }
  
  // List tasks with filters
  def listTasks(
    status: Option[TaskStatus] = None,
    assignedTo: Option[String] = None,
    tags: Seq[String] = Seq.empty,
    pageSize: Int = 20,
    pageToken: Option[String] = None,
    sortBy: String = "created_at",
    sortOrder: String = "desc"
  ): Future[ListTasksResponse] = {
    val request = ListTasksRequest(
      status = status,
      assignedTo = assignedTo.getOrElse(""),
      tags = tags,
      pageSize = pageSize,
      pageToken = pageToken.getOrElse(""),
      sortBy = sortBy,
      sortOrder = sortOrder
    )
    
    stub.listTasks(request)
  }
  
  // Get task by ID
  def getTask(id: String): Future[Task] = {
    val request = GetTaskRequest(id = id)
    stub.getTask(request)
  }
  
  // Create a new task
  def createTask(
    title: String,
    description: String = "",
    priority: TaskPriority = TaskPriority.MEDIUM,
    tags: Seq[String] = Seq.empty,
    assignedTo: String = ""
  ): Future[Task] = {
    val request = CreateTaskRequest(
      title = title,
      description = description,
      priority = Some(priority),
      tags = tags,
      assignedTo = assignedTo
    )
    
    stub.createTask(request)
  }
  
  // Update a task
  def updateTask(
    id: String,
    title: Option[String] = None,
    description: Option[String] = None,
    status: Option[TaskStatus] = None,
    priority: Option[TaskPriority] = None,
    tags: Option[Seq[String]] = None,
    assignedTo: Option[String] = None
  ): Future[Task] = {
    val request = UpdateTaskRequest(
      id = id,
      title = title.getOrElse(""),
      description = description.getOrElse(""),
      status = status,
      priority = priority,
      tags = tags.getOrElse(Seq.empty),
      assignedTo = assignedTo.getOrElse("")
    )
    
    stub.updateTask(request)
  }
  
  // Update task status
  def updateTaskStatus(id: String, status: TaskStatus): Future[Task] = {
    val request = UpdateTaskStatusRequest(
      id = id,
      status = status
    )
    
    stub.updateTaskStatus(request)
  }
  
  // Delete a task
  def deleteTask(id: String): Future[DeleteTaskResponse] = {
    val request = DeleteTaskRequest(id = id)
    stub.deleteTask(request)
  }
  
  // Watch tasks (server streaming)
  def watchTasks(taskIds: Seq[String] = Seq.empty): Unit = {
    val request = WatchTasksRequest(taskIds = taskIds)
    
    try {
      val responseIterator = blockingStub.watchTasks(request)
      
      println("\nWatching for task updates...")
      while (responseIterator.hasNext) {
        val update = responseIterator.next()
        val typeStr = update.updateType match {
          case UpdateType.CREATED => "CREATED"
          case UpdateType.UPDATED => "UPDATED"
          case UpdateType.DELETED => "DELETED"
          case _ => "UNKNOWN"
        }
        
        update.task.foreach { task =>
          println(s"[$typeStr] Task ${task.id}: ${task.title}")
        }
      }
      println("Stream completed")
      
    } catch {
      case e: StatusRuntimeException =>
        println(s"RPC failed: ${e.getStatus}")
    }
  }
  
  // Run a comprehensive demo
  def runDemo(): Future[Unit] = {
    println("\n" + "="*50)
    println("Scala gRPC Client Demo")
    println("="*50)
    
    for {
      // List existing tasks
      _ <- {
        println("\n1. Listing existing tasks...")
        listTasks().map { response =>
          println(s"Found ${response.totalCount} tasks:")
          response.tasks.foreach { task =>
            val statusStr = taskStatusToString(task.status)
            println(s"  - [$statusStr] ${task.title} (ID: ${task.id})")
          }
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error listing tasks: ${e.getStatus}")
        }
      }
      
      // Create a new task
      createdTask <- {
        println("\n2. Creating a new task...")
        createTask(
          title = "Scala gRPC Client Demo Task",
          description = "Testing the Scala gRPC client implementation",
          priority = TaskPriority.HIGH,
          tags = Seq("demo", "scala", "grpc"),
          assignedTo = "scala-team"
        ).map { task =>
          println(s"Created task successfully!")
          println(s"  ID: ${task.id}")
          println(s"  Title: ${task.title}")
          println(s"  Status: ${taskStatusToString(task.status)}")
          Some(task)
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error creating task: ${e.getStatus}")
            None
        }
      }
      
      // Get the created task
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n3. Retrieving task ${task.id}...")
          getTask(task.id).map { retrieved =>
            println(s"Retrieved task:")
            println(s"  Title: ${retrieved.title}")
            println(s"  Description: ${retrieved.description}")
            println(s"  Priority: ${taskPriorityToString(retrieved.priority)}")
          }.recover {
            case e: StatusRuntimeException =>
              println(s"Error getting task: ${e.getStatus}")
          }
        case None => Future.successful(())
      }
      
      // Update task status
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n4. Updating task status to IN_PROGRESS...")
          updateTaskStatus(task.id, TaskStatus.IN_PROGRESS).map { updated =>
            println(s"Status updated successfully!")
            println(s"  New status: ${taskStatusToString(updated.status)}")
          }.recover {
            case e: StatusRuntimeException =>
              println(s"Error updating status: ${e.getStatus}")
          }
        case None => Future.successful(())
      }
      
      // Update task details
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n5. Updating task details...")
          updateTask(
            id = task.id,
            description = Some("Updated description from Scala gRPC client"),
            priority = Some(TaskPriority.MEDIUM)
          ).map { updated =>
            println(s"Task updated successfully!")
            println(s"  Description: ${updated.description}")
            println(s"  Priority: ${taskPriorityToString(updated.priority)}")
          }.recover {
            case e: StatusRuntimeException =>
              println(s"Error updating task: ${e.getStatus}")
          }
        case None => Future.successful(())
      }
      
      // Delete the task
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n6. Deleting task ${task.id}...")
          deleteTask(task.id).map { response =>
            if (response.success) {
              println(s"Task deleted successfully!")
            } else {
              println(s"Failed to delete task")
            }
          }.recover {
            case e: StatusRuntimeException =>
              println(s"Error deleting task: ${e.getStatus}")
          }
        case None => Future.successful(())
      }
      
    } yield {
      println("\n" + "="*50)
      println("Demo completed!")
      println("="*50)
    }
  }
  
  private def taskStatusToString(status: TaskStatus): String = status match {
    case TaskStatus.PENDING => "PENDING"
    case TaskStatus.IN_PROGRESS => "IN_PROGRESS"
    case TaskStatus.COMPLETED => "COMPLETED"
    case TaskStatus.CANCELLED => "CANCELLED"
    case _ => "UNKNOWN"
  }
  
  private def taskPriorityToString(priority: TaskPriority): String = priority match {
    case TaskPriority.LOW => "LOW"
    case TaskPriority.MEDIUM => "MEDIUM"
    case TaskPriority.HIGH => "HIGH"
    case TaskPriority.URGENT => "URGENT"
    case _ => "UNKNOWN"
  }
}