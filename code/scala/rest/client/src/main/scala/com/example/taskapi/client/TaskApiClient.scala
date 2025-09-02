package com.example.taskapi.client

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.example.taskapi.models._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class TaskApiClient(baseUrl: String)(implicit system: ActorSystem[_]) {
  implicit val ec: ExecutionContext = system.executionContext
  
  private def makeRequest(request: HttpRequest): Future[HttpResponse] = {
    Http().singleRequest(request)
  }
  
  // List tasks with filters
  def listTasks(
    status: Option[String] = None,
    assignedTo: Option[String] = None,
    tags: Option[List[String]] = None,
    pageSize: Int = 20,
    pageToken: Option[String] = None,
    sortBy: String = "created_at",
    sortOrder: String = "desc"
  ): Future[Either[String, ListTasksResponse]] = {
    val params = List(
      status.map(s => s"status=$s"),
      assignedTo.map(a => s"assigned_to=$a"),
      tags.map(_.mkString(",")).map(t => s"tags=$t"),
      Some(s"page_size=$pageSize"),
      pageToken.map(t => s"page_token=$t"),
      Some(s"sort_by=$sortBy"),
      Some(s"sort_order=$sortOrder")
    ).flatten.mkString("&")
    
    val uri = Uri(s"$baseUrl/api/tasks?$params")
    val request = HttpRequest(method = HttpMethods.GET, uri = uri)
    
    makeRequest(request).flatMap { response =>
      if (response.status.isSuccess()) {
        Unmarshal(response.entity).to[ListTasksResponse].map(Right(_))
      } else {
        Unmarshal(response.entity).to[String].map(body => 
          Left(s"Error ${response.status.intValue()}: $body")
        )
      }
    }.recover {
      case ex => Left(s"Request failed: ${ex.getMessage}")
    }
  }
  
  // Get task by ID
  def getTask(id: String): Future[Either[String, Task]] = {
    val uri = Uri(s"$baseUrl/api/tasks/$id")
    val request = HttpRequest(method = HttpMethods.GET, uri = uri)
    
    makeRequest(request).flatMap { response =>
      if (response.status.isSuccess()) {
        Unmarshal(response.entity).to[Task].map(Right(_))
      } else if (response.status == StatusCodes.NotFound) {
        Future.successful(Left(s"Task with ID $id not found"))
      } else {
        Unmarshal(response.entity).to[String].map(body => 
          Left(s"Error ${response.status.intValue()}: $body")
        )
      }
    }.recover {
      case ex => Left(s"Request failed: ${ex.getMessage}")
    }
  }
  
  // Create a new task
  def createTask(request: CreateTaskRequest): Future[Either[String, Task]] = {
    val uri = Uri(s"$baseUrl/api/tasks")
    
    Marshal(request).to[RequestEntity].flatMap { entity =>
      val httpRequest = HttpRequest(
        method = HttpMethods.POST,
        uri = uri,
        entity = entity
      )
      
      makeRequest(httpRequest).flatMap { response =>
        if (response.status == StatusCodes.Created) {
          Unmarshal(response.entity).to[Task].map(Right(_))
        } else {
          Unmarshal(response.entity).to[String].map(body => 
            Left(s"Error ${response.status.intValue()}: $body")
          )
        }
      }
    }.recover {
      case ex => Left(s"Request failed: ${ex.getMessage}")
    }
  }
  
  // Update a task
  def updateTask(id: String, request: UpdateTaskRequest): Future[Either[String, Task]] = {
    val uri = Uri(s"$baseUrl/api/tasks/$id")
    
    Marshal(request).to[RequestEntity].flatMap { entity =>
      val httpRequest = HttpRequest(
        method = HttpMethods.PUT,
        uri = uri,
        entity = entity
      )
      
      makeRequest(httpRequest).flatMap { response =>
        if (response.status.isSuccess()) {
          Unmarshal(response.entity).to[Task].map(Right(_))
        } else if (response.status == StatusCodes.NotFound) {
          Future.successful(Left(s"Task with ID $id not found"))
        } else {
          Unmarshal(response.entity).to[String].map(body => 
            Left(s"Error ${response.status.intValue()}: $body")
          )
        }
      }
    }.recover {
      case ex => Left(s"Request failed: ${ex.getMessage}")
    }
  }
  
  // Update task status
  def updateTaskStatus(id: String, status: String): Future[Either[String, Task]] = {
    val uri = Uri(s"$baseUrl/api/tasks/$id/status?status=$status")
    val request = HttpRequest(method = HttpMethods.PATCH, uri = uri)
    
    makeRequest(request).flatMap { response =>
      if (response.status.isSuccess()) {
        Unmarshal(response.entity).to[Task].map(Right(_))
      } else if (response.status == StatusCodes.NotFound) {
        Future.successful(Left(s"Task with ID $id not found"))
      } else {
        Unmarshal(response.entity).to[String].map(body => 
          Left(s"Error ${response.status.intValue()}: $body")
        )
      }
    }.recover {
      case ex => Left(s"Request failed: ${ex.getMessage}")
    }
  }
  
  // Delete a task
  def deleteTask(id: String): Future[Either[String, Unit]] = {
    val uri = Uri(s"$baseUrl/api/tasks/$id")
    val request = HttpRequest(method = HttpMethods.DELETE, uri = uri)
    
    makeRequest(request).flatMap { response =>
      response.discardEntityBytes()
      if (response.status == StatusCodes.NoContent) {
        Future.successful(Right(()))
      } else if (response.status == StatusCodes.NotFound) {
        Future.successful(Left(s"Task with ID $id not found"))
      } else {
        Future.successful(Left(s"Error ${response.status.intValue()}"))
      }
    }.recover {
      case ex => Left(s"Request failed: ${ex.getMessage}")
    }
  }
  
  // Run a comprehensive demo
  def runDemo(): Future[Unit] = {
    println("\n" + "="*50)
    println("Scala REST Client Demo")
    println("="*50)
    
    for {
      // List existing tasks
      _ <- {
        println("\n1. Listing existing tasks...")
        listTasks().map {
          case Right(response) =>
            println(s"Found ${response.totalCount} tasks:")
            response.tasks.foreach { task =>
              println(s"  - [${TaskStatus.toString(task.status)}] ${task.title} (ID: ${task.id})")
            }
          case Left(error) =>
            println(s"Error listing tasks: $error")
        }
      }
      
      // Create a new task
      createdTask <- {
        println("\n2. Creating a new task...")
        val request = CreateTaskRequest(
          title = "Scala REST Client Demo Task",
          description = Some("Testing the Scala REST client"),
          priority = Some(TaskPriority.High),
          tags = Some(List("demo", "scala", "rest")),
          assignedTo = Some("scala-team")
        )
        createTask(request).map {
          case Right(task) =>
            println(s"Created task successfully!")
            println(s"  ID: ${task.id}")
            println(s"  Title: ${task.title}")
            println(s"  Status: ${TaskStatus.toString(task.status)}")
            Some(task)
          case Left(error) =>
            println(s"Error creating task: $error")
            None
        }
      }
      
      // Get the created task
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n3. Retrieving task ${task.id}...")
          getTask(task.id).map {
            case Right(retrieved) =>
              println(s"Retrieved task:")
              println(s"  Title: ${retrieved.title}")
              println(s"  Description: ${retrieved.description.getOrElse("N/A")}")
              println(s"  Priority: ${TaskPriority.toString(retrieved.priority)}")
            case Left(error) =>
              println(s"Error getting task: $error")
          }
        case None => Future.successful(())
      }
      
      // Update task status
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n4. Updating task status to in_progress...")
          updateTaskStatus(task.id, "in_progress").map {
            case Right(updated) =>
              println(s"Status updated successfully!")
              println(s"  New status: ${TaskStatus.toString(updated.status)}")
            case Left(error) =>
              println(s"Error updating status: $error")
          }
        case None => Future.successful(())
      }
      
      // Update task details
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n5. Updating task details...")
          val updateRequest = UpdateTaskRequest(
            description = Some("Updated description from Scala client"),
            priority = Some(TaskPriority.Medium)
          )
          updateTask(task.id, updateRequest).map {
            case Right(updated) =>
              println(s"Task updated successfully!")
              println(s"  Description: ${updated.description.getOrElse("N/A")}")
              println(s"  Priority: ${TaskPriority.toString(updated.priority)}")
            case Left(error) =>
              println(s"Error updating task: $error")
          }
        case None => Future.successful(())
      }
      
      // Delete the task
      _ <- createdTask match {
        case Some(task) =>
          println(s"\n6. Deleting task ${task.id}...")
          deleteTask(task.id).map {
            case Right(_) =>
              println(s"Task deleted successfully!")
            case Left(error) =>
              println(s"Error deleting task: $error")
          }
        case None => Future.successful(())
      }
      
    } yield {
      println("\n" + "="*50)
      println("Demo completed!")
      println("="*50)
    }
  }
}

// Copy models from server (normally would be in shared module)
object models {
  import java.time.Instant
  import java.util.UUID
  import io.circe.{Decoder, Encoder}
  import io.circe.generic.semiauto._
  
  sealed trait TaskStatus
  object TaskStatus {
    case object Pending extends TaskStatus
    case object InProgress extends TaskStatus
    case object Completed extends TaskStatus
    case object Cancelled extends TaskStatus
    
    def fromString(s: String): Option[TaskStatus] = s.toLowerCase match {
      case "pending" => Some(Pending)
      case "in_progress" | "inprogress" => Some(InProgress)
      case "completed" => Some(Completed)
      case "cancelled" => Some(Cancelled)
      case _ => None
    }
    
    def toString(status: TaskStatus): String = status match {
      case Pending => "pending"
      case InProgress => "in_progress"
      case Completed => "completed"
      case Cancelled => "cancelled"
    }
    
    implicit val encoder: Encoder[TaskStatus] = Encoder.encodeString.contramap(toString)
    implicit val decoder: Decoder[TaskStatus] = Decoder.decodeString.emap { str =>
      fromString(str).toRight(s"Invalid status: $str")
    }
  }
  
  sealed trait TaskPriority
  object TaskPriority {
    case object Low extends TaskPriority
    case object Medium extends TaskPriority
    case object High extends TaskPriority
    case object Urgent extends TaskPriority
    
    def fromString(s: String): Option[TaskPriority] = s.toLowerCase match {
      case "low" => Some(Low)
      case "medium" => Some(Medium)
      case "high" => Some(High)
      case "urgent" => Some(Urgent)
      case _ => None
    }
    
    def toString(priority: TaskPriority): String = priority match {
      case Low => "low"
      case Medium => "medium"
      case High => "high"
      case Urgent => "urgent"
    }
    
    implicit val encoder: Encoder[TaskPriority] = Encoder.encodeString.contramap(toString)
    implicit val decoder: Decoder[TaskPriority] = Decoder.decodeString.emap { str =>
      fromString(str).toRight(s"Invalid priority: $str")
    }
  }
  
  case class Task(
    id: String,
    title: String,
    description: Option[String] = None,
    status: TaskStatus = TaskStatus.Pending,
    priority: TaskPriority = TaskPriority.Medium,
    tags: List[String] = List.empty,
    assignedTo: Option[String] = None,
    createdAt: Instant = Instant.now(),
    updatedAt: Instant = Instant.now()
  )
  
  object Task {
    implicit val encoder: Encoder[Task] = deriveEncoder[Task]
    implicit val decoder: Decoder[Task] = deriveDecoder[Task]
  }
  
  case class CreateTaskRequest(
    title: String,
    description: Option[String] = None,
    priority: Option[TaskPriority] = None,
    tags: Option[List[String]] = None,
    assignedTo: Option[String] = None
  )
  
  object CreateTaskRequest {
    implicit val decoder: Decoder[CreateTaskRequest] = deriveDecoder[CreateTaskRequest]
    implicit val encoder: Encoder[CreateTaskRequest] = deriveEncoder[CreateTaskRequest]
  }
  
  case class UpdateTaskRequest(
    title: Option[String] = None,
    description: Option[String] = None,
    status: Option[TaskStatus] = None,
    priority: Option[TaskPriority] = None,
    tags: Option[List[String]] = None,
    assignedTo: Option[String] = None
  )
  
  object UpdateTaskRequest {
    implicit val decoder: Decoder[UpdateTaskRequest] = deriveDecoder[UpdateTaskRequest]
    implicit val encoder: Encoder[UpdateTaskRequest] = deriveEncoder[UpdateTaskRequest]
  }
  
  case class ListTasksResponse(
    tasks: List[Task],
    totalCount: Int,
    pageSize: Int,
    nextPageToken: Option[String] = None
  )
  
  object ListTasksResponse {
    implicit val encoder: Encoder[ListTasksResponse] = deriveEncoder[ListTasksResponse]
    implicit val decoder: Decoder[ListTasksResponse] = deriveDecoder[ListTasksResponse]
  }
}