package com.example.taskapi.models

import java.time.Instant
import java.util.UUID
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._

// Task status enumeration
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

// Task priority enumeration
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

// Main Task model
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
  
  def create(request: CreateTaskRequest): Task = Task(
    id = UUID.randomUUID().toString,
    title = request.title,
    description = request.description,
    status = TaskStatus.Pending,
    priority = request.priority.getOrElse(TaskPriority.Medium),
    tags = request.tags.getOrElse(List.empty),
    assignedTo = request.assignedTo,
    createdAt = Instant.now(),
    updatedAt = Instant.now()
  )
}

// Request/Response models
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

case class ListTasksRequest(
  status: Option[TaskStatus] = None,
  assignedTo: Option[String] = None,
  tags: Option[List[String]] = None,
  pageSize: Int = 20,
  pageToken: Option[String] = None,
  sortBy: String = "created_at",
  sortOrder: String = "desc"
)

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

case class ErrorResponse(
  error: String,
  message: String,
  statusCode: Int
)

object ErrorResponse {
  implicit val encoder: Encoder[ErrorResponse] = deriveEncoder[ErrorResponse]
  implicit val decoder: Decoder[ErrorResponse] = deriveDecoder[ErrorResponse]
}