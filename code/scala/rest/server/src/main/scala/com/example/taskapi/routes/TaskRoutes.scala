package com.example.taskapi.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.example.taskapi.db.TaskRepository
import com.example.taskapi.models._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class TaskRoutes(repository: TaskRepository)(implicit ec: ExecutionContext) {
  
  val routes: Route = pathPrefix("api" / "tasks") {
    concat(
      // GET /api/tasks - List tasks
      pathEndOrSingleSlash {
        get {
          parameters(
            Symbol("status").as[String].optional,
            Symbol("assigned_to").as[String].optional,
            Symbol("tags").as[String].optional,
            Symbol("page_size").as[Int].withDefault(20),
            Symbol("page_token").as[String].optional,
            Symbol("sort_by").as[String].withDefault("created_at"),
            Symbol("sort_order").as[String].withDefault("desc")
          ) { (statusStr, assignedTo, tagsStr, pageSize, pageToken, sortBy, sortOrder) =>
            
            val status = statusStr.flatMap(TaskStatus.fromString)
            val tags = tagsStr.map(_.split(",").toList)
            val offset = pageToken.map(_.toInt).getOrElse(0)
            
            val futureResult = for {
              tasks <- repository.list(status, assignedTo, tags, pageSize, offset, sortBy, sortOrder)
              totalCount <- repository.count(status, assignedTo, tags)
            } yield {
              val nextToken = if (offset + pageSize < totalCount) Some((offset + pageSize).toString) else None
              ListTasksResponse(tasks, totalCount, pageSize, nextToken)
            }
            
            onComplete(futureResult) {
              case Success(response) => complete(response)
              case Failure(ex) => 
                complete(StatusCodes.InternalServerError, ErrorResponse(
                  "internal_error", 
                  s"Failed to list tasks: ${ex.getMessage}",
                  500
                ))
            }
          }
        }
      },
      
      // POST /api/tasks - Create task
      pathEndOrSingleSlash {
        post {
          entity(as[CreateTaskRequest]) { request =>
            if (request.title.trim.isEmpty) {
              complete(StatusCodes.BadRequest, ErrorResponse(
                "validation_error",
                "Title is required and cannot be empty",
                400
              ))
            } else {
              val task = Task.create(request)
              onComplete(repository.create(task)) {
                case Success(createdTask) => 
                  complete(StatusCodes.Created, createdTask)
                case Failure(ex) => 
                  complete(StatusCodes.InternalServerError, ErrorResponse(
                    "internal_error",
                    s"Failed to create task: ${ex.getMessage}",
                    500
                  ))
              }
            }
          }
        }
      },
      
      // Routes with ID parameter
      pathPrefix(Segment) { taskId =>
        concat(
          // GET /api/tasks/{id} - Get task by ID
          pathEndOrSingleSlash {
            get {
              onComplete(repository.getById(taskId)) {
                case Success(Some(task)) => complete(task)
                case Success(None) => 
                  complete(StatusCodes.NotFound, ErrorResponse(
                    "not_found",
                    s"Task with ID $taskId not found",
                    404
                  ))
                case Failure(ex) => 
                  complete(StatusCodes.InternalServerError, ErrorResponse(
                    "internal_error",
                    s"Failed to get task: ${ex.getMessage}",
                    500
                  ))
              }
            }
          },
          
          // PUT /api/tasks/{id} - Update task
          pathEndOrSingleSlash {
            put {
              entity(as[UpdateTaskRequest]) { request =>
                onComplete(repository.update(taskId, request)) {
                  case Success(Some(task)) => complete(task)
                  case Success(None) => 
                    complete(StatusCodes.NotFound, ErrorResponse(
                      "not_found",
                      s"Task with ID $taskId not found",
                      404
                    ))
                  case Failure(ex) => 
                    complete(StatusCodes.InternalServerError, ErrorResponse(
                      "internal_error",
                      s"Failed to update task: ${ex.getMessage}",
                      500
                    ))
                }
              }
            }
          },
          
          // PATCH /api/tasks/{id}/status - Update task status
          path("status") {
            patch {
              parameter(Symbol("status").as[String]) { statusStr =>
                TaskStatus.fromString(statusStr) match {
                  case Some(status) =>
                    onComplete(repository.updateStatus(taskId, status)) {
                      case Success(Some(task)) => complete(task)
                      case Success(None) => 
                        complete(StatusCodes.NotFound, ErrorResponse(
                          "not_found",
                          s"Task with ID $taskId not found",
                          404
                        ))
                      case Failure(ex) => 
                        complete(StatusCodes.InternalServerError, ErrorResponse(
                          "internal_error",
                          s"Failed to update status: ${ex.getMessage}",
                          500
                        ))
                    }
                  case None =>
                    complete(StatusCodes.BadRequest, ErrorResponse(
                      "validation_error",
                      s"Invalid status: $statusStr. Valid values are: pending, in_progress, completed, cancelled",
                      400
                    ))
                }
              }
            }
          },
          
          // DELETE /api/tasks/{id} - Delete task
          pathEndOrSingleSlash {
            delete {
              onComplete(repository.delete(taskId)) {
                case Success(true) => complete(StatusCodes.NoContent)
                case Success(false) => 
                  complete(StatusCodes.NotFound, ErrorResponse(
                    "not_found",
                    s"Task with ID $taskId not found",
                    404
                  ))
                case Failure(ex) => 
                  complete(StatusCodes.InternalServerError, ErrorResponse(
                    "internal_error",
                    s"Failed to delete task: ${ex.getMessage}",
                    500
                  ))
              }
            }
          }
        )
      }
    )
  }
  
  // Health check endpoint
  val healthRoute: Route = path("health") {
    get {
      complete(StatusCodes.OK, Map("status" -> "healthy", "service" -> "task-api"))
    }
  }
  
  // Combined routes
  val allRoutes: Route = concat(routes, healthRoute)
}