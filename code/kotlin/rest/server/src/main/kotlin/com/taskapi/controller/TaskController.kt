package com.taskapi.controller

import com.taskapi.model.*
import com.taskapi.service.TaskService
import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

fun Application.configureRouting() {
    val taskService = TaskService()

    routing {
        route("/api/v1") {
            taskRoutes(taskService)
        }
    }
}

fun Route.taskRoutes(taskService: TaskService) {
    route("/tasks") {
        get {
            try {
                val params = QueryParams(
                    pageSize = call.request.queryParameters["pageSize"]?.toIntOrNull() ?: 20,
                    pageToken = call.request.queryParameters["pageToken"],
                    status = call.request.queryParameters["status"]?.let { 
                        try { TaskStatus.valueOf(it.uppercase()) } catch (e: Exception) { null }
                    },
                    assignedTo = call.request.queryParameters["assignedTo"],
                    tags = call.request.queryParameters["tags"],
                    sortOrder = call.request.queryParameters["sortOrder"]
                )
                
                val response = taskService.listTasks(params)
                call.respond(HttpStatusCode.OK, response)
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.InternalServerError,
                    ErrorResponse("internal_error", "An error occurred while listing tasks")
                )
            }
        }

        get("/{id}") {
            val id = call.parameters["id"]
            if (id.isNullOrBlank()) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Task ID is required")
                )
                return@get
            }

            val task = taskService.getTask(id)
            if (task != null) {
                call.respond(HttpStatusCode.OK, task)
            } else {
                call.respond(
                    HttpStatusCode.NotFound,
                    ErrorResponse("not_found", "Task with ID $id not found")
                )
            }
        }

        post {
            try {
                val request = call.receive<CreateTaskRequest>()
                
                // Validate request
                if (request.title.isBlank()) {
                    call.respond(
                        HttpStatusCode.BadRequest,
                        ErrorResponse("validation_error", "Title is required")
                    )
                    return@post
                }
                
                if (request.title.length > 200) {
                    call.respond(
                        HttpStatusCode.BadRequest,
                        ErrorResponse("validation_error", "Title must be 200 characters or less")
                    )
                    return@post
                }
                
                val task = taskService.createTask(request)
                call.respond(HttpStatusCode.Created, task)
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Invalid request data")
                )
            }
        }

        put("/{id}") {
            val id = call.parameters["id"]
            if (id.isNullOrBlank()) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Task ID is required")
                )
                return@put
            }

            try {
                val request = call.receive<UpdateTaskRequest>()
                
                // Validate if title is provided
                if (request.title != null && request.title.isBlank()) {
                    call.respond(
                        HttpStatusCode.BadRequest,
                        ErrorResponse("validation_error", "Title cannot be empty")
                    )
                    return@put
                }
                
                if (request.title != null && request.title.length > 200) {
                    call.respond(
                        HttpStatusCode.BadRequest,
                        ErrorResponse("validation_error", "Title must be 200 characters or less")
                    )
                    return@put
                }
                
                val task = taskService.updateTask(id, request)
                if (task != null) {
                    call.respond(HttpStatusCode.OK, task)
                } else {
                    call.respond(
                        HttpStatusCode.NotFound,
                        ErrorResponse("not_found", "Task with ID $id not found")
                    )
                }
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Invalid request data")
                )
            }
        }

        patch("/{id}/status") {
            val id = call.parameters["id"]
            if (id.isNullOrBlank()) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Task ID is required")
                )
                return@patch
            }

            try {
                val request = call.receive<UpdateStatusRequest>()
                val task = taskService.updateTaskStatus(id, request.status)
                
                if (task != null) {
                    call.respond(HttpStatusCode.OK, task)
                } else {
                    call.respond(
                        HttpStatusCode.NotFound,
                        ErrorResponse("not_found", "Task with ID $id not found")
                    )
                }
            } catch (e: Exception) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Invalid request data")
                )
            }
        }

        delete("/{id}") {
            val id = call.parameters["id"]
            if (id.isNullOrBlank()) {
                call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("invalid_request", "Task ID is required")
                )
                return@delete
            }

            val deleted = taskService.deleteTask(id)
            if (deleted) {
                call.respond(HttpStatusCode.NoContent)
            } else {
                call.respond(
                    HttpStatusCode.NotFound,
                    ErrorResponse("not_found", "Task with ID $id not found")
                )
            }
        }
    }
}