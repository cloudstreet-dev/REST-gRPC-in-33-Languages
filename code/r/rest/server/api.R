#!/usr/bin/env Rscript

# Task Management REST API Server
# Using Plumber for REST API

library(plumber)
library(uuid)
library(jsonlite)

# Task class definition
Task <- setRefClass("Task",
  fields = list(
    id = "character",
    title = "character",
    description = "character",
    status = "character",
    priority = "character",
    tags = "list",
    assigned_to = "character",
    created_at = "character",
    updated_at = "character"
  ),
  methods = list(
    initialize = function(title = "", description = "", status = "pending",
                         priority = "medium", tags = list(), assigned_to = "",
                         id = NULL, created_at = NULL, updated_at = NULL) {
      if (is.null(id)) {
        .self$id <<- UUIDgenerate()
      } else {
        .self$id <<- id
      }
      .self$title <<- title
      .self$description <<- description
      .self$status <<- status
      .self$priority <<- priority
      .self$tags <<- as.list(tags)
      .self$assigned_to <<- assigned_to
      
      current_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      .self$created_at <<- ifelse(is.null(created_at), current_time, created_at)
      .self$updated_at <<- ifelse(is.null(updated_at), current_time, updated_at)
    },
    
    update = function(updates) {
      if (!is.null(updates$title)) .self$title <<- updates$title
      if (!is.null(updates$description)) .self$description <<- updates$description
      if (!is.null(updates$status)) .self$status <<- updates$status
      if (!is.null(updates$priority)) .self$priority <<- updates$priority
      if (!is.null(updates$tags)) .self$tags <<- as.list(updates$tags)
      if (!is.null(updates$assigned_to)) .self$assigned_to <<- updates$assigned_to
      .self$updated_at <<- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    },
    
    to_list = function() {
      list(
        id = .self$id,
        title = .self$title,
        description = .self$description,
        status = .self$status,
        priority = .self$priority,
        tags = .self$tags,
        assigned_to = .self$assigned_to,
        created_at = .self$created_at,
        updated_at = .self$updated_at
      )
    }
  )
)

# Repository class
Repository <- setRefClass("Repository",
  fields = list(
    tasks = "list"
  ),
  methods = list(
    initialize = function() {
      .self$tasks <<- list()
      .self$load_sample_data()
    },
    
    load_sample_data = function() {
      task1 <- Task$new(
        title = "Implement R REST API",
        description = "Build a REST API server using Plumber framework",
        status = "in_progress",
        priority = "high",
        tags = list("r", "rest", "api"),
        assigned_to = "data-team"
      )
      .self$tasks[[task1$id]] <<- task1
      
      task2 <- Task$new(
        title = "Add Shiny dashboard",
        description = "Create interactive dashboard with Shiny",
        status = "pending",
        priority = "medium",
        tags = list("r", "shiny", "visualization"),
        assigned_to = "analytics-team"
      )
      .self$tasks[[task2$id]] <<- task2
      
      task3 <- Task$new(
        title = "Write testthat tests",
        description = "Add comprehensive test coverage using testthat",
        status = "pending",
        priority = "high",
        tags = list("testing", "quality"),
        assigned_to = "qa-team"
      )
      .self$tasks[[task3$id]] <<- task3
    },
    
    list_tasks = function(status = NULL, assigned_to = NULL, tags = NULL,
                         page_size = 20, page_token = 0, sort_by = "created_at",
                         sort_order = "desc") {
      # Get all tasks as list
      task_list <- lapply(.self$tasks, function(t) t$to_list())
      
      # Filter by status
      if (!is.null(status) && status != "") {
        task_list <- Filter(function(t) t$status == status, task_list)
      }
      
      # Filter by assigned_to
      if (!is.null(assigned_to) && assigned_to != "") {
        task_list <- Filter(function(t) t$assigned_to == assigned_to, task_list)
      }
      
      # Filter by tags
      if (!is.null(tags) && length(tags) > 0) {
        if (is.character(tags)) {
          tags <- strsplit(tags, ",")[[1]]
        }
        task_list <- Filter(function(t) {
          all(tags %in% unlist(t$tags))
        }, task_list)
      }
      
      # Sort tasks
      if (sort_by %in% c("created_at", "updated_at", "title")) {
        task_list <- task_list[order(sapply(task_list, function(t) t[[sort_by]]),
                                    decreasing = (sort_order == "desc"))]
      }
      
      # Pagination
      total_count <- length(task_list)
      page_size <- min(as.numeric(page_size), 100)
      page_token <- as.numeric(page_token)
      
      start_idx <- page_token + 1
      end_idx <- min(start_idx + page_size - 1, total_count)
      
      if (start_idx <= total_count) {
        paginated <- task_list[start_idx:end_idx]
      } else {
        paginated <- list()
      }
      
      next_token <- if (end_idx < total_count) end_idx else NULL
      
      list(
        tasks = paginated,
        total_count = total_count,
        page_size = page_size,
        next_page_token = next_token
      )
    },
    
    get_task = function(id) {
      if (id %in% names(.self$tasks)) {
        return(.self$tasks[[id]]$to_list())
      }
      return(NULL)
    },
    
    create_task = function(data) {
      task <- Task$new(
        title = data$title,
        description = ifelse(is.null(data$description), "", data$description),
        status = ifelse(is.null(data$status), "pending", data$status),
        priority = ifelse(is.null(data$priority), "medium", data$priority),
        tags = ifelse(is.null(data$tags), list(), data$tags),
        assigned_to = ifelse(is.null(data$assigned_to), "", data$assigned_to)
      )
      .self$tasks[[task$id]] <<- task
      return(task$to_list())
    },
    
    update_task = function(id, updates) {
      if (id %in% names(.self$tasks)) {
        .self$tasks[[id]]$update(updates)
        return(.self$tasks[[id]]$to_list())
      }
      return(NULL)
    },
    
    update_task_status = function(id, status) {
      return(.self$update_task(id, list(status = status)))
    },
    
    delete_task = function(id) {
      if (id %in% names(.self$tasks)) {
        .self$tasks[[id]] <<- NULL
        return(TRUE)
      }
      return(FALSE)
    },
    
    count = function() {
      return(length(.self$tasks))
    }
  )
)

# Initialize repository
repository <- Repository$new()

# Print startup banner
cat("╔════════════════════════════════════════════════╗\n")
cat("║          R Task Management REST API            ║\n")
cat("║            Built with Plumber                  ║\n")
cat("╚════════════════════════════════════════════════╝\n\n")

port <- as.numeric(Sys.getenv("PORT", 8080))

cat(sprintf("[INFO] R Task REST Server starting on port %d\n", port))
cat(sprintf("[INFO] Visit http://localhost:%d/api/tasks\n\n", port))

cat("Available endpoints:\n")
cat("  GET    /api/tasks          - List all tasks\n")
cat("  GET    /api/tasks/{id}     - Get a specific task\n")
cat("  POST   /api/tasks          - Create a new task\n")
cat("  PUT    /api/tasks/{id}     - Update a task\n")
cat("  PATCH  /api/tasks/{id}/status - Update task status\n")
cat("  DELETE /api/tasks/{id}     - Delete a task\n")
cat("  GET    /health             - Health check\n\n")

cat("Sample requests:\n")
cat(sprintf("  curl http://localhost:%d/api/tasks\n", port))
cat(sprintf("  curl -X POST http://localhost:%d/api/tasks \\\n", port))
cat("    -H \"Content-Type: application/json\" \\\n")
cat("    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'\n\n")

cat("[INFO] Press Ctrl+C to stop the server\n\n")

#* @apiTitle Task Management API
#* @apiDescription REST API for task management built with R and Plumber

#* Enable CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 204
    return(list())
  }
  
  plumber::forward()
}

#* Health check endpoint
#* @get /health
function() {
  list(
    status = "healthy",
    service = "r-task-api",
    task_count = repository$count()
  )
}

#* List all tasks
#* @param status:str Filter by status
#* @param assigned_to:str Filter by assignee
#* @param tags:str Filter by tags (comma-separated)
#* @param page_size:int Number of results per page
#* @param page_token:int Pagination token
#* @param sort_by:str Sort field
#* @param sort_order:str Sort order (asc/desc)
#* @get /api/tasks
function(status = NULL, assigned_to = NULL, tags = NULL,
         page_size = 20, page_token = 0, sort_by = "created_at",
         sort_order = "desc") {
  repository$list_tasks(status, assigned_to, tags, page_size, 
                       page_token, sort_by, sort_order)
}

#* Get a specific task
#* @param id:str Task ID
#* @get /api/tasks/<id>
function(id, res) {
  task <- repository$get_task(id)
  if (is.null(task)) {
    res$status <- 404
    return(list(error = "Task not found"))
  }
  return(task)
}

#* Create a new task
#* @post /api/tasks
function(req, res) {
  data <- req$body
  
  if (is.null(data$title) || data$title == "") {
    res$status <- 400
    return(list(error = "Title is required"))
  }
  
  task <- repository$create_task(data)
  res$status <- 201
  return(task)
}

#* Update a task
#* @param id:str Task ID
#* @put /api/tasks/<id>
function(id, req, res) {
  task <- repository$update_task(id, req$body)
  if (is.null(task)) {
    res$status <- 404
    return(list(error = "Task not found"))
  }
  return(task)
}

#* Update task status
#* @param id:str Task ID
#* @patch /api/tasks/<id>/status
function(id, req, res) {
  data <- req$body
  
  if (is.null(data$status)) {
    res$status <- 400
    return(list(error = "Status is required"))
  }
  
  task <- repository$update_task_status(id, data$status)
  if (is.null(task)) {
    res$status <- 404
    return(list(error = "Task not found"))
  }
  return(task)
}

#* Delete a task
#* @param id:str Task ID
#* @delete /api/tasks/<id>
function(id, res) {
  if (repository$delete_task(id)) {
    res$status <- 204
    return(list())
  } else {
    res$status <- 404
    return(list(error = "Task not found"))
  }
}