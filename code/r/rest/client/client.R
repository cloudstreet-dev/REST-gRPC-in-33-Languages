#!/usr/bin/env Rscript

# Task Management REST API Client
# Using httr for HTTP requests

library(httr)
library(jsonlite)

# Configuration
BASE_URL <- Sys.getenv("API_URL", "http://localhost:8080/api")

# Client class
TaskClient <- setRefClass("TaskClient",
  fields = list(
    base_url = "character"
  ),
  methods = list(
    initialize = function(base_url = BASE_URL) {
      .self$base_url <<- base_url
    },
    
    list_tasks = function(status = NULL, assigned_to = NULL, tags = NULL,
                         page_size = 20, page_token = 0, sort_by = "created_at",
                         sort_order = "desc") {
      params <- list()
      if (!is.null(status)) params$status <- status
      if (!is.null(assigned_to)) params$assigned_to <- assigned_to
      if (!is.null(tags)) {
        if (is.list(tags) || is.vector(tags)) {
          params$tags <- paste(tags, collapse = ",")
        } else {
          params$tags <- tags
        }
      }
      params$page_size <- page_size
      params$page_token <- page_token
      params$sort_by <- sort_by
      params$sort_order <- sort_order
      
      response <- GET(
        paste0(.self$base_url, "/tasks"),
        query = params,
        add_headers("Content-Type" = "application/json")
      )
      
      if (status_code(response) == 200) {
        return(content(response, "parsed"))
      } else {
        stop(paste("Request failed with status", status_code(response)))
      }
    },
    
    get_task = function(id) {
      response <- GET(
        paste0(.self$base_url, "/tasks/", id),
        add_headers("Content-Type" = "application/json")
      )
      
      if (status_code(response) == 200) {
        return(content(response, "parsed"))
      } else if (status_code(response) == 404) {
        return(NULL)
      } else {
        stop(paste("Request failed with status", status_code(response)))
      }
    },
    
    create_task = function(title, description = "", status = "pending",
                          priority = "medium", tags = list(), assigned_to = "") {
      body <- list(
        title = title,
        description = description,
        status = status,
        priority = priority,
        tags = tags,
        assigned_to = assigned_to
      )
      
      response <- POST(
        paste0(.self$base_url, "/tasks"),
        body = toJSON(body, auto_unbox = TRUE),
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
      )
      
      if (status_code(response) == 201) {
        return(content(response, "parsed"))
      } else {
        stop(paste("Request failed with status", status_code(response)))
      }
    },
    
    update_task = function(id, updates) {
      response <- PUT(
        paste0(.self$base_url, "/tasks/", id),
        body = toJSON(updates, auto_unbox = TRUE),
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
      )
      
      if (status_code(response) == 200) {
        return(content(response, "parsed"))
      } else if (status_code(response) == 404) {
        return(NULL)
      } else {
        stop(paste("Request failed with status", status_code(response)))
      }
    },
    
    update_task_status = function(id, status) {
      response <- PATCH(
        paste0(.self$base_url, "/tasks/", id, "/status"),
        body = toJSON(list(status = status), auto_unbox = TRUE),
        add_headers("Content-Type" = "application/json"),
        encode = "raw"
      )
      
      if (status_code(response) == 200) {
        return(content(response, "parsed"))
      } else if (status_code(response) == 404) {
        return(NULL)
      } else {
        stop(paste("Request failed with status", status_code(response)))
      }
    },
    
    delete_task = function(id) {
      response <- DELETE(
        paste0(.self$base_url, "/tasks/", id),
        add_headers("Content-Type" = "application/json")
      )
      
      if (status_code(response) == 204) {
        return(TRUE)
      } else if (status_code(response) == 404) {
        return(FALSE)
      } else {
        stop(paste("Request failed with status", status_code(response)))
      }
    }
  )
)

# Demo function
run_demo <- function() {
  cat("╔════════════════════════════════════════════════╗\n")
  cat("║         R Task Management REST Client          ║\n")
  cat("║            Testing API Operations              ║\n")
  cat("╚════════════════════════════════════════════════╝\n\n")
  
  # Create client
  client <- TaskClient$new()
  
  # Wait for server to be ready
  Sys.sleep(1)
  
  # 1. List all tasks
  cat("1. Listing all tasks...\n")
  tryCatch({
    result <- client$list_tasks()
    cat(sprintf("   Found %d tasks\n", result$total_count))
    for (task in result$tasks) {
      cat(sprintf("   - [%s] %s (%s)\n", task$id, task$title, task$status))
    }
  }, error = function(e) {
    cat(sprintf("   Error: %s\n", e$message))
  })
  
  # 2. Create a new task
  cat("\n2. Creating a new task...\n")
  task_id <- NULL
  tryCatch({
    task <- client$create_task(
      title = "Analyze dataset with dplyr",
      description = "Use dplyr to perform exploratory data analysis",
      priority = "high",
      tags = list("r", "data-analysis", "dplyr"),
      assigned_to = "data-team"
    )
    task_id <- task$id
    cat(sprintf("   Created task: %s\n", task$title))
    cat(sprintf("   ID: %s\n", task$id))
    cat(sprintf("   Priority: %s\n", task$priority))
    cat(sprintf("   Tags: %s\n", paste(unlist(task$tags), collapse = ", ")))
  }, error = function(e) {
    cat(sprintf("   Error: %s\n", e$message))
  })
  
  if (!is.null(task_id)) {
    # 3. Get task details
    cat("\n3. Getting task details...\n")
    tryCatch({
      task <- client$get_task(task_id)
      cat(sprintf("   Title: %s\n", task$title))
      cat(sprintf("   Description: %s\n", task$description))
      cat(sprintf("   Status: %s\n", task$status))
      cat(sprintf("   Assigned to: %s\n", task$assigned_to))
    }, error = function(e) {
      cat(sprintf("   Error: %s\n", e$message))
    })
    
    # 4. Update task status
    cat("\n4. Updating task status to 'in_progress'...\n")
    tryCatch({
      task <- client$update_task_status(task_id, "in_progress")
      cat(sprintf("   Updated status to: %s\n", task$status))
    }, error = function(e) {
      cat(sprintf("   Error: %s\n", e$message))
    })
    
    # 5. Update task details
    cat("\n5. Updating task details...\n")
    tryCatch({
      updates <- list(
        title = "Complete EDA with tidyverse",
        priority = "urgent"
      )
      task <- client$update_task(task_id, updates)
      cat(sprintf("   Updated title: %s\n", task$title))
      cat(sprintf("   Updated priority: %s\n", task$priority))
    }, error = function(e) {
      cat(sprintf("   Error: %s\n", e$message))
    })
    
    # 6. Filter tasks by status
    cat("\n6. Filtering tasks by status...\n")
    tryCatch({
      result <- client$list_tasks(status = "in_progress")
      cat(sprintf("   Found %d in-progress tasks\n", result$total_count))
      for (task in result$tasks) {
        cat(sprintf("   - %s\n", task$title))
      }
    }, error = function(e) {
      cat(sprintf("   Error: %s\n", e$message))
    })
    
    # 7. Delete the task
    cat("\n7. Deleting the task...\n")
    tryCatch({
      success <- client$delete_task(task_id)
      if (success) {
        cat("   Task deleted successfully\n")
      } else {
        cat("   Failed to delete task\n")
      }
    }, error = function(e) {
      cat(sprintf("   Error: %s\n", e$message))
    })
    
    # 8. Verify deletion
    cat("\n8. Verifying deletion...\n")
    tryCatch({
      task <- client$get_task(task_id)
      if (is.null(task)) {
        cat("   Task not found (as expected)\n")
      } else {
        cat("   Error: Task still exists\n")
      }
    }, error = function(e) {
      cat(sprintf("   Error: %s\n", e$message))
    })
  }
  
  cat("\n✅ Demo completed successfully!\n")
}

# Run the demo
run_demo()