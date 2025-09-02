# R REST API Implementation

This directory contains a REST API implementation in R using the Plumber framework, designed for statistical computing and data analysis applications.

## Features

- **Full REST API** with CRUD operations
- **Reference class system** for object-oriented programming
- **Plumber framework** for REST API development
- **httr client** for HTTP requests
- **JSON serialization** with jsonlite
- **Statistical computing** integration capabilities

## Prerequisites

- R 4.0 or higher
- Required R packages:
  ```r
  install.packages(c("plumber", "httr", "jsonlite", "uuid"))
  ```

## Server

The server implements a complete REST API using Plumber, R's REST API framework.

### Running the Server

```bash
cd server
Rscript server.R
```

Or directly:
```bash
cd server
R -e "plumber::plumb('api.R')$run(port=8080, host='0.0.0.0')"
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks (with filtering and pagination)
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in_progress, completed, cancelled)
- `assigned_to` - Filter by assignee
- `tags` - Filter by tags (comma-separated)
- `page_size` - Number of results per page (default: 20, max: 100)
- `page_token` - Pagination token
- `sort_by` - Sort field (created_at, updated_at, title)
- `sort_order` - Sort order (asc, desc)

## Client

The client provides a comprehensive SDK for interacting with the REST API using httr.

### Running the Client Demo

```bash
cd client
Rscript client.R
```

### Using the Client Library

```r
source("client.R")

# Create client instance
client <- TaskClient$new()

# List all tasks
result <- client$list_tasks()

# Create a task
task <- client$create_task(
  title = "New Task",
  priority = "high",
  tags = list("important", "urgent")
)

# Get a task
task <- client$get_task(task_id)

# Update a task
task <- client$update_task(task_id, list(
  title = "Updated Title",
  priority = "low"
))

# Update task status
task <- client$update_task_status(task_id, "completed")

# Delete a task
success <- client$delete_task(task_id)
```

## Architecture

### Reference Classes

R's reference classes provide mutable objects with methods:

```r
Task <- setRefClass("Task",
  fields = list(
    id = "character",
    title = "character",
    status = "character"
  ),
  methods = list(
    update = function(updates) {
      if (!is.null(updates$title)) 
        .self$title <<- updates$title
    }
  )
)
```

### Plumber Annotations

Plumber uses special comments to define endpoints:

```r
#* List all tasks
#* @param status Filter by status
#* @get /api/tasks
function(status = NULL) {
  repository$list_tasks(status)
}
```

## R Features Demonstrated

### Reference Classes (R5)

```r
Repository <- setRefClass("Repository",
  fields = list(tasks = "list"),
  methods = list(
    add_task = function(task) {
      .self$tasks[[task$id]] <<- task
    }
  )
)
```

### Functional Programming

```r
# Filter and transform
filtered_tasks <- Filter(
  function(t) t$status == "pending",
  lapply(tasks, function(t) t$to_list())
)
```

### Data Frames Integration

```r
# Convert tasks to data frame for analysis
tasks_df <- do.call(rbind, lapply(tasks, function(t) {
  data.frame(
    id = t$id,
    title = t$title,
    status = t$status,
    priority = t$priority,
    stringsAsFactors = FALSE
  )
}))

# Analyze with dplyr
library(dplyr)
summary <- tasks_df %>%
  group_by(status) %>%
  summarise(count = n())
```

## Testing

Run tests with testthat:

```r
# Install testthat
install.packages("testthat")

# Run tests
testthat::test_dir("tests/")
```

Example test:

```r
library(testthat)

test_that("Task creation works", {
  task <- Task$new(title = "Test Task")
  expect_equal(task$title, "Test Task")
  expect_equal(task$status, "pending")
})
```

## Docker Support

```dockerfile
FROM rocker/r-ver:4.3.0

RUN R -e "install.packages(c('plumber', 'httr', 'jsonlite', 'uuid'))"

WORKDIR /app
COPY server/ .

EXPOSE 8080
CMD ["Rscript", "server.R"]
```

## Statistical Analysis Integration

The REST API can be extended to include statistical operations:

```r
#* Perform statistical analysis on tasks
#* @post /api/tasks/analyze
function(req) {
  tasks_df <- tasks_to_dataframe(repository$list_tasks()$tasks)
  
  analysis <- list(
    summary = summary(tasks_df),
    status_distribution = table(tasks_df$status),
    priority_distribution = table(tasks_df$priority),
    completion_rate = mean(tasks_df$status == "completed")
  )
  
  return(analysis)
}
```

## Performance Considerations

### Memory Management

```r
# Use environments for large datasets
cache <- new.env(hash = TRUE)
cache$data <- large_dataset

# Clear memory
rm(large_object)
gc()  # Garbage collection
```

### Parallel Processing

```r
library(parallel)

# Use multiple cores
cl <- makeCluster(detectCores() - 1)
results <- parLapply(cl, tasks, process_task)
stopCluster(cl)
```

## Development Tips

### Interactive Development

```r
# Load API for interactive testing
pr <- plumber::plumb("api.R")

# Test endpoints interactively
pr$routes

# Run with auto-reload for development
options(plumber.autoreload = TRUE)
pr$run(port = 8080)
```

### Debugging

```r
# Enable debug mode
options(plumber.debug = TRUE)

# Add browser() for breakpoints
function(req, res) {
  browser()  # Stops here for debugging
  # Rest of function
}
```

## Integration with Shiny

Create an interactive dashboard:

```r
library(shiny)

ui <- fluidPage(
  titlePanel("Task Management Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("status", "Filter by Status:",
                  choices = c("All", "pending", "in_progress", "completed"))
    ),
    mainPanel(
      tableOutput("tasks")
    )
  )
)

server <- function(input, output) {
  output$tasks <- renderTable({
    client <- TaskClient$new()
    result <- if(input$status == "All") {
      client$list_tasks()
    } else {
      client$list_tasks(status = input$status)
    }
    do.call(rbind, lapply(result$tasks, as.data.frame))
  })
}

shinyApp(ui = ui, server = server)
```

## Dependencies

- `plumber` - REST API framework
- `httr` - HTTP client
- `jsonlite` - JSON parsing
- `uuid` - UUID generation