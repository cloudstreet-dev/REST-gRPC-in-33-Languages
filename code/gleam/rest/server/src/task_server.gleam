import gleam/erlang/process
import gleam/http
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/bytes_builder
import gleam/bit_array
import gleam/json
import gleam/dynamic
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/list
import gleam/int
import gleam/io
import mist
import task_store
import types.{
  type Task, type TaskStatus, type TaskPriority, type CreateTaskRequest,
  type UpdateTaskRequest,
  Task, Pending, InProgress, Completed, Cancelled,
  Low, Medium, High, Urgent
}

pub fn main() {
  io.println("Starting Gleam Task Server on port 8080...")
  
  // Start the task store
  let assert Ok(store) = task_store.start()
  
  // Create the HTTP handler
  let handler = fn(req: Request(mist.Connection)) -> Response(mist.ResponseData) {
    handle_request(req, store)
  }
  
  // Start the server
  let assert Ok(_) = 
    mist.new(handler)
    |> mist.port(8080)
    |> mist.start_http()
  
  // Keep the server running
  process.sleep_forever()
}

fn handle_request(
  req: Request(mist.Connection),
  store: process.Subject(task_store.Message)
) -> Response(mist.ResponseData) {
  let path = request.path_segments(req)
  let method = req.method
  
  case method, path {
    // Health check
    http.Get, [] | ["health"] -> {
      response.new(200)
      |> response.set_body(mist.Bytes(bytes_builder.from_string(
        json.to_string(json.object([
          #("status", json.string("healthy")),
          #("service", json.string("task-server")),
          #("version", json.string("1.0.0"))
        ]))
      )))
      |> response.set_header("content-type", "application/json")
    }
    
    // List tasks
    http.Get, ["api", "tasks"] -> {
      let query = request.get_query(req) |> result.unwrap([])
      handle_list_tasks(store, query)
    }
    
    // Get specific task
    http.Get, ["api", "tasks", id] -> {
      handle_get_task(store, id)
    }
    
    // Create task
    http.Post, ["api", "tasks"] -> {
      case mist.read_body(req, 1024 * 1024) {
        Ok(req_with_body) -> {
          handle_create_task(store, req_with_body.body)
        }
        Error(_) -> {
          error_response(400, "Invalid request body")
        }
      }
    }
    
    // Update task
    http.Put, ["api", "tasks", id] -> {
      case mist.read_body(req, 1024 * 1024) {
        Ok(req_with_body) -> {
          handle_update_task(store, id, req_with_body.body)
        }
        Error(_) -> {
          error_response(400, "Invalid request body")
        }
      }
    }
    
    // Update task status
    http.Patch, ["api", "tasks", id, "status"] -> {
      case mist.read_body(req, 1024 * 1024) {
        Ok(req_with_body) -> {
          handle_update_status(store, id, req_with_body.body)
        }
        Error(_) -> {
          error_response(400, "Invalid request body")
        }
      }
    }
    
    // Delete task
    http.Delete, ["api", "tasks", id] -> {
      handle_delete_task(store, id)
    }
    
    // Method not allowed
    _, ["api", "tasks"] | ["api", "tasks", ..] -> {
      error_response(405, "Method not allowed")
    }
    
    // Not found
    _, _ -> {
      error_response(404, "Not found")
    }
  }
}

fn handle_list_tasks(
  store: process.Subject(task_store.Message),
  query: List(#(String, String))
) -> Response(mist.ResponseData) {
  // Parse query parameters
  let status_filter = list.find(query, fn(q) { q.0 == "status" })
    |> result.map(fn(q) { parse_status(q.1) })
    |> result.flatten
    |> option.from_result
  
  let assigned_filter = list.find(query, fn(q) { q.0 == "assigned_to" })
    |> result.map(fn(q) { q.1 })
    |> option.from_result
  
  // Get tasks from store
  let tasks = task_store.list_tasks(store, status_filter, assigned_filter)
  
  // Convert to JSON
  let json_tasks = list.map(tasks, task_to_json)
  let response_json = json.object([
    #("tasks", json.array(json_tasks)),
    #("count", json.int(list.length(tasks)))
  ])
  
  response.new(200)
  |> response.set_body(mist.Bytes(bytes_builder.from_string(json.to_string(response_json))))
  |> response.set_header("content-type", "application/json")
}

fn handle_get_task(
  store: process.Subject(task_store.Message),
  id: String
) -> Response(mist.ResponseData) {
  case task_store.get_task(store, id) {
    Some(task) -> {
      response.new(200)
      |> response.set_body(mist.Bytes(bytes_builder.from_string(
        json.to_string(task_to_json(task))
      )))
      |> response.set_header("content-type", "application/json")
    }
    None -> {
      error_response(404, "Task not found")
    }
  }
}

fn handle_create_task(
  store: process.Subject(task_store.Message),
  body: mist.Body
) -> Response(mist.ResponseData) {
  case body {
    mist.Bytes(data) -> {
      let body_string = bytes_builder.to_bit_array(data)
        |> bit_array.to_string
        |> result.unwrap("")
      
      case parse_create_request(body_string) {
        Ok(request) -> {
          let task = task_store.create_task(store, request)
          response.new(201)
          |> response.set_body(mist.Bytes(bytes_builder.from_string(
            json.to_string(task_to_json(task))
          )))
          |> response.set_header("content-type", "application/json")
        }
        Error(_) -> {
          error_response(400, "Invalid request format")
        }
      }
    }
    _ -> {
      error_response(400, "Invalid request body")
    }
  }
}

fn handle_update_task(
  store: process.Subject(task_store.Message),
  id: String,
  body: mist.Body
) -> Response(mist.ResponseData) {
  case body {
    mist.Bytes(data) -> {
      let body_string = bytes_builder.to_bit_array(data)
        |> bit_array.to_string
        |> result.unwrap("")
      
      case parse_update_request(body_string) {
        Ok(request) -> {
          case task_store.update_task(store, id, request) {
            Some(task) -> {
              response.new(200)
              |> response.set_body(mist.Bytes(bytes_builder.from_string(
                json.to_string(task_to_json(task))
              )))
              |> response.set_header("content-type", "application/json")
            }
            None -> {
              error_response(404, "Task not found")
            }
          }
        }
        Error(_) -> {
          error_response(400, "Invalid request format")
        }
      }
    }
    _ -> {
      error_response(400, "Invalid request body")
    }
  }
}

fn handle_update_status(
  store: process.Subject(task_store.Message),
  id: String,
  body: mist.Body
) -> Response(mist.ResponseData) {
  case body {
    mist.Bytes(data) -> {
      let body_string = bytes_builder.to_bit_array(data)
        |> bit_array.to_string
        |> result.unwrap("")
      
      case parse_status_update(body_string) {
        Ok(status) -> {
          case task_store.update_task_status(store, id, status) {
            Some(task) -> {
              response.new(200)
              |> response.set_body(mist.Bytes(bytes_builder.from_string(
                json.to_string(task_to_json(task))
              )))
              |> response.set_header("content-type", "application/json")
            }
            None -> {
              error_response(404, "Task not found")
            }
          }
        }
        Error(_) -> {
          error_response(400, "Invalid status")
        }
      }
    }
    _ -> {
      error_response(400, "Invalid request body")
    }
  }
}

fn handle_delete_task(
  store: process.Subject(task_store.Message),
  id: String
) -> Response(mist.ResponseData) {
  case task_store.delete_task(store, id) {
    True -> {
      response.new(204)
      |> response.set_body(mist.Bytes(bytes_builder.new()))
    }
    False -> {
      error_response(404, "Task not found")
    }
  }
}

fn error_response(code: Int, message: String) -> Response(mist.ResponseData) {
  let error_json = json.object([
    #("error", json.string(message)),
    #("code", json.int(code))
  ])
  
  response.new(code)
  |> response.set_body(mist.Bytes(bytes_builder.from_string(json.to_string(error_json))))
  |> response.set_header("content-type", "application/json")
}

fn task_to_json(task: Task) -> json.Json {
  json.object([
    #("id", json.string(task.id)),
    #("title", json.string(task.title)),
    #("description", case task.description {
      Some(desc) -> json.string(desc)
      None -> json.null()
    }),
    #("status", json.string(status_to_string(task.status))),
    #("priority", json.string(priority_to_string(task.priority))),
    #("tags", json.array(list.map(task.tags, json.string))),
    #("assigned_to", case task.assigned_to {
      Some(assignee) -> json.string(assignee)
      None -> json.null()
    }),
    #("created_at", json.float(task.created_at)),
    #("updated_at", json.float(task.updated_at))
  ])
}

fn parse_create_request(body: String) -> Result(types.CreateTaskRequest, Nil) {
  use decoded <- result.try(json.decode(body, dynamic.dynamic))
  use title <- result.try(dynamic.field("title", dynamic.string)(decoded))
  
  let description = dynamic.field("description", dynamic.string)(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  let priority = dynamic.field("priority", dynamic.string)(decoded)
    |> result.map(parse_priority)
    |> result.flatten
    |> result.unwrap(Medium)
  
  let tags = dynamic.field("tags", dynamic.list(dynamic.string))(decoded)
    |> result.unwrap([])
  
  let assigned_to = dynamic.field("assigned_to", dynamic.string)(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  Ok(types.CreateTaskRequest(
    title: title,
    description: description,
    priority: priority,
    tags: tags,
    assigned_to: assigned_to
  ))
}

fn parse_update_request(body: String) -> Result(types.UpdateTaskRequest, Nil) {
  use decoded <- result.try(json.decode(body, dynamic.dynamic))
  
  let title = dynamic.field("title", dynamic.string)(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  let description = dynamic.field("description", dynamic.string)(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  let status = dynamic.field("status", dynamic.string)(decoded)
    |> result.map(parse_status)
    |> result.flatten
    |> option.from_result
  
  let priority = dynamic.field("priority", dynamic.string)(decoded)
    |> result.map(parse_priority)
    |> result.flatten
    |> option.from_result
  
  let tags = dynamic.field("tags", dynamic.list(dynamic.string))(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  let assigned_to = dynamic.field("assigned_to", dynamic.string)(decoded)
    |> result.map(Some)
    |> result.unwrap(None)
  
  Ok(types.UpdateTaskRequest(
    title: title,
    description: description,
    status: status,
    priority: priority,
    tags: tags,
    assigned_to: assigned_to
  ))
}

fn parse_status_update(body: String) -> Result(TaskStatus, Nil) {
  use decoded <- result.try(json.decode(body, dynamic.dynamic))
  use status_str <- result.try(dynamic.field("status", dynamic.string)(decoded))
  parse_status(status_str)
}

fn parse_status(status: String) -> Result(TaskStatus, Nil) {
  case string.lowercase(status) {
    "pending" -> Ok(Pending)
    "in-progress" | "in_progress" -> Ok(InProgress)
    "completed" -> Ok(Completed)
    "cancelled" -> Ok(Cancelled)
    _ -> Error(Nil)
  }
}

fn parse_priority(priority: String) -> Result(TaskPriority, Nil) {
  case string.lowercase(priority) {
    "low" -> Ok(Low)
    "medium" -> Ok(Medium)
    "high" -> Ok(High)
    "urgent" -> Ok(Urgent)
    _ -> Error(Nil)
  }
}

fn status_to_string(status: TaskStatus) -> String {
  case status {
    Pending -> "pending"
    InProgress -> "in-progress"
    Completed -> "completed"
    Cancelled -> "cancelled"
  }
}

fn priority_to_string(priority: TaskPriority) -> String {
  case priority {
    Low -> "low"
    Medium -> "medium"
    High -> "high"
    Urgent -> "urgent"
  }
}