import gleam/io
import gleam/result
import gleam/option.{type Option, None, Some}
import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/json
import gleam/dynamic
import gleam/list
import gleam/string
import gleam/int

// Task types
pub type TaskStatus {
  Pending
  InProgress
  Completed
  Cancelled
}

pub type Task {
  Task(
    id: String,
    title: String,
    description: Option(String),
    status: String,
    priority: String,
    tags: List(String),
    assigned_to: Option(String),
    created_at: Float,
    updated_at: Float,
  )
}

// Client type
pub type Client {
  Client(base_url: String)
}

// Create a new client
pub fn new(base_url: String) -> Client {
  Client(base_url: base_url)
}

// List all tasks
pub fn list_tasks(
  client: Client,
  status_filter: Option(String),
  assigned_filter: Option(String)
) -> Result(List(Task), String) {
  let base_path = "/api/tasks"
  
  let params = []
    |> fn(p) {
      case status_filter {
        Some(status) -> [#("status", status), ..p]
        None -> p
      }
    }
    |> fn(p) {
      case assigned_filter {
        Some(assigned) -> [#("assigned_to", assigned), ..p]
        None -> p
      }
    }
  
  let path = case params {
    [] -> base_path
    params -> {
      let query = params
        |> list.map(fn(p) { p.0 <> "=" <> p.1 })
        |> string.join("&")
      base_path <> "?" <> query
    }
  }
  
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_host(get_host(client.base_url))
    |> request.set_path(path)
    |> request.set_port(8080)
  
  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        200 -> parse_tasks_response(resp.body)
        _ -> Error("Failed to list tasks: " <> int.to_string(resp.status))
      }
    }
    Error(_) -> Error("Network error")
  }
}

// Get a specific task
pub fn get_task(client: Client, id: String) -> Result(Task, String) {
  let req = request.new()
    |> request.set_method(http.Get)
    |> request.set_host(get_host(client.base_url))
    |> request.set_path("/api/tasks/" <> id)
    |> request.set_port(8080)
  
  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        200 -> parse_task_response(resp.body)
        404 -> Error("Task not found")
        _ -> Error("Failed to get task: " <> int.to_string(resp.status))
      }
    }
    Error(_) -> Error("Network error")
  }
}

// Create a new task
pub fn create_task(
  client: Client,
  title: String,
  description: Option(String),
  priority: String,
  tags: List(String),
  assigned_to: Option(String)
) -> Result(Task, String) {
  let body_json = json.object([
    #("title", json.string(title)),
    #("description", case description {
      Some(desc) -> json.string(desc)
      None -> json.null()
    }),
    #("priority", json.string(priority)),
    #("tags", json.array(list.map(tags, json.string))),
    #("assigned_to", case assigned_to {
      Some(assignee) -> json.string(assignee)
      None -> json.null()
    })
  ])
  
  let req = request.new()
    |> request.set_method(http.Post)
    |> request.set_host(get_host(client.base_url))
    |> request.set_path("/api/tasks")
    |> request.set_port(8080)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(json.to_string(body_json))
  
  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        201 -> parse_task_response(resp.body)
        _ -> Error("Failed to create task: " <> int.to_string(resp.status))
      }
    }
    Error(_) -> Error("Network error")
  }
}

// Update a task
pub fn update_task(
  client: Client,
  id: String,
  title: Option(String),
  description: Option(String),
  status: Option(String),
  priority: Option(String),
  tags: Option(List(String)),
  assigned_to: Option(String)
) -> Result(Task, String) {
  let fields = []
    |> fn(f) {
      case title {
        Some(t) -> [#("title", json.string(t)), ..f]
        None -> f
      }
    }
    |> fn(f) {
      case description {
        Some(d) -> [#("description", json.string(d)), ..f]
        None -> f
      }
    }
    |> fn(f) {
      case status {
        Some(s) -> [#("status", json.string(s)), ..f]
        None -> f
      }
    }
    |> fn(f) {
      case priority {
        Some(p) -> [#("priority", json.string(p)), ..f]
        None -> f
      }
    }
    |> fn(f) {
      case tags {
        Some(t) -> [#("tags", json.array(list.map(t, json.string))), ..f]
        None -> f
      }
    }
    |> fn(f) {
      case assigned_to {
        Some(a) -> [#("assigned_to", json.string(a)), ..f]
        None -> f
      }
    }
  
  let body_json = json.object(fields)
  
  let req = request.new()
    |> request.set_method(http.Put)
    |> request.set_host(get_host(client.base_url))
    |> request.set_path("/api/tasks/" <> id)
    |> request.set_port(8080)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(json.to_string(body_json))
  
  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        200 -> parse_task_response(resp.body)
        404 -> Error("Task not found")
        _ -> Error("Failed to update task: " <> int.to_string(resp.status))
      }
    }
    Error(_) -> Error("Network error")
  }
}

// Update task status
pub fn update_task_status(
  client: Client,
  id: String,
  status: String
) -> Result(Task, String) {
  let body_json = json.object([
    #("status", json.string(status))
  ])
  
  let req = request.new()
    |> request.set_method(http.Patch)
    |> request.set_host(get_host(client.base_url))
    |> request.set_path("/api/tasks/" <> id <> "/status")
    |> request.set_port(8080)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(json.to_string(body_json))
  
  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        200 -> parse_task_response(resp.body)
        404 -> Error("Task not found")
        _ -> Error("Failed to update status: " <> int.to_string(resp.status))
      }
    }
    Error(_) -> Error("Network error")
  }
}

// Delete a task
pub fn delete_task(client: Client, id: String) -> Result(Nil, String) {
  let req = request.new()
    |> request.set_method(http.Delete)
    |> request.set_host(get_host(client.base_url))
    |> request.set_path("/api/tasks/" <> id)
    |> request.set_port(8080)
  
  case httpc.send(req) {
    Ok(resp) -> {
      case resp.status {
        204 -> Ok(Nil)
        404 -> Error("Task not found")
        _ -> Error("Failed to delete task: " <> int.to_string(resp.status))
      }
    }
    Error(_) -> Error("Network error")
  }
}

// Helper functions
fn get_host(url: String) -> String {
  url
  |> string.replace("http://", "")
  |> string.replace("https://", "")
  |> string.split(":")
  |> list.first
  |> result.unwrap("localhost")
}

fn get_path(url: String) -> String {
  case string.split(url, "/api/") {
    [_, path] -> "/api/" <> path
    _ -> "/api/tasks"
  }
}

fn parse_tasks_response(body: String) -> Result(List(Task), String) {
  use decoded <- result.try(
    json.decode(body, dynamic.dynamic)
    |> result.map_error(fn(_) { "Failed to decode JSON" })
  )
  
  use tasks_json <- result.try(
    dynamic.field("tasks", dynamic.list(dynamic.dynamic))(decoded)
    |> result.map_error(fn(_) { "Failed to get tasks field" })
  )
  
  tasks_json
  |> list.try_map(parse_task_from_dynamic)
  |> result.map_error(fn(_) { "Failed to parse tasks" })
}

fn parse_task_response(body: String) -> Result(Task, String) {
  use decoded <- result.try(
    json.decode(body, dynamic.dynamic)
    |> result.map_error(fn(_) { "Failed to decode JSON" })
  )
  
  parse_task_from_dynamic(decoded)
  |> result.map_error(fn(_) { "Failed to parse task" })
}

fn parse_task_from_dynamic(data: dynamic.Dynamic) -> Result(Task, Nil) {
  use id <- result.try(dynamic.field("id", dynamic.string)(data))
  use title <- result.try(dynamic.field("title", dynamic.string)(data))
  use status <- result.try(dynamic.field("status", dynamic.string)(data))
  use priority <- result.try(dynamic.field("priority", dynamic.string)(data))
  use created_at <- result.try(dynamic.field("created_at", dynamic.float)(data))
  use updated_at <- result.try(dynamic.field("updated_at", dynamic.float)(data))
  
  let description = dynamic.field("description", dynamic.string)(data)
    |> result.map(Some)
    |> result.unwrap(None)
  
  let tags = dynamic.field("tags", dynamic.list(dynamic.string))(data)
    |> result.unwrap([])
  
  let assigned_to = dynamic.field("assigned_to", dynamic.string)(data)
    |> result.map(Some)
    |> result.unwrap(None)
  
  Ok(Task(
    id: id,
    title: title,
    description: description,
    status: status,
    priority: priority,
    tags: tags,
    assigned_to: assigned_to,
    created_at: created_at,
    updated_at: updated_at
  ))
}

// Demo function
pub fn run_demo() {
  io.println("╔════════════════════════════════════════════════╗")
  io.println("║       Gleam Task Management REST Client        ║")
  io.println("║            Testing API Operations              ║")
  io.println("╚════════════════════════════════════════════════╝")
  io.println("")
  
  let client = new("http://localhost:8080")
  
  // 1. List all tasks
  io.println("1. Listing all tasks...")
  case list_tasks(client, None, None) {
    Ok(tasks) -> {
      io.println("   Found " <> int.to_string(list.length(tasks)) <> " tasks")
      list.each(tasks, fn(task) {
        io.println("   - [" <> task.id <> "] " <> task.title <> " (" <> task.status <> ")")
      })
    }
    Error(err) -> io.println("   Error: " <> err)
  }
  
  // 2. Create a new task
  io.println("\n2. Creating a new task...")
  case create_task(
    client,
    "Learn Gleam's actor model",
    Some("Master OTP patterns and concurrent programming"),
    "high",
    ["gleam", "concurrency", "beam"],
    Some("gleam-team")
  ) {
    Ok(task) -> {
      io.println("   Created task: " <> task.title)
      io.println("   ID: " <> task.id)
      io.println("   Priority: " <> task.priority)
      io.println("   Tags: " <> string.join(task.tags, ", "))
      
      // 3. Get task details
      io.println("\n3. Getting task details...")
      case get_task(client, task.id) {
        Ok(detail) -> {
          io.println("   Title: " <> detail.title)
          case detail.description {
            Some(desc) -> io.println("   Description: " <> desc)
            None -> Nil
          }
          io.println("   Status: " <> detail.status)
          case detail.assigned_to {
            Some(assignee) -> io.println("   Assigned to: " <> assignee)
            None -> Nil
          }
        }
        Error(err) -> io.println("   Error: " <> err)
      }
      
      // 4. Update task status
      io.println("\n4. Updating task status to 'in-progress'...")
      case update_task_status(client, task.id, "in-progress") {
        Ok(updated) -> {
          io.println("   Updated status to: " <> updated.status)
        }
        Error(err) -> io.println("   Error: " <> err)
      }
      
      // 5. Update task details
      io.println("\n5. Updating task details...")
      case update_task(
        client,
        task.id,
        Some("Master Gleam's type system"),
        None,
        None,
        Some("urgent"),
        None,
        None
      ) {
        Ok(updated) -> {
          io.println("   Updated title: " <> updated.title)
          io.println("   Updated priority: " <> updated.priority)
        }
        Error(err) -> io.println("   Error: " <> err)
      }
      
      // 6. Filter tasks by status
      io.println("\n6. Filtering tasks by status...")
      case list_tasks(client, Some("in-progress"), None) {
        Ok(tasks) -> {
          io.println("   Found " <> int.to_string(list.length(tasks)) <> " in-progress tasks")
          list.each(tasks, fn(t) {
            io.println("   - " <> t.title)
          })
        }
        Error(err) -> io.println("   Error: " <> err)
      }
      
      // 7. Delete the task
      io.println("\n7. Deleting the task...")
      case delete_task(client, task.id) {
        Ok(_) -> io.println("   Task deleted successfully")
        Error(err) -> io.println("   Error: " <> err)
      }
      
      // 8. Verify deletion
      io.println("\n8. Verifying deletion...")
      case get_task(client, task.id) {
        Ok(_) -> io.println("   Error: Task still exists")
        Error(_) -> io.println("   Task not found (as expected)")
      }
    }
    Error(err) -> io.println("   Error: " <> err)
  }
  
  io.println("\n✅ Demo completed successfully!")
}

pub fn main() {
  run_demo()
}