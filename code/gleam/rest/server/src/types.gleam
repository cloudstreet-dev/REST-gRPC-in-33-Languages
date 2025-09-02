import gleam/option.{type Option}

// Task status enum
pub type TaskStatus {
  Pending
  InProgress
  Completed
  Cancelled
}

// Task priority enum
pub type TaskPriority {
  Low
  Medium
  High
  Urgent
}

// Main Task type
pub type Task {
  Task(
    id: String,
    title: String,
    description: Option(String),
    status: TaskStatus,
    priority: TaskPriority,
    tags: List(String),
    assigned_to: Option(String),
    created_at: Float,
    updated_at: Float,
  )
}

// Request types
pub type CreateTaskRequest {
  CreateTaskRequest(
    title: String,
    description: Option(String),
    priority: TaskPriority,
    tags: List(String),
    assigned_to: Option(String),
  )
}

pub type UpdateTaskRequest {
  UpdateTaskRequest(
    title: Option(String),
    description: Option(String),
    status: Option(TaskStatus),
    priority: Option(TaskPriority),
    tags: Option(List(String)),
    assigned_to: Option(String),
  )
}