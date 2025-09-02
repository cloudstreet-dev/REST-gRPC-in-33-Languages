import gleam/erlang/process
import gleam/otp/actor
import gleam/option.{type Option, None, Some}
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import gleam/int
import gleam/result
import gleam/io
import gleam/erlang
import types.{
  type Task, type TaskStatus, type TaskPriority, type CreateTaskRequest,
  type UpdateTaskRequest,
  Task, Pending
}

// Messages for the actor
pub type Message {
  ListTasks(
    reply_with: process.Subject(List(Task)),
    status_filter: Option(TaskStatus),
    assigned_filter: Option(String),
  )
  GetTask(reply_with: process.Subject(Option(Task)), id: String)
  CreateTask(reply_with: process.Subject(Task), request: CreateTaskRequest)
  UpdateTask(
    reply_with: process.Subject(Option(Task)),
    id: String,
    request: UpdateTaskRequest,
  )
  UpdateTaskStatus(
    reply_with: process.Subject(Option(Task)),
    id: String,
    status: TaskStatus,
  )
  DeleteTask(reply_with: process.Subject(Bool), id: String)
}

// State for the actor
type State {
  State(tasks: Dict(String, Task), counter: Int)
}

// Start the task store actor
pub fn start() -> Result(process.Subject(Message), actor.StartError) {
  actor.start(State(tasks: dict.new(), counter: 0), handle_message)
}

// Handle incoming messages
fn handle_message(message: Message, state: State) -> actor.Next(Message, State) {
  case message {
    ListTasks(reply_with, status_filter, assigned_filter) -> {
      let all_tasks = dict.values(state.tasks)
      
      let filtered_tasks = all_tasks
        |> filter_by_status(status_filter)
        |> filter_by_assigned(assigned_filter)
      
      process.send(reply_with, filtered_tasks)
      actor.continue(state)
    }
    
    GetTask(reply_with, id) -> {
      let task = dict.get(state.tasks, id) |> option.from_result
      process.send(reply_with, task)
      actor.continue(state)
    }
    
    CreateTask(reply_with, request) -> {
      let new_counter = state.counter + 1
      let task_id = "task_" <> int.to_string(new_counter)
      let now = erlang.system_time(erlang.Millisecond) |> int.to_float
      
      let task = Task(
        id: task_id,
        title: request.title,
        description: request.description,
        status: Pending,
        priority: request.priority,
        tags: request.tags,
        assigned_to: request.assigned_to,
        created_at: now /. 1000.0,
        updated_at: now /. 1000.0,
      )
      
      let new_tasks = dict.insert(state.tasks, task_id, task)
      process.send(reply_with, task)
      actor.continue(State(tasks: new_tasks, counter: new_counter))
    }
    
    UpdateTask(reply_with, id, request) -> {
      case dict.get(state.tasks, id) {
        Ok(task) -> {
          let now = erlang.system_time(erlang.Millisecond) |> int.to_float
          
          let updated_task = Task(
            ..task,
            title: option.unwrap(request.title, task.title),
            description: case request.description {
              Some(desc) -> Some(desc)
              None -> task.description
            },
            status: option.unwrap(request.status, task.status),
            priority: option.unwrap(request.priority, task.priority),
            tags: option.unwrap(request.tags, task.tags),
            assigned_to: case request.assigned_to {
              Some(assignee) -> Some(assignee)
              None -> task.assigned_to
            },
            updated_at: now /. 1000.0,
          )
          
          let new_tasks = dict.insert(state.tasks, id, updated_task)
          process.send(reply_with, Some(updated_task))
          actor.continue(State(..state, tasks: new_tasks))
        }
        Error(_) -> {
          process.send(reply_with, None)
          actor.continue(state)
        }
      }
    }
    
    UpdateTaskStatus(reply_with, id, status) -> {
      case dict.get(state.tasks, id) {
        Ok(task) -> {
          let now = erlang.system_time(erlang.Millisecond) |> int.to_float
          
          let updated_task = Task(
            ..task,
            status: status,
            updated_at: now /. 1000.0,
          )
          
          let new_tasks = dict.insert(state.tasks, id, updated_task)
          process.send(reply_with, Some(updated_task))
          actor.continue(State(..state, tasks: new_tasks))
        }
        Error(_) -> {
          process.send(reply_with, None)
          actor.continue(state)
        }
      }
    }
    
    DeleteTask(reply_with, id) -> {
      case dict.get(state.tasks, id) {
        Ok(_) -> {
          let new_tasks = dict.delete(state.tasks, id)
          process.send(reply_with, True)
          actor.continue(State(..state, tasks: new_tasks))
        }
        Error(_) -> {
          process.send(reply_with, False)
          actor.continue(state)
        }
      }
    }
  }
}

// Helper functions for filtering
fn filter_by_status(
  tasks: List(Task),
  status_filter: Option(TaskStatus)
) -> List(Task) {
  case status_filter {
    Some(status) -> list.filter(tasks, fn(task) { task.status == status })
    None -> tasks
  }
}

fn filter_by_assigned(
  tasks: List(Task),
  assigned_filter: Option(String)
) -> List(Task) {
  case assigned_filter {
    Some(assignee) -> 
      list.filter(tasks, fn(task) {
        case task.assigned_to {
          Some(a) -> a == assignee
          None -> False
        }
      })
    None -> tasks
  }
}

// Public API functions that send messages to the actor
pub fn list_tasks(
  store: process.Subject(Message),
  status_filter: Option(TaskStatus),
  assigned_filter: Option(String)
) -> List(Task) {
  process.call(
    store,
    fn(reply) { ListTasks(reply, status_filter, assigned_filter) },
    1000
  )
}

pub fn get_task(store: process.Subject(Message), id: String) -> Option(Task) {
  process.call(store, fn(reply) { GetTask(reply, id) }, 1000)
}

pub fn create_task(
  store: process.Subject(Message),
  request: CreateTaskRequest
) -> Task {
  process.call(store, fn(reply) { CreateTask(reply, request) }, 1000)
}

pub fn update_task(
  store: process.Subject(Message),
  id: String,
  request: UpdateTaskRequest
) -> Option(Task) {
  process.call(store, fn(reply) { UpdateTask(reply, id, request) }, 1000)
}

pub fn update_task_status(
  store: process.Subject(Message),
  id: String,
  status: TaskStatus
) -> Option(Task) {
  process.call(store, fn(reply) { UpdateTaskStatus(reply, id, status) }, 1000)
}

pub fn delete_task(store: process.Subject(Message), id: String) -> Bool {
  process.call(store, fn(reply) { DeleteTask(reply, id) }, 1000)
}