type task_status = 
  | Pending 
  | InProgress 
  | Completed 
  | Cancelled
  [@@deriving yojson]

type task_priority = 
  | Low 
  | Medium 
  | High 
  | Urgent
  [@@deriving yojson]

type task = {
  id: string;
  title: string;
  description: string option;
  status: task_status;
  priority: task_priority;
  tags: string list;
  assigned_to: string option;
  created_at: float;
  updated_at: float;
} [@@deriving yojson]

type create_task_request = {
  title: string;
  description: string option;
  priority: task_priority option;
  tags: string list option;
  assigned_to: string option;
} [@@deriving yojson]

type update_task_request = {
  title: string option;
  description: string option;
  status: task_status option;
  priority: task_priority option;
  tags: string list option;
  assigned_to: string option;
} [@@deriving yojson]

type update_status_request = {
  status: task_status;
} [@@deriving yojson]

type task_list_response = {
  tasks: task list;
  total_count: int;
} [@@deriving yojson]

type error_response = {
  error: string;
} [@@deriving yojson]

let status_to_string = function
  | Pending -> "pending"
  | InProgress -> "in-progress"
  | Completed -> "completed"
  | Cancelled -> "cancelled"

let status_of_string = function
  | "pending" -> Some Pending
  | "in-progress" | "in_progress" -> Some InProgress
  | "completed" -> Some Completed
  | "cancelled" -> Some Cancelled
  | _ -> None

let priority_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Urgent -> "urgent"

let priority_of_string = function
  | "low" -> Some Low
  | "medium" -> Some Medium
  | "high" -> Some High
  | "urgent" -> Some Urgent
  | _ -> None