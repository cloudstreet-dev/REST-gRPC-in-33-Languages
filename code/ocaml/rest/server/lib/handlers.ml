open Types
open Store
open Lwt.Syntax

let json_response ?(status=`OK) json =
  Dream.json ~status (Yojson.Safe.to_string json)

let error_response status message =
  json_response ~status (error_response_to_yojson { error = message })

let health_handler _request =
  Dream.json {|{
    "status": "healthy",
    "service": "task-api",
    "version": "1.0.0"
  }|}

let list_tasks_handler request =
  let status = 
    Dream.query request "status"
    |> Option.bind status_of_string
  in
  let assigned_to = Dream.query request "assigned_to" in
  
  let* tasks = TaskStore.list_tasks ?status ?assigned_to () in
  let response = { tasks; total_count = List.length tasks } in
  json_response (task_list_response_to_yojson response)

let get_task_handler request =
  let id = Dream.param request "id" in
  let* task_opt = TaskStore.get_task id in
  match task_opt with
  | Some task -> json_response (task_to_yojson task)
  | None -> error_response `Not_Found "Task not found"

let create_task_handler request =
  let* body = Dream.body request in
  match Yojson.Safe.from_string body |> create_task_request_of_yojson with
  | Ok req ->
    let* task = TaskStore.create_task req in
    json_response ~status:`Created (task_to_yojson task)
  | Error msg -> error_response `Bad_Request msg

let update_task_handler request =
  let id = Dream.param request "id" in
  let* body = Dream.body request in
  match Yojson.Safe.from_string body |> update_task_request_of_yojson with
  | Ok updates ->
    let* task_opt = TaskStore.update_task id updates in
    (match task_opt with
     | Some task -> json_response (task_to_yojson task)
     | None -> error_response `Not_Found "Task not found")
  | Error msg -> error_response `Bad_Request msg

let update_task_status_handler request =
  let id = Dream.param request "id" in
  let* body = Dream.body request in
  match Yojson.Safe.from_string body |> update_status_request_of_yojson with
  | Ok req ->
    let* task_opt = TaskStore.update_task_status id req.status in
    (match task_opt with
     | Some task -> json_response (task_to_yojson task)
     | None -> error_response `Not_Found "Task not found")
  | Error msg -> error_response `Bad_Request msg

let delete_task_handler request =
  let id = Dream.param request "id" in
  let* existed = TaskStore.delete_task id in
  if existed then
    Dream.empty `No_Content
  else
    error_response `Not_Found "Task not found"