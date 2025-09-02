open Types
open Lwt.Syntax
open Cohttp_lwt_unix

type t = {
  base_url: string;
}

let create base_url = { base_url }

let make_request client meth path ?body () =
  let uri = Uri.of_string (client.base_url ^ path) in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body_string = 
    match body with
    | None -> None
    | Some json -> Some (Yojson.Safe.to_string json)
  in
  let body_cohttp = 
    match body_string with
    | None -> Cohttp_lwt.Body.empty
    | Some s -> Cohttp_lwt.Body.of_string s
  in
  Client.call ~headers ~body:body_cohttp meth uri

let handle_response (resp, body) =
  let code = Cohttp.Response.status resp in
  let* body_string = Cohttp_lwt.Body.to_string body in
  match Cohttp.Code.code_of_status code with
  | 200 | 201 -> Lwt.return (Ok body_string)
  | 204 -> Lwt.return (Ok "")
  | 404 -> Lwt.return (Error "Not found")
  | _ -> Lwt.return (Error body_string)

let list_tasks client ?status ?assigned_to () =
  let params = [] in
  let params = 
    match status with
    | None -> params
    | Some s -> ("status", status_to_string s) :: params
  in
  let params = 
    match assigned_to with
    | None -> params
    | Some a -> ("assigned_to", a) :: params
  in
  let path = 
    if params = [] then "/api/tasks"
    else "/api/tasks?" ^ Uri.encoded_of_query params
  in
  let* response = make_request client `GET path () in
  let* result = handle_response response in
  match result with
  | Error msg -> Lwt.return (Error msg)
  | Ok body ->
    match Yojson.Safe.from_string body |> task_list_response_of_yojson with
    | Ok resp -> Lwt.return (Ok resp.tasks)
    | Error msg -> Lwt.return (Error msg)

let get_task client id =
  let path = Printf.sprintf "/api/tasks/%s" id in
  let* response = make_request client `GET path () in
  let* result = handle_response response in
  match result with
  | Error msg -> Lwt.return (Error msg)
  | Ok body ->
    match Yojson.Safe.from_string body |> task_of_yojson with
    | Ok task -> Lwt.return (Ok task)
    | Error msg -> Lwt.return (Error msg)

let create_task client request =
  let body = create_task_request_to_yojson request in
  let* response = make_request client `POST "/api/tasks" ~body () in
  let* result = handle_response response in
  match result with
  | Error msg -> Lwt.return (Error msg)
  | Ok body ->
    match Yojson.Safe.from_string body |> task_of_yojson with
    | Ok task -> Lwt.return (Ok task)
    | Error msg -> Lwt.return (Error msg)

let update_task client id updates =
  let path = Printf.sprintf "/api/tasks/%s" id in
  let body = update_task_request_to_yojson updates in
  let* response = make_request client `PUT path ~body () in
  let* result = handle_response response in
  match result with
  | Error msg -> Lwt.return (Error msg)
  | Ok body ->
    match Yojson.Safe.from_string body |> task_of_yojson with
    | Ok task -> Lwt.return (Ok task)
    | Error msg -> Lwt.return (Error msg)

let update_task_status client id status =
  let path = Printf.sprintf "/api/tasks/%s/status" id in
  let body = update_status_request_to_yojson { status } in
  let* response = make_request client `PATCH path ~body () in
  let* result = handle_response response in
  match result with
  | Error msg -> Lwt.return (Error msg)
  | Ok body ->
    match Yojson.Safe.from_string body |> task_of_yojson with
    | Ok task -> Lwt.return (Ok task)
    | Error msg -> Lwt.return (Error msg)

let delete_task client id =
  let path = Printf.sprintf "/api/tasks/%s" id in
  let* response = make_request client `DELETE path () in
  let* result = handle_response response in
  match result with
  | Error msg -> Lwt.return (Error msg)
  | Ok _ -> Lwt.return (Ok ())