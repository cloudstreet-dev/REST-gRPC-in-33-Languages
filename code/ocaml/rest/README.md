# OCaml REST API Implementation

This directory contains a REST API implementation in OCaml using the Dream web framework, demonstrating functional programming with a powerful type system.

## Features

- **Type-safe API** with algebraic data types
- **Dream framework** for modern web development
- **Lwt** for asynchronous programming
- **PPX derivers** for automatic JSON serialization
- **Immutable data structures** by default
- **Pattern matching** for control flow

## Prerequisites

- OCaml 4.14 or higher
- opam (OCaml package manager)
- dune (build system)

Install OCaml and opam:
```bash
# macOS
brew install opam

# Ubuntu/Debian
apt-get install opam

# Initialize opam
opam init
eval $(opam env)

# Install OCaml
opam switch create 5.1.0
eval $(opam env)
```

## Server

The server implements a complete REST API using Dream framework.

### Running the Server

```bash
cd server
opam install . --deps-only -y
dune exec task_server
```

Or use the provided script:
```bash
./run-server.sh
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status
- `assigned_to` - Filter by assignee

## Client

The client provides a comprehensive SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
opam install . --deps-only -y
dune exec task_client
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```ocaml
open Task_client_lib
open Lwt.Syntax

let main () =
  let client = Client.create "http://localhost:8080" in
  
  (* List all tasks *)
  let* tasks = Client.list_tasks client in
  
  (* Create a task *)
  let new_task = Types.{
    title = "Learn OCaml";
    description = Some "Master functional programming";
    priority = Some High;
    tags = Some ["ocaml"; "learning"];
    assigned_to = Some "developer";
  } in
  let* task = Client.create_task client new_task in
  
  (* Update task status *)
  let* updated = Client.update_task_status client task.id InProgress in
  
  (* Delete task *)
  let* () = Client.delete_task client task.id in
  
  Lwt.return_unit

let () = Lwt_main.run (main ())
```

## Architecture

### Type System

```ocaml
(* Algebraic Data Types *)
type task_status = 
  | Pending 
  | InProgress 
  | Completed 
  | Cancelled

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
}

(* Pattern Matching *)
let status_to_string = function
  | Pending -> "pending"
  | InProgress -> "in-progress"
  | Completed -> "completed"
  | Cancelled -> "cancelled"
```

### Asynchronous Programming with Lwt

```ocaml
open Lwt.Syntax

let fetch_and_process_task id =
  let* task = get_task id in
  let* processed = process_task task in
  let* () = save_task processed in
  Lwt.return processed

(* Concurrent operations *)
let process_tasks ids =
  Lwt_list.map_p fetch_and_process_task ids
```

### Modules and Functors

```ocaml
module type STORE = sig
  type t
  val create : unit -> t
  val get : t -> string -> task option
  val put : t -> string -> task -> unit
  val delete : t -> string -> bool
end

module Make_API (Store : STORE) = struct
  let store = Store.create ()
  
  let get_task id =
    match Store.get store id with
    | Some task -> Dream.json (task_to_json task)
    | None -> Dream.empty `Not_Found
end
```

## OCaml Features Demonstrated

### Option Types

```ocaml
(* Safe null handling *)
type task = {
  description: string option;
  assigned_to: string option;
}

(* Using options *)
let get_description task =
  match task.description with
  | Some desc -> desc
  | None -> "No description"

(* Option module functions *)
let priority = Option.value request.priority ~default:Medium
```

### Result Types

```ocaml
(* Error handling without exceptions *)
type ('a, 'e) result = 
  | Ok of 'a
  | Error of 'e

let parse_json json =
  match Yojson.Safe.from_string json with
  | exception _ -> Error "Invalid JSON"
  | json -> 
    match task_of_yojson json with
    | Ok task -> Ok task
    | Error msg -> Error msg
```

### Polymorphic Variants

```ocaml
(* Flexible type definitions *)
type http_status = [
  | `OK
  | `Created
  | `Bad_Request
  | `Not_Found
  | `Internal_Server_Error
]

let handle_request = function
  | `GET -> handle_get ()
  | `POST -> handle_post ()
  | `DELETE -> handle_delete ()
  | _ -> Dream.empty `Method_Not_Allowed
```

### GADTs (Generalized Algebraic Data Types)

```ocaml
type _ query =
  | All : task list query
  | ById : string -> task option query
  | ByStatus : task_status -> task list query

let execute : type a. a query -> a Lwt.t = function
  | All -> TaskStore.list_tasks ()
  | ById id -> TaskStore.get_task id
  | ByStatus status -> TaskStore.list_tasks ~status ()
```

## Testing

### Unit Tests with Alcotest

```ocaml
open Alcotest

let test_task_creation () =
  let task = create_task "Test" in
  check string "title" "Test" task.title;
  check bool "is pending" true (task.status = Pending)

let test_suite = [
  "Task creation", `Quick, test_task_creation;
  "Task update", `Quick, test_task_update;
  "Task deletion", `Quick, test_task_deletion;
]

let () =
  run "Task API Tests" [
    "tasks", test_suite;
  ]
```

### Property-Based Testing with QCheck

```ocaml
open QCheck

let test_id_uniqueness =
  Test.make ~count:1000
    ~name:"task IDs are unique"
    (list string)
    (fun titles ->
      let tasks = List.map create_task titles in
      let ids = List.map (fun t -> t.id) tasks in
      List.length ids = List.length (List.sort_uniq compare ids))

let () = QCheck_runner.run_tests [test_id_uniqueness]
```

## Performance Optimization

### Tail Recursion

```ocaml
(* Tail-recursive list processing *)
let rec filter_tasks acc = function
  | [] -> List.rev acc
  | task :: rest ->
    if task.status = InProgress then
      filter_tasks (task :: acc) rest
    else
      filter_tasks acc rest

(* Using List.fold_left for efficiency *)
let sum_priorities tasks =
  List.fold_left (fun acc task ->
    acc + priority_to_int task.priority
  ) 0 tasks
```

### Lazy Evaluation

```ocaml
(* Lazy sequences for large datasets *)
type 'a stream = Cons of 'a * 'a stream Lazy.t | Nil

let rec take n stream =
  match n, stream with
  | 0, _ | _, Nil -> []
  | n, Cons (x, lazy xs) -> x :: take (n - 1) xs

(* Memoization *)
let memo f =
  let table = Hashtbl.create 17 in
  fun x ->
    try Hashtbl.find table x
    with Not_found ->
      let y = f x in
      Hashtbl.add table x y;
      y
```

## Best Practices

1. **Use the type system**: Leverage OCaml's powerful type system
2. **Prefer immutability**: Use immutable data structures
3. **Handle errors explicitly**: Use Result and Option types
4. **Pattern match exhaustively**: Cover all cases
5. **Use modules for abstraction**: Hide implementation details
6. **Leverage PPX**: Use preprocessor extensions for boilerplate
7. **Test with properties**: Use property-based testing

## Docker Support

```dockerfile
FROM ocaml/opam:alpine-ocaml-5.1

RUN sudo apk add --no-cache git build-base

WORKDIR /app

# Copy opam files first for caching
COPY server/*.opam .
RUN opam install . --deps-only

COPY server/ .
RUN opam exec -- dune build

EXPOSE 8080
CMD ["opam", "exec", "--", "dune", "exec", "task_server"]
```

## Dependencies

### Server
- `dream` - Modern web framework
- `lwt` - Asynchronous programming
- `yojson` - JSON handling
- `ppx_deriving_yojson` - JSON serialization
- `uuidm` - UUID generation

### Client
- `cohttp-lwt-unix` - HTTP client
- `lwt` - Asynchronous programming
- `yojson` - JSON handling
- `ppx_deriving_yojson` - JSON serialization