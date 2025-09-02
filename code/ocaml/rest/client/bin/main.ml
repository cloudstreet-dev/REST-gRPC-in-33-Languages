open Task_client_lib
open Lwt.Syntax

let print_banner () =
  print_endline "╔════════════════════════════════════════════════╗";
  print_endline "║       OCaml Task Management REST Client        ║";
  print_endline "║            Testing API Operations              ║";
  print_endline "╚════════════════════════════════════════════════╝";
  print_newline ()

let run_demo () =
  let client = Client.create "http://localhost:8080" in
  
  (* 1. List all tasks *)
  let* () = Lwt_io.printl "1. Listing all tasks..." in
  let* tasks_result = Client.list_tasks client in
  let* () = 
    match tasks_result with
    | Ok tasks ->
      let* () = Lwt_io.printlf "   Found %d tasks" (List.length tasks) in
      Lwt_list.iter_s (fun task ->
        Lwt_io.printlf "   - [%s] %s (%s)" 
          task.Types.id 
          task.Types.title 
          (Types.status_to_string task.Types.status)
      ) tasks
    | Error msg -> Lwt_io.printlf "   Error: %s" msg
  in
  
  (* 2. Create a new task *)
  let* () = Lwt_io.printl "\n2. Creating a new task..." in
  let new_task = Types.{
    title = "Learn OCaml type system";
    description = Some "Master algebraic data types and modules";
    priority = Some High;
    tags = Some ["ocaml"; "types"; "functional"];
    assigned_to = Some "ocaml-team";
  } in
  
  let* create_result = Client.create_task client new_task in
  let* task_id = 
    match create_result with
    | Ok task ->
      let* () = Lwt_io.printlf "   Created task: %s" task.Types.title in
      let* () = Lwt_io.printlf "   ID: %s" task.Types.id in
      let* () = Lwt_io.printlf "   Priority: %s" 
        (Types.priority_to_string task.Types.priority) in
      let* () = Lwt_io.printlf "   Tags: %s" 
        (String.concat ", " task.Types.tags) in
      Lwt.return (Some task.Types.id)
    | Error msg ->
      let* () = Lwt_io.printlf "   Error: %s" msg in
      Lwt.return None
  in
  
  match task_id with
  | None -> Lwt.return_unit
  | Some id ->
    (* 3. Get task details *)
    let* () = Lwt_io.printl "\n3. Getting task details..." in
    let* get_result = Client.get_task client id in
    let* () = 
      match get_result with
      | Ok task ->
        let* () = Lwt_io.printlf "   Title: %s" task.Types.title in
        let* () = 
          match task.Types.description with
          | Some desc -> Lwt_io.printlf "   Description: %s" desc
          | None -> Lwt.return_unit
        in
        let* () = Lwt_io.printlf "   Status: %s" 
          (Types.status_to_string task.Types.status) in
        match task.Types.assigned_to with
        | Some assignee -> Lwt_io.printlf "   Assigned to: %s" assignee
        | None -> Lwt.return_unit
      | Error msg -> Lwt_io.printlf "   Error: %s" msg
    in
    
    (* 4. Update task status *)
    let* () = Lwt_io.printl "\n4. Updating task status to 'in-progress'..." in
    let* status_result = Client.update_task_status client id Types.InProgress in
    let* () = 
      match status_result with
      | Ok task ->
        Lwt_io.printlf "   Updated status to: %s" 
          (Types.status_to_string task.Types.status)
      | Error msg -> Lwt_io.printlf "   Error: %s" msg
    in
    
    (* 5. Update task details *)
    let* () = Lwt_io.printl "\n5. Updating task details..." in
    let updates = Types.{
      title = Some "Master OCaml module system";
      description = None;
      status = None;
      priority = Some Urgent;
      tags = None;
      assigned_to = None;
    } in
    let* update_result = Client.update_task client id updates in
    let* () = 
      match update_result with
      | Ok task ->
        let* () = Lwt_io.printlf "   Updated title: %s" task.Types.title in
        Lwt_io.printlf "   Updated priority: %s" 
          (Types.priority_to_string task.Types.priority)
      | Error msg -> Lwt_io.printlf "   Error: %s" msg
    in
    
    (* 6. Filter tasks by status *)
    let* () = Lwt_io.printl "\n6. Filtering tasks by status..." in
    let* filter_result = Client.list_tasks client ~status:Types.InProgress in
    let* () = 
      match filter_result with
      | Ok tasks ->
        let* () = Lwt_io.printlf "   Found %d in-progress tasks" (List.length tasks) in
        Lwt_list.iter_s (fun task ->
          Lwt_io.printlf "   - %s" task.Types.title
        ) tasks
      | Error msg -> Lwt_io.printlf "   Error: %s" msg
    in
    
    (* 7. Delete the task *)
    let* () = Lwt_io.printl "\n7. Deleting the task..." in
    let* delete_result = Client.delete_task client id in
    let* () = 
      match delete_result with
      | Ok () -> Lwt_io.printl "   Task deleted successfully"
      | Error msg -> Lwt_io.printlf "   Error: %s" msg
    in
    
    (* 8. Verify deletion *)
    let* () = Lwt_io.printl "\n8. Verifying deletion..." in
    let* verify_result = Client.get_task client id in
    let* () = 
      match verify_result with
      | Ok _ -> Lwt_io.printl "   Error: Task still exists"
      | Error msg when String.exists (fun _ -> true) msg -> 
        Lwt_io.printl "   Task not found (as expected)"
    in
    
    Lwt_io.printl "\n✅ Demo completed successfully!"

let () =
  print_banner ();
  Unix.sleep 1;
  Lwt_main.run (run_demo ())