open Types

module TaskStore = struct
  type t = {
    mutable tasks: (string, task) Hashtbl.t;
    mutable counter: int;
    mutex: Lwt_mutex.t;
  }

  let create () = {
    tasks = Hashtbl.create 100;
    counter = 0;
    mutex = Lwt_mutex.create ();
  }

  let instance = create ()

  let generate_id store =
    store.counter <- store.counter + 1;
    Printf.sprintf "task-%d" store.counter

  let create_task request =
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock instance.mutex in
    let id = generate_id instance in
    let now = Unix.time () in
    let task = {
      id;
      title = request.title;
      description = request.description;
      status = Pending;
      priority = Option.value request.priority ~default:Medium;
      tags = Option.value request.tags ~default:[];
      assigned_to = request.assigned_to;
      created_at = now;
      updated_at = now;
    } in
    Hashtbl.add instance.tasks id task;
    Lwt_mutex.unlock instance.mutex;
    Lwt.return task

  let get_task id =
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock instance.mutex in
    let result = Hashtbl.find_opt instance.tasks id in
    Lwt_mutex.unlock instance.mutex;
    Lwt.return result

  let update_task id updates =
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock instance.mutex in
    let result = 
      match Hashtbl.find_opt instance.tasks id with
      | None -> None
      | Some task ->
        let updated_task = {
          task with
          title = Option.value updates.title ~default:task.title;
          description = 
            (match updates.description with
             | Some desc -> desc
             | None -> task.description);
          status = Option.value updates.status ~default:task.status;
          priority = Option.value updates.priority ~default:task.priority;
          tags = Option.value updates.tags ~default:task.tags;
          assigned_to = 
            (match updates.assigned_to with
             | Some assignee -> assignee
             | None -> task.assigned_to);
          updated_at = Unix.time ();
        } in
        Hashtbl.replace instance.tasks id updated_task;
        Some updated_task
    in
    Lwt_mutex.unlock instance.mutex;
    Lwt.return result

  let update_task_status id status =
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock instance.mutex in
    let result = 
      match Hashtbl.find_opt instance.tasks id with
      | None -> None
      | Some task ->
        let updated_task = {
          task with
          status;
          updated_at = Unix.time ();
        } in
        Hashtbl.replace instance.tasks id updated_task;
        Some updated_task
    in
    Lwt_mutex.unlock instance.mutex;
    Lwt.return result

  let delete_task id =
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock instance.mutex in
    let existed = Hashtbl.mem instance.tasks id in
    if existed then Hashtbl.remove instance.tasks id;
    Lwt_mutex.unlock instance.mutex;
    Lwt.return existed

  let list_tasks ?status ?assigned_to () =
    let open Lwt.Syntax in
    let* () = Lwt_mutex.lock instance.mutex in
    let all_tasks = Hashtbl.fold (fun _ task acc -> task :: acc) instance.tasks [] in
    let filtered_tasks = 
      all_tasks
      |> List.filter (fun task ->
        let status_match = 
          match status with
          | None -> true
          | Some s -> task.status = s
        in
        let assigned_match =
          match assigned_to with
          | None -> true
          | Some a -> task.assigned_to = Some a
        in
        status_match && assigned_match
      )
      |> List.sort (fun t1 t2 -> compare t2.created_at t1.created_at)
    in
    Lwt_mutex.unlock instance.mutex;
    Lwt.return filtered_tasks

  let init () =
    let open Lwt.Syntax in
    (* Add sample tasks *)
    let* _ = create_task {
      title = "Learn OCaml";
      description = Some "Master functional programming with OCaml";
      priority = Some High;
      tags = Some ["ocaml"; "learning"; "functional"];
      assigned_to = Some "developer";
    } in
    let* _ = create_task {
      title = "Build REST API";
      description = Some "Create REST API with Dream framework";
      priority = Some Urgent;
      tags = Some ["ocaml"; "api"; "rest"; "dream"];
      assigned_to = Some "backend-team";
    } in
    let* _ = create_task {
      title = "Write Tests";
      description = Some "Add unit tests with Alcotest";
      priority = Some Medium;
      tags = Some ["testing"; "quality"];
      assigned_to = None;
    } in
    Lwt.return_unit
end