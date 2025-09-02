(* Initialize the store with sample data *)
let () = 
  Lwt_main.run (Store.TaskStore.init ())