open Handlers

let all = [
  Dream.get "/health" health_handler;
  Dream.get "/api/tasks" list_tasks_handler;
  Dream.get "/api/tasks/:id" get_task_handler;
  Dream.post "/api/tasks" create_task_handler;
  Dream.put "/api/tasks/:id" update_task_handler;
  Dream.patch "/api/tasks/:id/status" update_task_status_handler;
  Dream.delete "/api/tasks/:id" delete_task_handler;
]