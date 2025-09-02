defmodule Tasks.V1.TaskService.Service do
  @moduledoc false
  use GRPC.Service, name: "tasks.v1.TaskService", protoc_gen_elixir_version: "0.11.0"

  rpc :ListTasks, Tasks.V1.ListTasksRequest, stream(Tasks.V1.Task)
  rpc :GetTask, Tasks.V1.GetTaskRequest, Tasks.V1.Task
  rpc :CreateTask, Tasks.V1.CreateTaskRequest, Tasks.V1.Task
  rpc :UpdateTask, Tasks.V1.UpdateTaskRequest, Tasks.V1.Task
  rpc :DeleteTask, Tasks.V1.DeleteTaskRequest, Google.Protobuf.Empty
  rpc :WatchTasks, stream(Tasks.V1.WatchTasksRequest), stream(Tasks.V1.TaskEvent)
end

defmodule Tasks.V1.TaskService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Tasks.V1.TaskService.Service
end