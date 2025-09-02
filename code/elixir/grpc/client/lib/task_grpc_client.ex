defmodule TaskGrpcClient do
  alias Tasks.V1.{
    Task,
    TaskStatus,
    TaskPriority,
    ListTasksRequest,
    GetTaskRequest,
    CreateTaskRequest,
    UpdateTaskRequest,
    DeleteTaskRequest,
    WatchTasksRequest,
    TaskService
  }
  alias GRPC.Channel

  def connect(host \\ "localhost", port \\ 50051) do
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")
    channel
  end

  def list_tasks(channel, opts \\ []) do
    request = %ListTasksRequest{
      page_size: Keyword.get(opts, :page_size, 20),
      page_token: Keyword.get(opts, :page_token, ""),
      status: Keyword.get(opts, :status),
      assigned_to: Keyword.get(opts, :assigned_to, ""),
      tags: Keyword.get(opts, :tags, [])
    }

    stream = channel
    |> TaskService.Stub.list_tasks(request)
    
    # Collect all streamed tasks
    Enum.to_list(stream)
  end

  def get_task(channel, id) do
    request = %GetTaskRequest{id: id}
    
    case TaskService.Stub.get_task(channel, request) do
      {:ok, task} -> {:ok, task}
      {:error, %GRPC.RPCError{status: :not_found}} -> {:error, :not_found}
      {:error, error} -> {:error, error}
    end
  end

  def create_task(channel, title, opts \\ []) do
    task = %Task{
      title: title,
      description: Keyword.get(opts, :description, ""),
      status: Keyword.get(opts, :status, :TASK_STATUS_PENDING),
      priority: Keyword.get(opts, :priority, :TASK_PRIORITY_MEDIUM),
      tags: Keyword.get(opts, :tags, []),
      assigned_to: Keyword.get(opts, :assigned_to, "")
    }
    
    request = %CreateTaskRequest{task: task}
    
    case TaskService.Stub.create_task(channel, request) do
      {:ok, created_task} -> {:ok, created_task}
      {:error, error} -> {:error, error}
    end
  end

  def update_task(channel, id, updates, update_mask \\ []) do
    task = struct(Task, Map.put(updates, :id, id))
    request = %UpdateTaskRequest{
      task: task,
      update_mask: update_mask
    }
    
    case TaskService.Stub.update_task(channel, request) do
      {:ok, updated_task} -> {:ok, updated_task}
      {:error, %GRPC.RPCError{status: :not_found}} -> {:error, :not_found}
      {:error, error} -> {:error, error}
    end
  end

  def delete_task(channel, id) do
    request = %DeleteTaskRequest{id: id}
    
    case TaskService.Stub.delete_task(channel, request) do
      {:ok, _} -> :ok
      {:error, %GRPC.RPCError{status: :not_found}} -> {:error, :not_found}
      {:error, error} -> {:error, error}
    end
  end

  def watch_tasks(channel, opts \\ []) do
    # Create the bidirectional stream
    stream = TaskService.Stub.watch_tasks(channel)
    
    # Send initial watch request
    request = %WatchTasksRequest{
      watch_all: Keyword.get(opts, :watch_all, true),
      task_ids: Keyword.get(opts, :task_ids, []),
      assigned_to: Keyword.get(opts, :assigned_to, "")
    }
    
    GRPC.Stub.send_request(stream, request)
    
    # Return the stream for the caller to consume events
    stream
  end
end