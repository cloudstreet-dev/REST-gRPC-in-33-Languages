defmodule TaskGrpcServer.Service do
  use GRPC.Server, service: Tasks.V1.TaskService.Service

  alias Tasks.V1.{
    Task,
    TaskStatus,
    TaskPriority,
    ListTasksRequest,
    ListTasksResponse,
    GetTaskRequest,
    CreateTaskRequest,
    UpdateTaskRequest,
    DeleteTaskRequest,
    WatchTasksRequest,
    TaskEvent,
    EventType
  }
  alias TaskGrpcServer.Repository
  alias Google.Protobuf.{Timestamp, Empty}

  @spec list_tasks(ListTasksRequest.t(), GRPC.Server.Stream.t()) :: any()
  def list_tasks(request, stream) do
    tasks = Repository.list_tasks(
      status: request.status,
      assigned_to: request.assigned_to,
      tags: request.tags,
      page_size: request.page_size || 20,
      page_token: request.page_token || "",
      sort_order: request.sort_order
    )

    # Stream each task individually
    Enum.each(tasks, fn task ->
      GRPC.Server.send_reply(stream, task)
    end)
  end

  @spec get_task(GetTaskRequest.t(), GRPC.Server.Stream.t()) :: Task.t()
  def get_task(%GetTaskRequest{id: id}, _stream) do
    case Repository.get_task(id) do
      nil ->
        raise GRPC.RPCError, status: :not_found, message: "Task not found"
      task ->
        task
    end
  end

  @spec create_task(CreateTaskRequest.t(), GRPC.Server.Stream.t()) :: Task.t()
  def create_task(%CreateTaskRequest{task: task_data}, _stream) do
    if is_nil(task_data.title) || task_data.title == "" do
      raise GRPC.RPCError, status: :invalid_argument, message: "Title is required"
    end

    Repository.create_task(task_data)
  end

  @spec update_task(UpdateTaskRequest.t(), GRPC.Server.Stream.t()) :: Task.t()
  def update_task(%UpdateTaskRequest{task: task_data, update_mask: mask}, _stream) do
    if is_nil(task_data.id) || task_data.id == "" do
      raise GRPC.RPCError, status: :invalid_argument, message: "Task ID is required"
    end

    case Repository.update_task(task_data.id, task_data, mask) do
      nil ->
        raise GRPC.RPCError, status: :not_found, message: "Task not found"
      task ->
        task
    end
  end

  @spec delete_task(DeleteTaskRequest.t(), GRPC.Server.Stream.t()) :: Empty.t()
  def delete_task(%DeleteTaskRequest{id: id}, _stream) do
    case Repository.delete_task(id) do
      :ok ->
        %Empty{}
      :not_found ->
        raise GRPC.RPCError, status: :not_found, message: "Task not found"
    end
  end

  @spec watch_tasks(Enumerable.t(), GRPC.Server.Stream.t()) :: any()
  def watch_tasks(request_stream, stream) do
    # Start a process to handle the bidirectional stream
    Task.start_link(fn ->
      # Subscribe to repository events
      Repository.subscribe(self())

      # Handle incoming requests
      request_task = Task.async(fn ->
        Enum.each(request_stream, fn request ->
          process_watch_request(request)
        end)
      end)

      # Handle outgoing events
      event_loop(stream)
      
      # Cleanup
      Task.await(request_task)
      Repository.unsubscribe(self())
    end)
  end

  defp process_watch_request(%WatchTasksRequest{} = request) do
    # Store watch filters in process state
    filters = %{
      task_ids: request.task_ids,
      watch_all: request.watch_all,
      assigned_to: request.assigned_to
    }
    Process.put(:watch_filters, filters)
  end

  defp event_loop(stream) do
    receive do
      {:task_event, event_type, task} ->
        filters = Process.get(:watch_filters, %{watch_all: true})
        
        if should_send_event?(task, filters) do
          event = %TaskEvent{
            event_type: event_type,
            task: task,
            timestamp: current_timestamp()
          }
          GRPC.Server.send_reply(stream, event)
        end
        
        event_loop(stream)
      
      :stop ->
        :ok
        
      _ ->
        event_loop(stream)
    end
  end

  defp should_send_event?(task, filters) do
    cond do
      filters[:watch_all] == true -> true
      task.id in (filters[:task_ids] || []) -> true
      task.assigned_to == filters[:assigned_to] -> true
      true -> false
    end
  end

  defp current_timestamp do
    now = DateTime.utc_now()
    %Timestamp{
      seconds: DateTime.to_unix(now),
      nanos: 0
    }
  end
end