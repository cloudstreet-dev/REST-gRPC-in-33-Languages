defmodule TaskGrpcServer.Repository do
  use GenServer
  alias Tasks.V1.{Task, TaskStatus, TaskPriority}
  alias Google.Protobuf.Timestamp

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def list_tasks(filters \\ %{}) do
    GenServer.call(__MODULE__, {:list_tasks, filters})
  end

  def get_task(id) do
    GenServer.call(__MODULE__, {:get_task, id})
  end

  def create_task(task_data) do
    GenServer.call(__MODULE__, {:create_task, task_data})
  end

  def update_task(id, task_data, update_mask \\ []) do
    GenServer.call(__MODULE__, {:update_task, id, task_data, update_mask})
  end

  def delete_task(id) do
    GenServer.call(__MODULE__, {:delete_task, id})
  end

  def subscribe(pid) do
    GenServer.cast(__MODULE__, {:subscribe, pid})
  end

  def unsubscribe(pid) do
    GenServer.cast(__MODULE__, {:unsubscribe, pid})
  end

  # Server callbacks

  @impl true
  def init(_) do
    state = %{
      tasks: load_sample_data(),
      subscribers: []
    }
    {:ok, state}
  end

  @impl true
  def handle_call({:list_tasks, filters}, _from, state) do
    tasks = Map.values(state.tasks)
    |> filter_tasks(filters)
    |> sort_tasks(filters[:sort_order])
    |> paginate(filters[:page_size], filters[:page_token])
    
    {:reply, tasks, state}
  end

  @impl true
  def handle_call({:get_task, id}, _from, state) do
    {:reply, Map.get(state.tasks, id), state}
  end

  @impl true
  def handle_call({:create_task, task_data}, _from, state) do
    task = create_task_struct(task_data)
    new_tasks = Map.put(state.tasks, task.id, task)
    
    # Notify subscribers
    notify_subscribers(state.subscribers, :EVENT_TYPE_CREATED, task)
    
    {:reply, task, %{state | tasks: new_tasks}}
  end

  @impl true
  def handle_call({:update_task, id, task_data, update_mask}, _from, state) do
    case Map.get(state.tasks, id) do
      nil ->
        {:reply, nil, state}
      existing_task ->
        updated_task = apply_updates(existing_task, task_data, update_mask)
        new_tasks = Map.put(state.tasks, id, updated_task)
        
        # Notify subscribers
        notify_subscribers(state.subscribers, :EVENT_TYPE_UPDATED, updated_task)
        
        {:reply, updated_task, %{state | tasks: new_tasks}}
    end
  end

  @impl true
  def handle_call({:delete_task, id}, _from, state) do
    case Map.get(state.tasks, id) do
      nil ->
        {:reply, :not_found, state}
      task ->
        new_tasks = Map.delete(state.tasks, id)
        
        # Notify subscribers
        notify_subscribers(state.subscribers, :EVENT_TYPE_DELETED, task)
        
        {:reply, :ok, %{state | tasks: new_tasks}}
    end
  end

  @impl true
  def handle_cast({:subscribe, pid}, state) do
    {:noreply, %{state | subscribers: [pid | state.subscribers]}}
  end

  @impl true
  def handle_cast({:unsubscribe, pid}, state) do
    {:noreply, %{state | subscribers: List.delete(state.subscribers, pid)}}
  end

  # Private functions

  defp load_sample_data do
    task1 = %Task{
      id: UUID.uuid4(),
      title: "Implement Elixir gRPC server",
      description: "Build a gRPC server using grpc-elixir",
      status: :TASK_STATUS_IN_PROGRESS,
      priority: :TASK_PRIORITY_HIGH,
      tags: ["elixir", "grpc", "backend"],
      assigned_to: "backend-team",
      created_at: current_timestamp(),
      updated_at: current_timestamp()
    }
    
    task2 = %Task{
      id: UUID.uuid4(),
      title: "Add streaming support",
      description: "Implement bidirectional streaming for real-time updates",
      status: :TASK_STATUS_PENDING,
      priority: :TASK_PRIORITY_MEDIUM,
      tags: ["elixir", "streaming", "grpc"],
      assigned_to: "backend-team",
      created_at: current_timestamp(),
      updated_at: current_timestamp()
    }
    
    task3 = %Task{
      id: UUID.uuid4(),
      title: "Write property tests",
      description: "Add property-based testing with StreamData",
      status: :TASK_STATUS_PENDING,
      priority: :TASK_PRIORITY_HIGH,
      tags: ["testing", "quality"],
      assigned_to: "qa-team",
      created_at: current_timestamp(),
      updated_at: current_timestamp()
    }
    
    %{
      task1.id => task1,
      task2.id => task2,
      task3.id => task3
    }
  end

  defp create_task_struct(task_data) do
    %Task{
      id: UUID.uuid4(),
      title: task_data.title,
      description: task_data.description || "",
      status: task_data.status || :TASK_STATUS_PENDING,
      priority: task_data.priority || :TASK_PRIORITY_MEDIUM,
      tags: task_data.tags || [],
      created_by: task_data.created_by || "",
      assigned_to: task_data.assigned_to || "",
      created_at: current_timestamp(),
      updated_at: current_timestamp(),
      due_date: task_data.due_date
    }
  end

  defp apply_updates(task, updates, []) do
    # If no update mask, update all provided fields
    %{task |
      title: updates.title || task.title,
      description: updates.description || task.description,
      status: updates.status || task.status,
      priority: updates.priority || task.priority,
      tags: updates.tags || task.tags,
      assigned_to: updates.assigned_to || task.assigned_to,
      due_date: updates.due_date || task.due_date,
      updated_at: current_timestamp()
    }
  end

  defp apply_updates(task, updates, update_mask) do
    # Apply only fields specified in update mask
    Enum.reduce(update_mask, task, fn field, acc ->
      case field do
        "title" -> %{acc | title: updates.title}
        "description" -> %{acc | description: updates.description}
        "status" -> %{acc | status: updates.status}
        "priority" -> %{acc | priority: updates.priority}
        "tags" -> %{acc | tags: updates.tags}
        "assigned_to" -> %{acc | assigned_to: updates.assigned_to}
        "due_date" -> %{acc | due_date: updates.due_date}
        _ -> acc
      end
    end)
    |> Map.put(:updated_at, current_timestamp())
  end

  defp filter_tasks(tasks, filters) do
    tasks
    |> filter_by_status(filters[:status])
    |> filter_by_assigned_to(filters[:assigned_to])
    |> filter_by_tags(filters[:tags])
  end

  defp filter_by_status(tasks, nil), do: tasks
  defp filter_by_status(tasks, :TASK_STATUS_UNSPECIFIED), do: tasks
  defp filter_by_status(tasks, status) do
    Enum.filter(tasks, fn task -> task.status == status end)
  end

  defp filter_by_assigned_to(tasks, nil), do: tasks
  defp filter_by_assigned_to(tasks, ""), do: tasks
  defp filter_by_assigned_to(tasks, assigned_to) do
    Enum.filter(tasks, fn task -> task.assigned_to == assigned_to end)
  end

  defp filter_by_tags(tasks, nil), do: tasks
  defp filter_by_tags(tasks, []), do: tasks
  defp filter_by_tags(tasks, tags) do
    Enum.filter(tasks, fn task ->
      Enum.all?(tags, fn tag -> tag in task.tags end)
    end)
  end

  defp sort_tasks(tasks, nil), do: tasks
  defp sort_tasks(tasks, :SORT_ORDER_UNSPECIFIED), do: tasks
  defp sort_tasks(tasks, :SORT_ORDER_CREATED_AT_ASC) do
    Enum.sort_by(tasks, & &1.created_at.seconds)
  end
  defp sort_tasks(tasks, :SORT_ORDER_CREATED_AT_DESC) do
    Enum.sort_by(tasks, & &1.created_at.seconds, :desc)
  end
  defp sort_tasks(tasks, :SORT_ORDER_PRIORITY_ASC) do
    Enum.sort_by(tasks, & priority_value(&1.priority))
  end
  defp sort_tasks(tasks, :SORT_ORDER_PRIORITY_DESC) do
    Enum.sort_by(tasks, & priority_value(&1.priority), :desc)
  end
  defp sort_tasks(tasks, _), do: tasks

  defp priority_value(:TASK_PRIORITY_LOW), do: 1
  defp priority_value(:TASK_PRIORITY_MEDIUM), do: 2
  defp priority_value(:TASK_PRIORITY_HIGH), do: 3
  defp priority_value(:TASK_PRIORITY_CRITICAL), do: 4
  defp priority_value(_), do: 0

  defp paginate(tasks, nil, _), do: tasks
  defp paginate(tasks, page_size, page_token) do
    offset = parse_page_token(page_token)
    Enum.slice(tasks, offset, page_size)
  end

  defp parse_page_token(nil), do: 0
  defp parse_page_token(""), do: 0
  defp parse_page_token(token) when is_binary(token) do
    case Integer.parse(token) do
      {num, _} -> num
      :error -> 0
    end
  end

  defp current_timestamp do
    now = DateTime.utc_now()
    %Timestamp{
      seconds: DateTime.to_unix(now),
      nanos: 0
    }
  end

  defp notify_subscribers(subscribers, event_type, task) do
    Enum.each(subscribers, fn pid ->
      send(pid, {:task_event, event_type, task})
    end)
  end
end