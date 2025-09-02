defmodule TaskServer.Repository do
  @moduledoc """
  GenServer-based repository for managing tasks
  """
  
  use GenServer
  alias TaskServer.Task

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

  def create_task(attrs) do
    GenServer.call(__MODULE__, {:create_task, attrs})
  end

  def update_task(id, attrs) do
    GenServer.call(__MODULE__, {:update_task, id, attrs})
  end

  def update_task_status(id, status) do
    GenServer.call(__MODULE__, {:update_task_status, id, status})
  end

  def delete_task(id) do
    GenServer.call(__MODULE__, {:delete_task, id})
  end

  def count() do
    GenServer.call(__MODULE__, :count)
  end

  # Server callbacks

  @impl true
  def init(_) do
    tasks = load_sample_data()
    {:ok, %{tasks: tasks}}
  end

  @impl true
  def handle_call({:list_tasks, filters}, _from, state) do
    tasks = Map.values(state.tasks)
    
    # Apply filters
    tasks = filter_tasks(tasks, filters)
    
    # Sort
    sort_by = Map.get(filters, "sort_by", "created_at")
    sort_order = Map.get(filters, "sort_order", "desc")
    tasks = sort_tasks(tasks, sort_by, sort_order)
    
    # Paginate
    page_size = min(Map.get(filters, "page_size", 20), 100)
    page_token = Map.get(filters, "page_token", 0)
    
    total = length(tasks)
    paginated = Enum.slice(tasks, page_token, page_size)
    next_token = if page_token + page_size < total, do: page_token + page_size, else: nil
    
    result = %{
      tasks: Enum.map(paginated, &Task.to_map/1),
      total_count: total,
      page_size: page_size,
      next_page_token: next_token
    }
    
    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:get_task, id}, _from, state) do
    case Map.get(state.tasks, id) do
      nil -> {:reply, {:error, :not_found}, state}
      task -> {:reply, {:ok, task}, state}
    end
  end

  @impl true
  def handle_call({:create_task, attrs}, _from, state) do
    task = Task.new(attrs)
    new_tasks = Map.put(state.tasks, task.id, task)
    {:reply, {:ok, task}, %{state | tasks: new_tasks}}
  end

  @impl true
  def handle_call({:update_task, id, attrs}, _from, state) do
    case Map.get(state.tasks, id) do
      nil -> 
        {:reply, {:error, :not_found}, state}
      task ->
        updated_task = update_task_attrs(task, attrs)
        new_tasks = Map.put(state.tasks, id, updated_task)
        {:reply, {:ok, updated_task}, %{state | tasks: new_tasks}}
    end
  end

  @impl true
  def handle_call({:update_task_status, id, status}, _from, state) do
    case Map.get(state.tasks, id) do
      nil -> 
        {:reply, {:error, :not_found}, state}
      task ->
        updated_task = %{task | status: parse_status(status), updated_at: DateTime.utc_now()}
        new_tasks = Map.put(state.tasks, id, updated_task)
        {:reply, {:ok, updated_task}, %{state | tasks: new_tasks}}
    end
  end

  @impl true
  def handle_call({:delete_task, id}, _from, state) do
    case Map.has_key?(state.tasks, id) do
      false -> 
        {:reply, {:error, :not_found}, state}
      true ->
        new_tasks = Map.delete(state.tasks, id)
        {:reply, :ok, %{state | tasks: new_tasks}}
    end
  end

  @impl true
  def handle_call(:count, _from, state) do
    {:reply, map_size(state.tasks), state}
  end

  # Private functions

  defp load_sample_data do
    task1 = Task.new(%{
      title: "Implement Elixir REST API",
      description: "Build a REST API server using Plug and Cowboy",
      status: "in_progress",
      priority: "high",
      tags: ["elixir", "rest", "api"],
      assigned_to: "backend-team"
    })
    
    task2 = Task.new(%{
      title: "Add Phoenix LiveView",
      description: "Implement real-time features with Phoenix LiveView",
      status: "pending",
      priority: "medium",
      tags: ["elixir", "phoenix", "liveview"],
      assigned_to: "frontend-team"
    })
    
    task3 = Task.new(%{
      title: "Write ExUnit tests",
      description: "Add comprehensive test coverage using ExUnit",
      status: "pending",
      priority: "high",
      tags: ["testing", "quality"],
      assigned_to: "qa-team"
    })
    
    %{
      task1.id => task1,
      task2.id => task2,
      task3.id => task3
    }
  end

  defp filter_tasks(tasks, filters) do
    tasks
    |> filter_by_status(Map.get(filters, "status"))
    |> filter_by_assigned_to(Map.get(filters, "assigned_to"))
    |> filter_by_tags(Map.get(filters, "tags"))
  end

  defp filter_by_status(tasks, nil), do: tasks
  defp filter_by_status(tasks, status) do
    status_atom = parse_status(status)
    Enum.filter(tasks, fn task -> task.status == status_atom end)
  end

  defp filter_by_assigned_to(tasks, nil), do: tasks
  defp filter_by_assigned_to(tasks, assigned_to) do
    Enum.filter(tasks, fn task -> task.assigned_to == assigned_to end)
  end

  defp filter_by_tags(tasks, nil), do: tasks
  defp filter_by_tags(tasks, tags_string) when is_binary(tags_string) do
    tags = String.split(tags_string, ",")
    Enum.filter(tasks, fn task ->
      Enum.all?(tags, fn tag -> tag in task.tags end)
    end)
  end
  defp filter_by_tags(tasks, _), do: tasks

  defp sort_tasks(tasks, "title", "asc"), do: Enum.sort_by(tasks, & &1.title)
  defp sort_tasks(tasks, "title", _), do: Enum.sort_by(tasks, & &1.title, :desc)
  defp sort_tasks(tasks, "updated_at", "asc"), do: Enum.sort_by(tasks, & &1.updated_at, DateTime)
  defp sort_tasks(tasks, "updated_at", _), do: Enum.sort_by(tasks, & &1.updated_at, {:desc, DateTime})
  defp sort_tasks(tasks, _, "asc"), do: Enum.sort_by(tasks, & &1.created_at, DateTime)
  defp sort_tasks(tasks, _, _), do: Enum.sort_by(tasks, & &1.created_at, {:desc, DateTime})

  defp update_task_attrs(task, attrs) do
    task
    |> maybe_update(:title, Map.get(attrs, "title"))
    |> maybe_update(:description, Map.get(attrs, "description"))
    |> maybe_update(:status, parse_status(Map.get(attrs, "status")))
    |> maybe_update(:priority, parse_priority(Map.get(attrs, "priority")))
    |> maybe_update(:tags, Map.get(attrs, "tags"))
    |> maybe_update(:assigned_to, Map.get(attrs, "assigned_to"))
    |> Map.put(:updated_at, DateTime.utc_now())
  end

  defp maybe_update(task, _field, nil), do: task
  defp maybe_update(task, field, value), do: Map.put(task, field, value)

  defp parse_status("pending"), do: :pending
  defp parse_status("in_progress"), do: :in_progress
  defp parse_status("completed"), do: :completed
  defp parse_status("cancelled"), do: :cancelled
  defp parse_status(_), do: nil

  defp parse_priority("low"), do: :low
  defp parse_priority("medium"), do: :medium
  defp parse_priority("high"), do: :high
  defp parse_priority("urgent"), do: :urgent
  defp parse_priority(_), do: nil
end