defmodule TaskServer.Task do
  @moduledoc """
  Task model representing a task in the system
  """

  @derive Jason.Encoder
  defstruct [
    :id,
    :title,
    :description,
    :status,
    :priority,
    :tags,
    :assigned_to,
    :created_at,
    :updated_at
  ]

  @type status :: :pending | :in_progress | :completed | :cancelled
  @type priority :: :low | :medium | :high | :urgent

  @type t :: %__MODULE__{
    id: String.t(),
    title: String.t(),
    description: String.t(),
    status: status(),
    priority: priority(),
    tags: [String.t()],
    assigned_to: String.t(),
    created_at: DateTime.t(),
    updated_at: DateTime.t()
  }

  @doc """
  Creates a new task with default values
  """
  def new(attrs \\ %{}) do
    now = DateTime.utc_now()
    
    %__MODULE__{
      id: UUID.uuid4(),
      title: Map.get(attrs, "title", Map.get(attrs, :title, "")),
      description: Map.get(attrs, "description", Map.get(attrs, :description, "")),
      status: parse_status(Map.get(attrs, "status", Map.get(attrs, :status, "pending"))),
      priority: parse_priority(Map.get(attrs, "priority", Map.get(attrs, :priority, "medium"))),
      tags: Map.get(attrs, "tags", Map.get(attrs, :tags, [])),
      assigned_to: Map.get(attrs, "assigned_to", Map.get(attrs, :assigned_to, "")),
      created_at: now,
      updated_at: now
    }
  end

  @doc """
  Updates a task with new attributes
  """
  def update(task = %__MODULE__{}, attrs) do
    task
    |> Map.merge(attrs)
    |> Map.put(:updated_at, DateTime.utc_now())
  end

  @doc """
  Converts task to JSON-encodable map
  """
  def to_map(task = %__MODULE__{}) do
    %{
      id: task.id,
      title: task.title,
      description: task.description,
      status: to_string(task.status),
      priority: to_string(task.priority),
      tags: task.tags,
      assigned_to: task.assigned_to,
      created_at: DateTime.to_iso8601(task.created_at),
      updated_at: DateTime.to_iso8601(task.updated_at)
    }
  end

  defp parse_status(status) when is_atom(status), do: status
  defp parse_status("pending"), do: :pending
  defp parse_status("in_progress"), do: :in_progress
  defp parse_status("completed"), do: :completed
  defp parse_status("cancelled"), do: :cancelled
  defp parse_status(_), do: :pending

  defp parse_priority(priority) when is_atom(priority), do: priority
  defp parse_priority("low"), do: :low
  defp parse_priority("medium"), do: :medium
  defp parse_priority("high"), do: :high
  defp parse_priority("urgent"), do: :urgent
  defp parse_priority(_), do: :medium
end