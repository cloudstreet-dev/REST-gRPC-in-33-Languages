defmodule TaskGrpcClient.CLI do
  @moduledoc """
  Command-line interface for Task Management gRPC client
  """
  
  alias TaskGrpcClient

  def main(_args) do
    IO.puts """
    ╔════════════════════════════════════════════════╗
    ║       Elixir Task Management gRPC Client       ║
    ║            Testing gRPC Operations             ║
    ╚════════════════════════════════════════════════╝
    """
    
    # Connect to server
    IO.puts "\nConnecting to gRPC server..."
    channel = TaskGrpcClient.connect()
    
    # Give server time to start
    :timer.sleep(1000)
    
    run_demo(channel)
    
    # Disconnect
    GRPC.Stub.disconnect(channel)
  end

  defp run_demo(channel) do
    IO.puts "\n1. Listing all tasks (streaming)..."
    case TaskGrpcClient.list_tasks(channel) do
      {:ok, tasks} ->
        IO.puts "   Received #{length(tasks)} tasks via stream"
        Enum.each(tasks, fn task ->
          IO.puts "   - [#{task.id}] #{task.title} (#{task.status})"
        end)
      {:error, error} ->
        IO.puts "   Error: #{inspect(error)}"
    end
    
    IO.puts "\n2. Creating a new task..."
    task_id = case TaskGrpcClient.create_task(channel, "Implement gRPC streaming",
      description: "Add support for bidirectional streaming in Elixir",
      priority: :TASK_PRIORITY_HIGH,
      tags: ["elixir", "grpc", "streaming"],
      assigned_to: "elixir-team"
    ) do
      {:ok, task} ->
        IO.puts "   Created task: #{task.title}"
        IO.puts "   ID: #{task.id}"
        IO.puts "   Priority: #{task.priority}"
        IO.puts "   Tags: #{Enum.join(task.tags, ", ")}"
        task.id
      {:error, error} ->
        IO.puts "   Error: #{inspect(error)}"
        nil
    end
    
    if task_id do
      IO.puts "\n3. Getting task details..."
      case TaskGrpcClient.get_task(channel, task_id) do
        {:ok, task} ->
          IO.puts "   Title: #{task.title}"
          IO.puts "   Description: #{task.description}"
          IO.puts "   Status: #{task.status}"
          IO.puts "   Assigned to: #{task.assigned_to}"
        {:error, :not_found} ->
          IO.puts "   Task not found"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n4. Updating task status..."
      updates = %{status: :TASK_STATUS_IN_PROGRESS}
      case TaskGrpcClient.update_task(channel, task_id, updates, ["status"]) do
        {:ok, task} ->
          IO.puts "   Updated status to: #{task.status}"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n5. Updating task details..."
      updates = %{
        title: "Master gRPC streaming patterns",
        priority: :TASK_PRIORITY_CRITICAL
      }
      case TaskGrpcClient.update_task(channel, task_id, updates, ["title", "priority"]) do
        {:ok, task} ->
          IO.puts "   Updated title: #{task.title}"
          IO.puts "   Updated priority: #{task.priority}"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n6. Watching for task changes (bidirectional streaming)..."
      stream = TaskGrpcClient.watch_tasks(channel, watch_all: true)
      
      # Start a task to receive events
      event_task = Task.async(fn ->
        IO.puts "   Waiting for events..."
        stream
        |> Enum.take(3)  # Take first 3 events
        |> Enum.each(fn event ->
          IO.puts "   Event: #{event.event_type} for task #{event.task.id}"
        end)
      end)
      
      # Give some time for initial events
      :timer.sleep(1000)
      
      # Create another task to generate an event
      IO.puts "   Creating task to trigger event..."
      TaskGrpcClient.create_task(channel, "Event test task", 
        description: "This should trigger a watch event")
      
      # Wait for events
      Task.await(event_task, 5000)
      
      IO.puts "\n7. Deleting the task..."
      case TaskGrpcClient.delete_task(channel, task_id) do
        :ok ->
          IO.puts "   Task deleted successfully"
        {:error, :not_found} ->
          IO.puts "   Task not found"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n8. Verifying deletion..."
      case TaskGrpcClient.get_task(channel, task_id) do
        {:error, :not_found} ->
          IO.puts "   Task not found (as expected)"
        {:ok, _} ->
          IO.puts "   Error: Task still exists"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
    end
    
    IO.puts "\n✅ Demo completed successfully!"
  end
end