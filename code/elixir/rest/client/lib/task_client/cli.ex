defmodule TaskClient.CLI do
  @moduledoc """
  Command-line interface for Task Management REST API client
  """

  def main(_args) do
    IO.puts """
    ╔════════════════════════════════════════════════╗
    ║      Elixir Task Management REST Client        ║
    ║           Testing API Operations               ║
    ╚════════════════════════════════════════════════╝
    """
    
    # Ensure server is running
    :timer.sleep(1000)
    
    run_demo()
  end

  defp run_demo do
    IO.puts "\n1. Listing all tasks..."
    case TaskClient.list_tasks() do
      {:ok, result} ->
        IO.puts "   Found #{result["total_count"]} tasks"
        Enum.each(result["tasks"], fn task ->
          IO.puts "   - [#{task["id"]}] #{task["title"]} (#{task["status"]})"
        end)
      {:error, error} ->
        IO.puts "   Error: #{inspect(error)}"
    end
    
    IO.puts "\n2. Creating a new task..."
    new_task = %{
      title: "Implement GenServer patterns",
      description: "Learn and implement OTP GenServer patterns in Elixir",
      priority: "high",
      tags: ["elixir", "otp", "genserver"],
      assigned_to: "elixir-team"
    }
    
    task_id = case TaskClient.create_task(new_task) do
      {:ok, task} ->
        IO.puts "   Created task: #{task["title"]}"
        IO.puts "   ID: #{task["id"]}"
        IO.puts "   Priority: #{task["priority"]}"
        IO.puts "   Tags: #{Enum.join(task["tags"], ", ")}"
        task["id"]
      {:error, error} ->
        IO.puts "   Error: #{inspect(error)}"
        nil
    end
    
    if task_id do
      IO.puts "\n3. Getting task details..."
      case TaskClient.get_task(task_id) do
        {:ok, task} ->
          IO.puts "   Title: #{task["title"]}"
          IO.puts "   Description: #{task["description"]}"
          IO.puts "   Status: #{task["status"]}"
          IO.puts "   Assigned to: #{task["assigned_to"]}"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n4. Updating task status to 'in_progress'..."
      case TaskClient.update_task_status(task_id, "in_progress") do
        {:ok, task} ->
          IO.puts "   Updated status to: #{task["status"]}"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n5. Updating task details..."
      updates = %{
        title: "Master OTP GenServer patterns",
        priority: "urgent"
      }
      case TaskClient.update_task(task_id, updates) do
        {:ok, task} ->
          IO.puts "   Updated title: #{task["title"]}"
          IO.puts "   Updated priority: #{task["priority"]}"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n6. Filtering tasks by status..."
      case TaskClient.list_tasks(%{status: "in_progress"}) do
        {:ok, result} ->
          IO.puts "   Found #{result["total_count"]} in-progress tasks"
          Enum.each(result["tasks"], fn task ->
            IO.puts "   - #{task["title"]}"
          end)
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n7. Deleting the task..."
      case TaskClient.delete_task(task_id) do
        :ok ->
          IO.puts "   Task deleted successfully"
        {:error, error} ->
          IO.puts "   Error: #{inspect(error)}"
      end
      
      IO.puts "\n8. Verifying deletion..."
      case TaskClient.get_task(task_id) do
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