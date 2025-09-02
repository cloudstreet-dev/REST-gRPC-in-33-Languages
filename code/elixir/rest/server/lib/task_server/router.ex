defmodule TaskServer.Router do
  use Plug.Router
  use Plug.ErrorHandler

  alias TaskServer.{Repository, Task}

  plug CORSPlug
  plug :match
  plug Plug.Parsers,
    parsers: [:json],
    pass: ["application/json"],
    json_decoder: Jason
  plug :dispatch

  # Health check
  get "/health" do
    response = %{
      status: "healthy",
      service: "elixir-task-api",
      task_count: Repository.count()
    }
    
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(response))
  end

  # List all tasks
  get "/api/tasks" do
    filters = %{
      "status" => conn.params["status"],
      "assigned_to" => conn.params["assigned_to"],
      "tags" => conn.params["tags"],
      "page_size" => parse_integer(conn.params["page_size"], 20),
      "page_token" => parse_integer(conn.params["page_token"], 0),
      "sort_by" => conn.params["sort_by"] || "created_at",
      "sort_order" => conn.params["sort_order"] || "desc"
    }
    
    case Repository.list_tasks(filters) do
      {:ok, result} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(result))
      _ ->
        send_error(conn, 500, "Internal server error")
    end
  end

  # Get a specific task
  get "/api/tasks/:id" do
    case Repository.get_task(id) do
      {:ok, task} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(Task.to_map(task)))
      {:error, :not_found} ->
        send_error(conn, 404, "Task not found")
    end
  end

  # Create a new task
  post "/api/tasks" do
    case conn.body_params do
      %{"title" => title} when title != "" ->
        case Repository.create_task(conn.body_params) do
          {:ok, task} ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(201, Jason.encode!(Task.to_map(task)))
          _ ->
            send_error(conn, 400, "Failed to create task")
        end
      _ ->
        send_error(conn, 400, "Title is required")
    end
  end

  # Update a task
  put "/api/tasks/:id" do
    case Repository.update_task(id, conn.body_params) do
      {:ok, task} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(Task.to_map(task)))
      {:error, :not_found} ->
        send_error(conn, 404, "Task not found")
    end
  end

  # Update task status
  patch "/api/tasks/:id/status" do
    status = conn.body_params["status"] || conn.params["status"]
    
    if status do
      case Repository.update_task_status(id, status) do
        {:ok, task} ->
          conn
          |> put_resp_content_type("application/json")
          |> send_resp(200, Jason.encode!(Task.to_map(task)))
        {:error, :not_found} ->
          send_error(conn, 404, "Task not found")
      end
    else
      send_error(conn, 400, "Status is required")
    end
  end

  # Delete a task
  delete "/api/tasks/:id" do
    case Repository.delete_task(id) do
      :ok ->
        send_resp(conn, 204, "")
      {:error, :not_found} ->
        send_error(conn, 404, "Task not found")
    end
  end

  # Handle OPTIONS for CORS
  options _ do
    send_resp(conn, 204, "")
  end

  # Catch-all
  match _ do
    send_error(conn, 404, "Not found")
  end

  # Error handler
  @impl Plug.ErrorHandler
  def handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    send_error(conn, conn.status, "Something went wrong")
  end

  # Helper functions
  defp send_error(conn, status, message) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(%{error: message}))
  end

  defp parse_integer(nil, default), do: default
  defp parse_integer(str, default) do
    case Integer.parse(str) do
      {num, _} -> num
      :error -> default
    end
  end
end