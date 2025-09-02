defmodule TaskServer.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    port = String.to_integer(System.get_env("PORT", "8080"))
    
    IO.puts """
    ╔════════════════════════════════════════════════╗
    ║        Elixir Task Management REST API         ║
    ║            Built with Plug & Cowboy            ║
    ╚════════════════════════════════════════════════╝
    
    [INFO] Elixir Task REST Server starting on port #{port}
    [INFO] Visit http://localhost:#{port}/api/tasks
    
    Available endpoints:
      GET    /api/tasks          - List all tasks
      GET    /api/tasks/{id}     - Get a specific task
      POST   /api/tasks          - Create a new task
      PUT    /api/tasks/{id}     - Update a task
      PATCH  /api/tasks/{id}/status - Update task status
      DELETE /api/tasks/{id}     - Delete a task
      GET    /health             - Health check
    
    Sample requests:
      curl http://localhost:#{port}/api/tasks
      curl -X POST http://localhost:#{port}/api/tasks \\
        -H "Content-Type: application/json" \\
        -d '{"title":"New Task","priority":"high"}'
    
    [INFO] Press Ctrl+C twice to stop the server
    """

    children = [
      TaskServer.Repository,
      {Plug.Cowboy, scheme: :http, plug: TaskServer.Router, options: [port: port]}
    ]

    opts = [strategy: :one_for_one, name: TaskServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end