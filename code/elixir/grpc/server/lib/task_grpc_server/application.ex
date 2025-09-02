defmodule TaskGrpcServer.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    port = String.to_integer(System.get_env("GRPC_PORT", "50051"))
    
    IO.puts """
    ╔════════════════════════════════════════════════╗
    ║        Elixir Task Management gRPC Server      ║
    ║               Built with grpc-elixir           ║
    ╚════════════════════════════════════════════════╝
    
    [INFO] Elixir gRPC Server starting on port #{port}
    [INFO] Server ready to accept connections
    
    Available services:
      TaskService
        - ListTasks (streaming)
        - GetTask
        - CreateTask
        - UpdateTask
        - DeleteTask
        - WatchTasks (bidirectional streaming)
    
    [INFO] Press Ctrl+C twice to stop the server
    """

    children = [
      {TaskGrpcServer.Repository, []},
      {GRPC.Server.Supervisor, 
       endpoint: TaskGrpcServer.Endpoint, 
       port: port, 
       start_server: true}
    ]

    opts = [strategy: :one_for_one, name: TaskGrpcServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end