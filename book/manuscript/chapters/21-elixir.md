# Chapter 21: Elixir - Erlang's Ruby

## Introduction

Elixir is a dynamic, functional programming language designed for building maintainable and scalable applications. Created by José Valim in 2011, Elixir leverages the Erlang Virtual Machine (BEAM), known for running low-latency, distributed, and fault-tolerant systems. With its Ruby-inspired syntax, powerful metaprogramming capabilities, and the battle-tested OTP (Open Telecom Platform) framework, Elixir brings modern language features to a platform with decades of production reliability.

## About the Elixir Programming Language

Elixir was born from José Valim's desire to bring modern programming language features to the Erlang ecosystem. While working with Ruby on Rails, Valim recognized the need for better concurrency and fault tolerance in web applications. Rather than building from scratch, he chose to build on Erlang's proven foundation, creating a language that combines Ruby's developer-friendly syntax with Erlang's industrial-strength runtime.

### Language Philosophy

Elixir embraces several core principles:
- **Fault Tolerance**: Let it crash and recover gracefully
- **Concurrency**: Millions of lightweight processes
- **Functional Programming**: Immutability and pattern matching
- **Metaprogramming**: Powerful macro system
- **Interoperability**: Seamless integration with Erlang
- **Developer Happiness**: Clear syntax and excellent tooling

## REST API Implementation

Our Elixir implementation uses Plug (a specification for composable modules) and Cowboy (an Erlang HTTP server) to build a lightweight REST API.

### Task Model with Structs

```elixir
defmodule TaskServer.Task do
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

  def new(attrs \\ %{}) do
    now = DateTime.utc_now()
    
    %__MODULE__{
      id: UUID.uuid4(),
      title: Map.get(attrs, "title", ""),
      description: Map.get(attrs, "description", ""),
      status: parse_status(Map.get(attrs, "status", "pending")),
      priority: parse_priority(Map.get(attrs, "priority", "medium")),
      tags: Map.get(attrs, "tags", []),
      assigned_to: Map.get(attrs, "assigned_to", ""),
      created_at: now,
      updated_at: now
    }
  end
end
```

### GenServer Repository

Elixir's GenServer provides stateful server processes:

```elixir
defmodule TaskServer.Repository do
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

  # Server callbacks
  @impl true
  def init(_) do
    tasks = load_sample_data()
    {:ok, %{tasks: tasks}}
  end

  @impl true
  def handle_call({:list_tasks, filters}, _from, state) do
    tasks = Map.values(state.tasks)
    |> filter_tasks(filters)
    |> sort_tasks(filters["sort_by"], filters["sort_order"])
    |> paginate(filters["page_size"], filters["page_token"])
    
    {:reply, {:ok, tasks}, state}
  end

  @impl true
  def handle_call({:create_task, attrs}, _from, state) do
    task = Task.new(attrs)
    new_tasks = Map.put(state.tasks, task.id, task)
    {:reply, {:ok, task}, %{state | tasks: new_tasks}}
  end
end
```

### Plug Router

```elixir
defmodule TaskServer.Router do
  use Plug.Router
  use Plug.ErrorHandler

  plug CORSPlug
  plug :match
  plug Plug.Parsers,
    parsers: [:json],
    pass: ["application/json"],
    json_decoder: Jason
  plug :dispatch

  get "/api/tasks" do
    filters = extract_filters(conn)
    
    case Repository.list_tasks(filters) do
      {:ok, result} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(result))
      _ ->
        send_error(conn, 500, "Internal server error")
    end
  end

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
end
```

## The Actor Model and OTP

### Processes and Message Passing

```elixir
# Spawn a new process
pid = spawn(fn ->
  receive do
    {:hello, sender} ->
      send(sender, {:world, self()})
  end
end)

# Send a message
send(pid, {:hello, self()})

# Receive response
receive do
  {:world, from} ->
    IO.puts("Got response from #{inspect(from)}")
after
  1000 ->
    IO.puts("Timeout waiting for response")
end
```

### Supervisors and Fault Tolerance

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      {TaskServer.Repository, []},
      {Plug.Cowboy, scheme: :http, plug: TaskServer.Router, options: [port: 8080]},
      {MyApp.Worker, []}
    ]

    # Restart strategy: one_for_one, rest_for_one, one_for_all
    opts = [strategy: :one_for_one, max_restarts: 10, max_seconds: 10]
    Supervisor.init(children, opts)
  end
end
```

### GenServer Pattern

```elixir
defmodule Counter do
  use GenServer

  # Client API
  def start_link(initial_value) do
    GenServer.start_link(__MODULE__, initial_value)
  end

  def increment(pid) do
    GenServer.call(pid, :increment)
  end

  def get_value(pid) do
    GenServer.call(pid, :get_value)
  end

  # Server Callbacks
  @impl true
  def init(initial_value) do
    {:ok, initial_value}
  end

  @impl true
  def handle_call(:increment, _from, state) do
    new_state = state + 1
    {:reply, new_state, new_state}
  end

  @impl true
  def handle_call(:get_value, _from, state) do
    {:reply, state, state}
  end
end
```

## Pattern Matching

### Basic Pattern Matching

```elixir
# Simple matching
{:ok, result} = {:ok, 42}

# List matching
[head | tail] = [1, 2, 3, 4]
# head = 1, tail = [2, 3, 4]

# Map matching
%{name: name, age: age} = %{name: "Alice", age: 30, city: "NYC"}
# name = "Alice", age = 30

# Function heads
def process({:ok, data}), do: handle_success(data)
def process({:error, reason}), do: handle_error(reason)
```

### Guards and Multiple Clauses

```elixir
defmodule Validator do
  def validate(value) when is_binary(value) and byte_size(value) > 0 do
    {:ok, value}
  end

  def validate(value) when is_integer(value) and value > 0 do
    {:ok, value}
  end

  def validate(nil) do
    {:error, "Value cannot be nil"}
  end

  def validate(_) do
    {:error, "Invalid value"}
  end
end
```

### Case and Cond

```elixir
# Case expression
result = case File.read("config.json") do
  {:ok, content} ->
    Jason.decode(content)
  {:error, :enoent} ->
    {:error, "File not found"}
  {:error, reason} ->
    {:error, "Failed to read file: #{reason}"}
end

# Cond expression
cond do
  temperature > 30 -> "Hot"
  temperature > 20 -> "Warm"
  temperature > 10 -> "Cool"
  true -> "Cold"
end
```

## Pipe Operator and Function Composition

```elixir
# Without pipes (nested calls)
result = Enum.map(
  Enum.filter(
    String.split(text, " "),
    &(String.length(&1) > 3)
  ),
  &String.upcase/1
)

# With pipes (readable flow)
result = text
|> String.split(" ")
|> Enum.filter(&(String.length(&1) > 3))
|> Enum.map(&String.upcase/1)

# Custom pipeline functions
defmodule Pipeline do
  def process_tasks(tasks) do
    tasks
    |> filter_active()
    |> sort_by_priority()
    |> assign_to_workers()
    |> notify_assignments()
  end

  defp filter_active(tasks) do
    Enum.filter(tasks, & &1.status in [:pending, :in_progress])
  end

  defp sort_by_priority(tasks) do
    Enum.sort_by(tasks, & &1.priority, :desc)
  end
end
```

## Metaprogramming with Macros

### Basic Macros

```elixir
defmodule MyMacros do
  defmacro unless(condition, do: block) do
    quote do
      if !unquote(condition), do: unquote(block)
    end
  end

  defmacro log(expression) do
    quote do
      result = unquote(expression)
      IO.puts("Executed: #{unquote(Macro.to_string(expression))}")
      IO.puts("Result: #{inspect(result)}")
      result
    end
  end
end

# Usage
import MyMacros
unless false, do: IO.puts("This prints")
value = log(1 + 2 + 3)
```

### DSL Creation

```elixir
defmodule Router do
  defmacro __using__(_opts) do
    quote do
      import Router
      @routes []
    end
  end

  defmacro get(path, do: block) do
    quote do
      @routes [{:get, unquote(path), unquote(Macro.escape(block))} | @routes]
    end
  end

  defmacro post(path, do: block) do
    quote do
      @routes [{:post, unquote(path), unquote(Macro.escape(block))} | @routes]
    end
  end
end

# Usage
defmodule MyRouter do
  use Router

  get "/users" do
    send_resp(conn, 200, "Users list")
  end

  post "/users" do
    send_resp(conn, 201, "User created")
  end
end
```

## Concurrent Programming

### Task Module

```elixir
# Async execution
task = Task.async(fn ->
  # Expensive computation
  :timer.sleep(1000)
  42
end)

# Do other work...

# Wait for result
result = Task.await(task)

# Multiple async tasks
tasks = for i <- 1..10 do
  Task.async(fn ->
    process_item(i)
  end)
end

results = Task.await_many(tasks)
```

### Agent for State Management

```elixir
{:ok, agent} = Agent.start_link(fn -> %{} end)

# Update state
Agent.update(agent, fn state ->
  Map.put(state, :counter, 1)
end)

# Get state
value = Agent.get(agent, fn state ->
  Map.get(state, :counter)
end)

# Get and update
new_value = Agent.get_and_update(agent, fn state ->
  current = Map.get(state, :counter, 0)
  {current, Map.put(state, :counter, current + 1)}
end)
```

### GenStage for Data Pipelines

```elixir
defmodule Producer do
  use GenStage

  def start_link(initial) do
    GenStage.start_link(__MODULE__, initial, name: __MODULE__)
  end

  def init(counter) do
    {:producer, counter}
  end

  def handle_demand(demand, counter) do
    events = Enum.to_list(counter..(counter + demand - 1))
    {:noreply, events, counter + demand}
  end
end

defmodule Consumer do
  use GenStage

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    {:consumer, :ok}
  end

  def handle_events(events, _from, state) do
    Enum.each(events, &process_event/1)
    {:noreply, [], state}
  end
end
```

## Phoenix Framework Integration

While our implementation uses Plug directly, Phoenix is Elixir's premier web framework:

### Phoenix Contexts

```elixir
defmodule TaskManager.Tasks do
  @moduledoc """
  The Tasks context.
  """

  import Ecto.Query, warn: false
  alias TaskManager.Repo
  alias TaskManager.Tasks.Task

  def list_tasks do
    Repo.all(Task)
  end

  def get_task!(id), do: Repo.get!(Task, id)

  def create_task(attrs \\ %{}) do
    %Task{}
    |> Task.changeset(attrs)
    |> Repo.insert()
  end

  def update_task(%Task{} = task, attrs) do
    task
    |> Task.changeset(attrs)
    |> Repo.update()
  end

  def delete_task(%Task{} = task) do
    Repo.delete(task)
  end
end
```

### Phoenix LiveView

```elixir
defmodule TaskManagerWeb.TaskLive.Index do
  use TaskManagerWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :tasks, list_tasks())}
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    task = Tasks.get_task!(id)
    {:ok, _} = Tasks.delete_task(task)

    {:noreply, assign(socket, :tasks, list_tasks())}
  end

  @impl true
  def handle_event("toggle_status", %{"id" => id}, socket) do
    task = Tasks.get_task!(id)
    new_status = if task.completed, do: false, else: true
    
    {:ok, _} = Tasks.update_task(task, %{completed: new_status})
    
    {:noreply, assign(socket, :tasks, list_tasks())}
  end
end
```

## Testing with ExUnit

### Basic Tests

```elixir
defmodule TaskServerTest do
  use ExUnit.Case
  
  describe "Task creation" do
    test "creates task with required fields" do
      task = Task.new(%{"title" => "Test Task"})
      
      assert task.title == "Test Task"
      assert task.status == :pending
      assert task.priority == :medium
    end

    test "validates title presence" do
      assert_raise ArgumentError, fn ->
        Task.new(%{})
      end
    end
  end

  describe "Repository" do
    setup do
      {:ok, pid} = TaskServer.Repository.start_link([])
      {:ok, repository: pid}
    end

    test "lists all tasks", %{repository: _repo} do
      {:ok, result} = TaskServer.Repository.list_tasks()
      
      assert is_list(result.tasks)
      assert result.total_count >= 0
    end

    test "creates and retrieves task" do
      {:ok, task} = TaskServer.Repository.create_task(%{
        "title" => "New Task"
      })
      
      {:ok, retrieved} = TaskServer.Repository.get_task(task.id)
      assert retrieved.id == task.id
    end
  end
end
```

### Property-Based Testing with StreamData

```elixir
use ExUnitProperties

property "task status is always valid" do
  check all status <- member_of([:pending, :in_progress, :completed, :cancelled]) do
    task = Task.new(%{"status" => to_string(status)})
    assert task.status in [:pending, :in_progress, :completed, :cancelled]
  end
end

property "pagination never exceeds page size" do
  check all page_size <- integer(1..100),
            total <- integer(0..1000) do
    result = paginate(generate_tasks(total), page_size)
    assert length(result.tasks) <= page_size
  end
end
```

## Error Handling

### The "Let It Crash" Philosophy

```elixir
defmodule ResilientWorker do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(args) do
    # Don't try to handle every possible error
    # Let the supervisor restart us if something goes wrong
    {:ok, args}
  end

  @impl true
  def handle_call({:process, data}, _from, state) do
    # This might crash, and that's OK
    result = dangerous_operation(data)
    {:reply, {:ok, result}, state}
  end

  # The supervisor will restart this process if it crashes
  defp dangerous_operation(data) do
    # Potentially dangerous code
    process_data(data)
  end
end
```

### With Construct for Error Handling

```elixir
def complex_operation(params) do
  with {:ok, validated} <- validate(params),
       {:ok, prepared} <- prepare(validated),
       {:ok, result} <- execute(prepared),
       {:ok, formatted} <- format(result) do
    {:ok, formatted}
  else
    {:error, :validation_failed} = error ->
      Logger.error("Validation failed")
      error
    
    {:error, reason} = error ->
      Logger.error("Operation failed: #{inspect(reason)}")
      error
  end
end
```

## Performance and Optimization

### ETS (Erlang Term Storage)

```elixir
# Create an ETS table for caching
:ets.new(:task_cache, [:set, :public, :named_table])

# Store data
:ets.insert(:task_cache, {task_id, task_data})

# Retrieve data
case :ets.lookup(:task_cache, task_id) do
  [{^task_id, data}] -> {:ok, data}
  [] -> {:error, :not_found}
end

# Delete data
:ets.delete(:task_cache, task_id)
```

### Process Pooling with Poolboy

```elixir
defmodule Worker do
  use GenServer

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil)
  end

  def process(worker, data) do
    GenServer.call(worker, {:process, data})
  end

  @impl true
  def handle_call({:process, data}, _from, state) do
    result = expensive_operation(data)
    {:reply, result, state}
  end
end

# Configuration
:poolboy.child_spec(:worker_pool,
  [
    name: {:local, :worker_pool},
    worker_module: Worker,
    size: 10,
    max_overflow: 5
  ]
)

# Usage
:poolboy.transaction(:worker_pool, fn worker ->
  Worker.process(worker, data)
end)
```

## Deployment and Operations

### Release Building with Mix

```elixir
# mix.exs
def project do
  [
    app: :my_app,
    version: "0.1.0",
    elixir: "~> 1.14",
    releases: [
      my_app: [
        include_executables_for: [:unix],
        applications: [runtime_tools: :permanent]
      ]
    ]
  ]
end

# Build release
# mix release

# Run release
# _build/prod/rel/my_app/bin/my_app start
```

### Hot Code Reloading

```elixir
defmodule Versioned do
  @vsn "1.0.0"

  def version, do: @vsn

  # Code upgrade callbacks
  def code_change("0.9.0", state, _extra) do
    # Migrate state from old version
    new_state = migrate_state(state)
    {:ok, new_state}
  end
end
```

### Distributed Elixir

```elixir
# Start node with name
# iex --name node1@localhost --cookie secret

# Connect nodes
Node.connect(:"node2@localhost")

# Spawn process on remote node
Node.spawn(:"node2@localhost", fn ->
  IO.puts("Running on node2")
end)

# Global process registry
:global.register_name(:my_process, self())
pid = :global.whereis_name(:my_process)
```

## Elixir vs Other Languages

### Elixir vs Ruby
- **Elixir Advantages**: True concurrency, fault tolerance, functional paradigm
- **Ruby Advantages**: Larger ecosystem, simpler learning curve
- **Use Elixir when**: Building concurrent, fault-tolerant systems
- **Use Ruby when**: Rapid prototyping, extensive gem requirements

### Elixir vs Go
- **Elixir Advantages**: Actor model, hot code reloading, pattern matching
- **Go Advantages**: Static typing, simpler deployment, larger community
- **Use Elixir when**: Need fault tolerance and distribution
- **Use Go when**: Building microservices, need static typing

### Elixir vs Node.js
- **Elixir Advantages**: True parallelism, fault tolerance, better performance
- **Node.js Advantages**: JavaScript everywhere, larger ecosystem
- **Use Elixir when**: Building real-time, concurrent applications
- **Use Node.js when**: Frontend/backend code sharing is important

## Best Practices

1. **Embrace Immutability**: Design with immutable data structures
2. **Think in Processes**: Model concurrency with lightweight processes
3. **Pattern Match Early**: Use pattern matching in function heads
4. **Let It Crash**: Don't defensive program, use supervisors
5. **Use OTP**: Leverage GenServer, Supervisor, and other behaviors
6. **Document with @doc**: Use ExDoc for documentation
7. **Type Specifications**: Add @spec annotations for better tooling
8. **Property Testing**: Use StreamData for comprehensive testing

## gRPC Implementation

Elixir has excellent gRPC support through the `grpc` library, which provides both client and server implementations:

### Server Implementation

```elixir
# mix.exs dependencies
defp deps do
  [
    {:grpc, "~> 0.6"},
    {:protobuf, "~> 0.11"},
    {:google_protos, "~> 0.3"}
  ]
end

# Generated from tasks.proto
defmodule Tasks.TaskService.Server do
  use GRPC.Server, service: Tasks.TaskService.Service

  def list_tasks(request, stream) do
    tasks = TaskStore.list_tasks(
      status: request.status,
      assigned_to: request.assigned_to
    )
    
    Enum.each(tasks, fn task ->
      GRPC.Server.send_reply(stream, task)
    end)
  end

  def get_task(%{id: id}, _stream) do
    case TaskStore.get_task(id) do
      {:ok, task} -> task
      {:error, :not_found} ->
        raise GRPC.RPCError, status: :not_found, message: "Task not found"
    end
  end

  def create_task(%{task: task_data}, _stream) do
    {:ok, task} = TaskStore.create_task(task_data)
    task
  end

  def watch_tasks(stream) do
    # Bidirectional streaming
    spawn(fn -> handle_watch_input(stream) end)
    
    TaskStore.subscribe_to_events()
    
    receive do
      {:task_event, event} ->
        GRPC.Server.send_reply(stream, event)
        watch_tasks(stream)
      :stop ->
        :ok
    end
  end
end

# Start gRPC server
defmodule TaskGrpcServer.Application do
  use Application

  def start(_type, _args) do
    children = [
      {GRPC.Server.Supervisor, 
       {Tasks.TaskService.Server, 50051, [interceptors: [LoggingInterceptor]]}}
    ]

    opts = [strategy: :one_for_one, name: TaskGrpcServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### Client Implementation

```elixir
defmodule TaskGrpcClient do
  def connect(host \\ "localhost", port \\ 50051) do
    {:ok, channel} = GRPC.Stub.connect("#{host}:#{port}")
    channel
  end

  def list_tasks(channel, opts \\ []) do
    request = %ListTasksRequest{
      status: Keyword.get(opts, :status),
      assigned_to: Keyword.get(opts, :assigned_to)
    }
    
    {:ok, stream} = channel |> Tasks.TaskService.Stub.list_tasks(request)
    
    Enum.to_list(stream)
  end

  def create_task(channel, task_data) do
    request = %CreateTaskRequest{task: task_data}
    
    case channel |> Tasks.TaskService.Stub.create_task(request) do
      {:ok, task} -> {:ok, task}
      {:error, %GRPC.RPCError{} = error} -> {:error, error}
    end
  end

  def watch_tasks(channel) do
    {:ok, stream} = channel |> Tasks.TaskService.Stub.watch_tasks()
    
    # Send watch requests
    GRPC.Stub.send_request(stream, %WatchTasksRequest{watch_all: true})
    
    # Receive events
    Task.async(fn ->
      Enum.each(stream, fn event ->
        IO.inspect(event, label: "Task event")
      end)
    end)
    
    stream
  end
end
```

### Interceptors

```elixir
defmodule LoggingInterceptor do
  @behaviour GRPC.Server.Interceptor

  def init(opts), do: opts

  def call(req, stream, next, _opts) do
    start_time = System.monotonic_time()
    
    try do
      result = next.(req, stream)
      duration = System.monotonic_time() - start_time
      Logger.info("gRPC call completed in #{duration}μs")
      result
    rescue
      error ->
        Logger.error("gRPC call failed: #{inspect(error)}")
        reraise error, __STACKTRACE__
    end
  end
end
```

### Advantages of gRPC in Elixir

1. **Native Streaming**: Elixir's processes map naturally to gRPC streams
2. **Fault Tolerance**: OTP supervision trees handle gRPC service failures
3. **Hot Code Reload**: Update gRPC services without downtime
4. **Distributed**: Easy to scale gRPC services across nodes
5. **Performance**: Efficient binary protocol handling

The combination of Elixir's actor model with gRPC's streaming capabilities creates a powerful platform for building resilient, real-time microservices.

## Conclusion

Elixir represents a unique combination of developer-friendly syntax, functional programming paradigms, and industrial-strength runtime capabilities. By building on the Erlang VM's decades of production experience, Elixir provides a modern language for building systems that need to be concurrent, fault-tolerant, and distributed.

The REST API implementation demonstrates Elixir's strengths: clean syntax, powerful pattern matching, built-in concurrency through GenServer, and excellent error handling through supervisors. While the ecosystem is smaller than mainstream languages, the quality of available libraries and the language's unique capabilities make it an excellent choice for real-time applications, IoT systems, and any application requiring high availability.

For developers willing to embrace functional programming and the actor model, Elixir offers a path to building systems that are not just scalable and maintainable, but also resilient by design. The combination of productivity and reliability makes Elixir a compelling choice for modern distributed systems.