# Elixir gRPC Implementation

This directory contains a gRPC implementation in Elixir using the grpc-elixir library.

## Features

- **Full gRPC service** implementation with all RPC types
- **Unary RPC**: GetTask, CreateTask, UpdateTask, DeleteTask
- **Server streaming**: ListTasks
- **Bidirectional streaming**: WatchTasks
- **GenServer-based repository** for state management
- **Protobuf serialization** with type safety
- **Real-time events** through streaming

## Prerequisites

- Elixir 1.14 or higher
- Mix (comes with Elixir)
- protoc (Protocol Buffers compiler) - optional for regenerating code

## Server

The server implements the TaskService gRPC service with full streaming support.

### Running the Server

```bash
cd server
mix deps.get
mix run --no-halt
```

The server will start on port 50051 by default.

### Service Methods

- `ListTasks` - Server streaming of filtered tasks
- `GetTask` - Unary call to get a single task
- `CreateTask` - Unary call to create a new task
- `UpdateTask` - Unary call with field mask support
- `DeleteTask` - Unary call returning Empty
- `WatchTasks` - Bidirectional streaming for real-time updates

## Client

The client provides both a library interface and a CLI demo.

### Running the Client Demo

```bash
cd client
mix deps.get
mix escript.build
./task_grpc_client
```

### Using the Client Library

```elixir
# Connect to server
channel = TaskGrpcClient.connect("localhost", 50051)

# List tasks (server streaming)
{:ok, tasks} = TaskGrpcClient.list_tasks(channel, 
  status: :TASK_STATUS_PENDING,
  page_size: 10
)

# Create a task
{:ok, task} = TaskGrpcClient.create_task(channel, "New Task",
  priority: :TASK_PRIORITY_HIGH,
  tags: ["important"]
)

# Get a task
{:ok, task} = TaskGrpcClient.get_task(channel, task_id)

# Update a task with field mask
updates = %{status: :TASK_STATUS_COMPLETED}
{:ok, task} = TaskGrpcClient.update_task(channel, task_id, updates, ["status"])

# Delete a task
:ok = TaskGrpcClient.delete_task(channel, task_id)

# Watch for changes (bidirectional streaming)
stream = TaskGrpcClient.watch_tasks(channel, watch_all: true)
Enum.each(stream, fn event ->
  IO.puts "Event: #{event.event_type} for task #{event.task.id}"
end)

# Disconnect
GRPC.Stub.disconnect(channel)
```

## Protocol Buffers

The service is defined in `tasks.proto`. The Elixir code is pre-generated, but you can regenerate it:

```bash
# Install protoc-gen-elixir
mix escript.install hex protobuf

# Generate Elixir code
protoc --elixir_out=plugins=grpc:./lib/proto tasks.proto
```

## Architecture

### Repository Pattern

The server uses a GenServer-based repository for thread-safe state management:

```elixir
defmodule TaskGrpcServer.Repository do
  use GenServer
  
  # Stores tasks in memory
  # Notifies subscribers of changes
  # Handles concurrent access safely
end
```

### Streaming Implementation

Server streaming example:
```elixir
def list_tasks(request, stream) do
  tasks = Repository.list_tasks(filters)
  Enum.each(tasks, fn task ->
    GRPC.Server.send_reply(stream, task)
  end)
end
```

Bidirectional streaming:
```elixir
def watch_tasks(request_stream, stream) do
  # Process incoming requests
  Enum.each(request_stream, fn request ->
    process_watch_request(request)
  end)
  
  # Send outgoing events
  receive do
    {:task_event, event_type, task} ->
      GRPC.Server.send_reply(stream, event)
  end
end
```

## Testing

Run tests with:

```bash
mix test
```

Example test:
```elixir
test "creates and retrieves task" do
  channel = TaskGrpcClient.connect()
  
  {:ok, task} = TaskGrpcClient.create_task(channel, "Test Task")
  assert task.title == "Test Task"
  
  {:ok, retrieved} = TaskGrpcClient.get_task(channel, task.id)
  assert retrieved.id == task.id
  
  GRPC.Stub.disconnect(channel)
end
```

## Performance Considerations

### Connection Pooling

For production use, implement connection pooling:

```elixir
defmodule GrpcPool do
  use GenServer
  
  def get_channel do
    # Return available channel from pool
  end
  
  def return_channel(channel) do
    # Return channel to pool
  end
end
```

### Streaming Best Practices

1. Use server streaming for large result sets
2. Implement backpressure for bidirectional streams
3. Set appropriate deadlines for unary calls
4. Handle disconnections gracefully

## Docker Support

```dockerfile
FROM elixir:1.14-alpine

RUN apk add --no-cache build-base

WORKDIR /app
COPY mix.exs mix.lock ./
RUN mix deps.get && mix deps.compile

COPY . .
RUN mix compile

EXPOSE 50051
CMD ["mix", "run", "--no-halt"]
```

## Monitoring

Add interceptors for logging and metrics:

```elixir
defmodule TaskGrpcServer.Interceptor do
  def init(opts), do: opts
  
  def call(req, stream, next, _opts) do
    start_time = System.monotonic_time()
    
    try do
      result = next.(req, stream)
      log_success(req, start_time)
      result
    rescue
      error ->
        log_error(req, error, start_time)
        reraise error, __STACKTRACE__
    end
  end
end
```

## Error Handling

gRPC status codes are mapped to Elixir errors:

```elixir
case TaskGrpcClient.get_task(channel, id) do
  {:ok, task} -> 
    # Success
  {:error, %GRPC.RPCError{status: :not_found}} ->
    # Handle not found
  {:error, %GRPC.RPCError{status: :invalid_argument, message: msg}} ->
    # Handle validation error
  {:error, error} ->
    # Handle other errors
end
```

## Dependencies

- `grpc` - gRPC implementation for Elixir
- `protobuf` - Protocol Buffers support
- `google_protos` - Google's common proto definitions
- `uuid` - UUID generation