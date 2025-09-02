# Elixir REST API Implementation

This directory contains a REST API implementation in Elixir using Plug and Cowboy for the server, and HTTPoison for the client.

## Features

- **Full REST API** with CRUD operations
- **GenServer-based repository** for concurrent task management
- **Plug router** for HTTP request handling
- **HTTPoison client** for API consumption
- **Functional programming** with pattern matching
- **OTP supervision** for fault tolerance

## Prerequisites

- Elixir 1.14 or higher
- Mix (comes with Elixir)

## Server

The server implements a complete REST API using Plug and Cowboy.

### Running the Server

```bash
cd server
mix deps.get
mix run --no-halt
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks (with filtering and pagination)
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in_progress, completed, cancelled)
- `assigned_to` - Filter by assignee
- `tags` - Filter by tags (comma-separated)
- `page_size` - Number of results per page (default: 20, max: 100)
- `page_token` - Pagination token
- `sort_by` - Sort field (created_at, updated_at, title)
- `sort_order` - Sort order (asc, desc)

## Client

The client provides a comprehensive SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
mix deps.get
mix escript.build
./task_client
```

### Using the Client Library

```elixir
# List all tasks
{:ok, result} = TaskClient.list_tasks()

# Create a task
{:ok, task} = TaskClient.create_task(%{
  title: "New Task",
  priority: "high",
  tags: ["important"]
})

# Get a task
{:ok, task} = TaskClient.get_task(task_id)

# Update a task
{:ok, task} = TaskClient.update_task(task_id, %{
  title: "Updated Title"
})

# Update task status
{:ok, task} = TaskClient.update_task_status(task_id, "completed")

# Delete a task
:ok = TaskClient.delete_task(task_id)
```

## Architecture

### GenServer Repository

The repository uses OTP GenServer for state management:
- Thread-safe operations
- Supervised process
- In-memory storage with atomic updates

### Plug Pipeline

The server uses Plug's composable middleware:
1. CORS handling
2. Request parsing
3. Route matching
4. Response formatting

### Error Handling

- Pattern matching for error cases
- Proper HTTP status codes
- Graceful error recovery with supervisors

## Testing

Run tests with:

```bash
mix test
```

## Docker Support

Build and run with Docker:

```bash
# Build image
docker build -t elixir-rest-api .

# Run container
docker run -p 8080:8080 elixir-rest-api
```

## Performance

Elixir's actor model provides excellent concurrency:
- Lightweight processes (millions possible)
- Fault-tolerant with supervisors
- Efficient message passing
- Preemptive scheduling

## Development

### Live Reloading

For development with auto-reloading:

```bash
mix run --no-halt
# In another terminal:
iex -S mix
```

### Interactive Console

Connect to running server:

```bash
iex -S mix
# Interact with the repository
TaskServer.Repository.list_tasks()
```

## Dependencies

### Server
- `plug_cowboy` - HTTP server
- `jason` - JSON encoding/decoding
- `cors_plug` - CORS support
- `uuid` - UUID generation

### Client
- `httpoison` - HTTP client
- `jason` - JSON parsing