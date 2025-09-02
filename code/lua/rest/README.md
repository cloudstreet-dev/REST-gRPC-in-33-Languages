# Lua REST API Implementation

This directory contains REST API implementations in Lua using two approaches:
1. Pure Lua with lua-http library
2. OpenResty (nginx with embedded Lua)

## Features

- **Full REST API** with CRUD operations
- **In-memory task storage** with thread-safe operations
- **JSON serialization** using cjson
- **CORS support** for cross-origin requests
- **Pagination and filtering** support
- **Two implementations** for different deployment scenarios

## Prerequisites

### For Pure Lua Server
- Lua 5.3 or higher
- LuaRocks package manager
- Required libraries:
  ```bash
  luarocks install http
  luarocks install lua-cjson
  luarocks install uuid
  ```

### For OpenResty Server
- OpenResty installation
- Or Docker with OpenResty image

## Pure Lua Server

The pure Lua implementation uses the lua-http library for HTTP server functionality.

### Running the Server

```bash
cd server
lua server.lua
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

## OpenResty Server

The OpenResty implementation uses nginx with embedded Lua for high-performance serving.

### Running with OpenResty

```bash
cd server
openresty -c nginx.conf
```

### Running with Docker

```bash
docker run -v $(pwd)/server/nginx.conf:/usr/local/openresty/nginx/conf/nginx.conf:ro -p 8080:8080 openresty/openresty
```

## Client

The client provides a comprehensive SDK for interacting with the REST API.

### Running the Client Demo

```bash
cd client
lua client.lua
```

### Using the Client Library

```lua
local Client = require("client")
local client = Client:new()

-- List all tasks
local result = client:list_tasks()

-- Create a task
local task = client:create_task({
    title = "New Task",
    priority = "high",
    tags = {"important"}
})

-- Get a task
local task = client:get_task(task_id)

-- Update a task
local task = client:update_task(task_id, {
    title = "Updated Title"
})

-- Update task status
local task = client:update_task_status(task_id, "completed")

-- Delete a task
local success = client:delete_task(task_id)
```

## Architecture

### Pure Lua Implementation

- Uses coroutine-based async I/O from lua-http
- Manual routing and request handling
- In-memory repository with Lua tables

### OpenResty Implementation

- Leverages nginx's event-driven architecture
- Uses nginx shared dictionary for data storage
- Lua code embedded in nginx configuration

## Lua Features Demonstrated

### Metatables and OOP

```lua
local Task = {}
Task.__index = Task

function Task:new(data)
    local task = setmetatable({}, Task)
    -- Initialize task
    return task
end
```

### Closures and Functional Programming

```lua
local function create_filter(field, value)
    return function(task)
        return task[field] == value
    end
end
```

### Coroutines (in lua-http)

The lua-http library uses coroutines for non-blocking I/O operations.

## Performance Considerations

### Pure Lua Server
- Good for development and small deployments
- Single-threaded by default
- Can handle moderate load

### OpenResty Server
- Production-ready performance
- nginx's event-driven architecture
- Can handle thousands of concurrent connections
- Shared memory for data persistence across workers

## Testing

Run tests with busted:

```bash
luarocks install busted
busted spec/
```

## Docker Support

### Pure Lua Server

```dockerfile
FROM nickblah/lua:5.3-alpine
RUN apk add --no-cache gcc musl-dev openssl-dev
RUN luarocks install http
RUN luarocks install lua-cjson
RUN luarocks install uuid
COPY server/server.lua /app/
WORKDIR /app
EXPOSE 8080
CMD ["lua", "server.lua"]
```

### OpenResty Server

```dockerfile
FROM openresty/openresty:alpine
COPY server/nginx.conf /usr/local/openresty/nginx/conf/nginx.conf
EXPOSE 8080
```

## Development Tips

### Debugging

Enable debug output:
```lua
-- In server.lua
local DEBUG = true

local function debug_log(msg)
    if DEBUG then
        print("[DEBUG] " .. msg)
    end
end
```

### Hot Reloading

For development with OpenResty:
```bash
openresty -c nginx.conf -s reload
```

## Dependencies

### Pure Lua Server
- `lua-http` - HTTP server and client
- `lua-cjson` - JSON encoding/decoding
- `uuid` - UUID generation

### OpenResty Server
- Built-in cjson module
- nginx shared dictionary for storage