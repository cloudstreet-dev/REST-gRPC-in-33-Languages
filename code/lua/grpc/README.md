# Lua gRPC Implementation

This directory contains a gRPC implementation for Lua. Due to Lua's nature as an embedded language, gRPC support requires compilation of C/C++ bindings.

## Prerequisites

### Option 1: Using lua-grpc (Recommended)
```bash
# Install dependencies
apt-get install -y protobuf-compiler libprotobuf-dev
luarocks install grpc
luarocks install lua-protobuf
```

### Option 2: Using OpenResty with gRPC module
```bash
# OpenResty provides nginx with Lua and can proxy gRPC
apt-get install openresty
opm get thibaultcha/lua-resty-grpc-gateway
```

## Implementation Notes

Due to the complexity of gRPC in Lua requiring compiled C extensions, this implementation provides:

1. **Example server structure** showing how a Lua gRPC server would be organized
2. **Example client structure** demonstrating gRPC client patterns
3. **nginx configuration** for proxying gRPC with OpenResty

## Server Implementation (Conceptual)

```lua
-- server.lua (requires lua-grpc)
local grpc = require "grpc"
local pb = require "pb"

-- Load proto file
pb.loadfile("tasks.proto")

-- Task service implementation
local TaskService = {}

function TaskService.GetTask(request)
    local task = repository:get(request.id)
    if not task then
        return nil, grpc.status.NOT_FOUND
    end
    return task
end

function TaskService.CreateTask(request)
    local task = repository:create(request.task)
    return task
end

function TaskService.ListTasks(request, writer)
    -- Server streaming
    local tasks = repository:list(request)
    for _, task in ipairs(tasks) do
        writer(task)
    end
end

-- Start server
local server = grpc.server()
server:add_service(TaskService)
server:start("0.0.0.0:50051")
```

## Client Implementation (Conceptual)

```lua
-- client.lua (requires lua-grpc)
local grpc = require "grpc"
local pb = require "pb"

pb.loadfile("tasks.proto")

-- Connect to server
local channel = grpc.channel("localhost:50051")
local client = grpc.client(channel, "tasks.v1.TaskService")

-- Make unary call
local task = client:GetTask({id = "task-123"})
print("Task:", task.title)

-- Server streaming
local stream = client:ListTasks({page_size = 10})
for task in stream do
    print("Received:", task.id, task.title)
end
```

## OpenResty gRPC Proxy Configuration

```nginx
# nginx.conf for OpenResty gRPC proxy
http {
    upstream grpc_backend {
        server localhost:50051;
    }
    
    server {
        listen 8080 http2;
        
        location /tasks.v1.TaskService {
            grpc_pass grpc://grpc_backend;
            
            # Convert to REST-like API
            content_by_lua_block {
                local grpc_gateway = require "resty.grpc-gateway"
                
                -- Map REST to gRPC
                local gateway = grpc_gateway.new({
                    proto = "tasks.proto",
                    service = "tasks.v1.TaskService"
                })
                
                gateway:handle()
            }
        }
    }
}
```

## Building from Source

If you need to build lua-grpc from source:

```bash
# Clone lua-grpc
git clone https://github.com/Neopallium/lua-grpc.git
cd lua-grpc

# Build
mkdir build && cd build
cmake ..
make

# Install
make install
```

## Alternative: REST-to-gRPC Gateway

For production Lua applications, consider using a REST-to-gRPC gateway:

1. Implement gRPC server in a language with better gRPC support (Go, Python, etc.)
2. Use Lua for REST API that communicates with the gRPC server
3. Or use nginx/OpenResty as a gRPC proxy

## Docker Support

```dockerfile
# Using OpenResty for gRPC proxy
FROM openresty/openresty:alpine

RUN opm get thibaultcha/lua-resty-grpc-gateway

COPY nginx.conf /usr/local/openresty/nginx/conf/nginx.conf
COPY *.proto /etc/grpc/

EXPOSE 8080
```

## Testing with grpcurl

```bash
# List services
grpcurl -plaintext localhost:50051 list

# Describe service
grpcurl -plaintext localhost:50051 describe tasks.v1.TaskService

# Make request
grpcurl -plaintext -d '{"id": "task-123"}' \
    localhost:50051 tasks.v1.TaskService/GetTask
```

## Limitations

1. **Native gRPC support in Lua is limited** - Requires C/C++ bindings
2. **Not all gRPC features supported** - Some streaming modes may not work
3. **Performance overhead** - Lua bridge adds latency
4. **Limited ecosystem** - Fewer libraries compared to other languages

## Recommendations

For production gRPC services with Lua:

1. **Use OpenResty** as a gRPC proxy/gateway
2. **Implement core logic in Lua**, gRPC handling in Go/Python
3. **Consider REST APIs** with JSON for simpler integration
4. **Use MessagePack** instead of Protobuf for Lua-native serialization

## Resources

- [lua-grpc](https://github.com/Neopallium/lua-grpc) - gRPC for Lua
- [OpenResty](https://openresty.org/) - nginx with Lua
- [lua-protobuf](https://github.com/starwing/lua-protobuf) - Protobuf for Lua
- [grpc-gateway](https://github.com/grpc-ecosystem/grpc-gateway) - REST to gRPC