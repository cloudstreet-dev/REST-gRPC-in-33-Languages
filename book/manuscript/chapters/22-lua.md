# Chapter 22: Lua - The Embeddable Scripting Language

## Introduction

Lua is a powerful, efficient, lightweight, embeddable scripting language designed with simplicity and flexibility in mind. Created in 1993 at the Pontifical Catholic University of Rio de Janeiro in Brazil, Lua has become the de facto standard for embedded scripting in applications ranging from video games (World of Warcraft, Angry Birds) to network software (nginx/OpenResty) and embedded systems. Its minimal footprint, fast execution, and clean C API make it an ideal choice for extending applications with scripting capabilities.

## About the Lua Programming Language

Lua (Portuguese for "moon") was born from the need for a flexible, embeddable configuration language at Tecgraf, the Computer Graphics Technology Group of PUC-Rio. The language was designed to be simple enough for non-programmers to use while powerful enough for complex scripting tasks. Its entire implementation fits in less than 30,000 lines of C code, making it one of the smallest mainstream scripting languages.

### Language Philosophy

Lua embraces these core principles:
- **Simplicity**: Few, orthogonal constructs
- **Flexibility**: Mechanisms, not policies
- **Portability**: Pure ANSI C implementation
- **Embeddability**: Designed to be embedded in host applications
- **Efficiency**: Fast execution and small footprint
- **Power through Simplicity**: Tables as the sole data structuring mechanism

## REST API Implementation

Our Lua implementation provides two approaches: a pure Lua server using lua-http, and an OpenResty (nginx + Lua) configuration for production use.

### Task Model with Metatables

```lua
local Task = {}
Task.__index = Task

function Task:new(data)
    local task = setmetatable({}, Task)
    task.id = uuid()
    task.title = data.title or ""
    task.description = data.description or ""
    task.status = data.status or "pending"
    task.priority = data.priority or "medium"
    task.tags = data.tags or {}
    task.assigned_to = data.assigned_to or ""
    task.created_at = os.date("!%Y-%m-%dT%H:%M:%SZ")
    task.updated_at = task.created_at
    return task
end

function Task:update(data)
    if data.title then self.title = data.title end
    if data.description then self.description = data.description end
    if data.status then self.status = data.status end
    if data.priority then self.priority = data.priority end
    if data.tags then self.tags = data.tags end
    if data.assigned_to then self.assigned_to = data.assigned_to end
    self.updated_at = os.date("!%Y-%m-%dT%H:%M:%SZ")
end

function Task:to_table()
    return {
        id = self.id,
        title = self.title,
        description = self.description,
        status = self.status,
        priority = self.priority,
        tags = self.tags,
        assigned_to = self.assigned_to,
        created_at = self.created_at,
        updated_at = self.updated_at
    }
end
```

### Pure Lua HTTP Server

```lua
local http_server = require "http.server"
local http_headers = require "http.headers"
local json = require "cjson"

local function handle_request(stream)
    local headers = stream:get_headers()
    local method = headers:get(":method")
    local path = headers:get(":path")
    
    -- Parse path and query string
    local base_path, query_string = path:match("^([^?]+)%??(.*)$")
    local params = parse_query_string(query_string)
    
    -- Route to appropriate handler
    if base_path == "/api/tasks" and method == "GET" then
        local result = repository:list_tasks(params)
        send_json_response(stream, 200, result)
    elseif base_path == "/api/tasks" and method == "POST" then
        local body = stream:get_body_as_string()
        local data = json.decode(body)
        
        if not data.title then
            send_error(stream, 400, "Title is required")
            return
        end
        
        local task = repository:create_task(data)
        send_json_response(stream, 201, task:to_table())
    else
        send_error(stream, 404, "Not found")
    end
end

local server = http_server.listen {
    host = "0.0.0.0",
    port = 8080,
    onstream = handle_request
}

server:listen()
server:loop()
```

### OpenResty Implementation

```nginx
http {
    lua_shared_dict tasks 10m;
    
    server {
        listen 8080;
        
        location /api/tasks {
            content_by_lua_block {
                local cjson = require "cjson"
                local tasks = ngx.shared.tasks
                
                if ngx.req.get_method() == "GET" then
                    local keys = tasks:get_keys()
                    local result = {}
                    
                    for _, key in ipairs(keys) do
                        local task_json = tasks:get(key)
                        if task_json then
                            table.insert(result, cjson.decode(task_json))
                        end
                    end
                    
                    ngx.say(cjson.encode({
                        tasks = result,
                        total_count = #result
                    }))
                    
                elseif ngx.req.get_method() == "POST" then
                    ngx.req.read_body()
                    local body = ngx.req.get_body_data()
                    local data = cjson.decode(body)
                    
                    -- Create and store task
                    local task = create_task(data)
                    tasks:set(task.id, cjson.encode(task))
                    
                    ngx.status = 201
                    ngx.say(cjson.encode(task))
                end
            }
        }
    }
}
```

## Tables: Lua's Swiss Army Knife

Tables are Lua's only data structuring mechanism, yet they're powerful enough to represent arrays, dictionaries, objects, and more.

### Arrays and Lists

```lua
-- Array (1-indexed by convention)
local fruits = {"apple", "banana", "orange"}
print(fruits[1])  -- "apple"

-- Adding elements
table.insert(fruits, "grape")
fruits[#fruits + 1] = "mango"

-- Iteration
for i, fruit in ipairs(fruits) do
    print(i, fruit)
end

-- Array operations
table.sort(fruits)
table.remove(fruits, 2)  -- Remove second element
```

### Dictionaries and Records

```lua
-- Dictionary/hash table
local person = {
    name = "Alice",
    age = 30,
    email = "alice@example.com"
}

-- Alternative syntax
person["city"] = "New York"
person.country = "USA"

-- Iteration over key-value pairs
for key, value in pairs(person) do
    print(key, "=", value)
end

-- Nested structures
local config = {
    server = {
        host = "localhost",
        port = 8080
    },
    database = {
        url = "postgresql://localhost/mydb"
    }
}
```

### Tables as Objects

```lua
-- Object creation with metatables
local Vector = {}
Vector.__index = Vector

function Vector:new(x, y)
    return setmetatable({x = x, y = y}, Vector)
end

function Vector:length()
    return math.sqrt(self.x^2 + self.y^2)
end

function Vector:__add(other)
    return Vector:new(self.x + other.x, self.y + other.y)
end

function Vector:__tostring()
    return string.format("Vector(%g, %g)", self.x, self.y)
end

-- Usage
local v1 = Vector:new(3, 4)
local v2 = Vector:new(1, 2)
local v3 = v1 + v2
print(v3)  -- Vector(4, 6)
print(v1:length())  -- 5
```

## Metatables and Metamethods

Metatables allow you to change the behavior of tables, enabling operator overloading and object-oriented programming.

### Common Metamethods

```lua
local Set = {}
Set.__index = Set

function Set:new(list)
    local set = setmetatable({}, Set)
    for _, v in ipairs(list or {}) do
        set[v] = true
    end
    return set
end

-- Union operation
function Set:__add(other)
    local union = Set:new()
    for k in pairs(self) do union[k] = true end
    for k in pairs(other) do union[k] = true end
    return union
end

-- Intersection operation
function Set:__mul(other)
    local intersection = Set:new()
    for k in pairs(self) do
        if other[k] then
            intersection[k] = true
        end
    end
    return intersection
end

-- String representation
function Set:__tostring()
    local list = {}
    for k in pairs(self) do
        table.insert(list, tostring(k))
    end
    return "{" .. table.concat(list, ", ") .. "}"
end

-- Usage
local s1 = Set:new{1, 2, 3}
local s2 = Set:new{2, 3, 4}
print(s1 + s2)  -- {1, 2, 3, 4}
print(s1 * s2)  -- {2, 3}
```

### Proxy Tables and Access Control

```lua
function createProxy(target)
    local proxy = {}
    local mt = {
        __index = function(_, key)
            print("Reading", key)
            return target[key]
        end,
        
        __newindex = function(_, key, value)
            print("Writing", key, "=", value)
            target[key] = value
        end,
        
        __pairs = function()
            return pairs(target)
        end,
        
        __len = function()
            return #target
        end
    }
    
    setmetatable(proxy, mt)
    return proxy
end

-- Usage
local data = {x = 10, y = 20}
local proxy = createProxy(data)
print(proxy.x)  -- Reading x \n 10
proxy.z = 30    -- Writing z = 30
```

## Closures and First-Class Functions

Functions in Lua are first-class values with proper lexical scoping.

### Closures for State Management

```lua
function createCounter(initial)
    local count = initial or 0
    
    return {
        increment = function()
            count = count + 1
            return count
        end,
        
        decrement = function()
            count = count - 1
            return count
        end,
        
        get = function()
            return count
        end,
        
        reset = function()
            count = initial or 0
        end
    }
end

local counter = createCounter(10)
print(counter.increment())  -- 11
print(counter.increment())  -- 12
print(counter.get())        -- 12
```

### Higher-Order Functions

```lua
function map(fn, list)
    local result = {}
    for i, v in ipairs(list) do
        result[i] = fn(v)
    end
    return result
end

function filter(fn, list)
    local result = {}
    for _, v in ipairs(list) do
        if fn(v) then
            table.insert(result, v)
        end
    end
    return result
end

function reduce(fn, list, initial)
    local acc = initial
    for _, v in ipairs(list) do
        acc = fn(acc, v)
    end
    return acc
end

-- Usage
local numbers = {1, 2, 3, 4, 5}
local doubled = map(function(x) return x * 2 end, numbers)
local evens = filter(function(x) return x % 2 == 0 end, numbers)
local sum = reduce(function(a, b) return a + b end, numbers, 0)
```

## Coroutines

Lua's coroutines provide cooperative multitasking and are the foundation for many async frameworks.

### Basic Coroutines

```lua
function producer()
    local i = 0
    while true do
        i = i + 1
        coroutine.yield(i)
        if i >= 5 then break end
    end
end

function consumer()
    local co = coroutine.create(producer)
    
    while coroutine.status(co) ~= "dead" do
        local ok, value = coroutine.resume(co)
        if ok then
            print("Consumed:", value)
        end
    end
end

consumer()
```

### Coroutines for Iteration

```lua
function permutations(list)
    local function permgen(list, n)
        n = n or #list
        if n <= 1 then
            coroutine.yield(list)
        else
            for i = 1, n do
                list[i], list[n] = list[n], list[i]
                permgen(list, n - 1)
                list[i], list[n] = list[n], list[i]
            end
        end
    end
    
    return coroutine.wrap(function() permgen(list) end)
end

-- Usage
for perm in permutations{"a", "b", "c"} do
    print(table.concat(perm, ", "))
end
```

### Async-Style Programming

```lua
function async_request(url, callback)
    local co = coroutine.running()
    
    -- Simulate async operation
    setTimeout(function()
        local response = http.get(url)
        if co then
            coroutine.resume(co, response)
        elseif callback then
            callback(response)
        end
    end, 0)
    
    if co then
        return coroutine.yield()
    end
end

function main()
    local response = async_request("http://api.example.com/data")
    print("Got response:", response)
end

coroutine.wrap(main)()
```

## Module System

Lua's module system is simple yet flexible.

### Creating Modules

```lua
-- mymodule.lua
local M = {}

-- Private variables and functions
local private_data = "hidden"

local function private_function()
    return "private"
end

-- Public interface
function M.public_function()
    return private_function() .. " made public"
end

M.version = "1.0.0"

-- Object constructor
function M:new(options)
    local instance = setmetatable({}, {__index = self})
    instance.options = options or {}
    return instance
end

return M
```

### Using Modules

```lua
-- Using the module
local mymodule = require("mymodule")

print(mymodule.version)
local result = mymodule.public_function()

-- Create instance
local obj = mymodule:new({debug = true})
```

## Error Handling

Lua provides protected calls and error handling mechanisms.

### Protected Calls

```lua
function risky_operation(x)
    if type(x) ~= "number" then
        error("Expected number, got " .. type(x))
    end
    return x * 2
end

-- Using pcall
local ok, result = pcall(risky_operation, "not a number")
if ok then
    print("Result:", result)
else
    print("Error:", result)
end

-- Using xpcall with error handler
local function error_handler(err)
    print("Error occurred:", err)
    print(debug.traceback())
end

xpcall(function()
    risky_operation("invalid")
end, error_handler)
```

### Custom Error Objects

```lua
local Error = {}
Error.__index = Error

function Error:new(message, code)
    return setmetatable({
        message = message,
        code = code or 500,
        timestamp = os.time()
    }, Error)
end

function Error:__tostring()
    return string.format("[%d] %s", self.code, self.message)
end

-- Usage
function validate_input(data)
    if not data.email then
        error(Error:new("Email is required", 400))
    end
end

local ok, err = pcall(validate_input, {})
if not ok then
    print(tostring(err))
end
```

## Pattern Matching

Lua's pattern matching is simpler than full regex but sufficient for many tasks.

### Basic Patterns

```lua
-- Simple matching
local email = "user@example.com"
if email:match("^[%w.]+@[%w.]+%.%w+$") then
    print("Valid email format")
end

-- Captures
local date = "2024-03-15"
local year, month, day = date:match("(%d+)-(%d+)-(%d+)")

-- String substitution
local text = "Hello, {name}! Welcome to {place}."
local result = text:gsub("{(%w+)}", {
    name = "Alice",
    place = "Wonderland"
})
print(result)  -- Hello, Alice! Welcome to Wonderland.

-- Multiple captures
local log = "2024-03-15 10:30:45 [ERROR] Database connection failed"
local date, time, level, message = log:match(
    "([%d-]+) ([%d:]+) %[(%w+)%] (.+)"
)
```

### Advanced Pattern Operations

```lua
-- Iterator over matches
local text = "The price is $10.50 and tax is $2.10"
for amount in text:gmatch("$([%d.]+)") do
    print("Amount:", amount)
end

-- Position-based matching
local str = "Lua is a powerful language"
local start, finish = str:find("powerful")
if start then
    print("Found at position", start, "to", finish)
end

-- Custom split function
function split(str, delimiter)
    local result = {}
    local pattern = string.format("([^%s]+)", delimiter)
    for match in str:gmatch(pattern) do
        table.insert(result, match)
    end
    return result
end

local parts = split("apple,banana,orange", ",")
```

## C API and Embedding

Lua's clean C API makes it ideal for embedding in applications.

### Embedding Lua in C

```c
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

int main() {
    lua_State *L = luaL_newstate();
    luaL_openlibs(L);
    
    // Execute Lua code
    luaL_dostring(L, "print('Hello from Lua!')");
    
    // Push values to Lua
    lua_pushstring(L, "example");
    lua_setglobal(L, "myvar");
    
    // Call Lua function
    luaL_dostring(L, "function add(a, b) return a + b end");
    lua_getglobal(L, "add");
    lua_pushnumber(L, 10);
    lua_pushnumber(L, 20);
    lua_call(L, 2, 1);
    
    double result = lua_tonumber(L, -1);
    printf("Result: %f\n", result);
    
    lua_close(L);
    return 0;
}
```

### Extending Lua with C

```c
static int l_sqrt(lua_State *L) {
    double n = luaL_checknumber(L, 1);
    lua_pushnumber(L, sqrt(n));
    return 1;  // Number of return values
}

static const struct luaL_Reg mathlib[] = {
    {"sqrt", l_sqrt},
    {NULL, NULL}
};

int luaopen_mymath(lua_State *L) {
    luaL_newlib(L, mathlib);
    return 1;
}
```

## Performance Optimization

### LuaJIT

LuaJIT provides just-in-time compilation for dramatic performance improvements:

```lua
-- FFI for C interop
local ffi = require("ffi")

ffi.cdef[[
    typedef struct { double x, y; } point_t;
    double distance(point_t *a, point_t *b);
]]

local point_t = ffi.typeof("point_t")
local p1 = point_t(3, 4)
local p2 = point_t(0, 0)

-- Direct C function call
local dist = ffi.C.distance(p1, p2)
```

### Optimization Tips

```lua
-- Localize frequently used functions
local insert = table.insert
local format = string.format

-- Preallocate tables
local data = {}
for i = 1, 1000000 do
    data[i] = false  -- Preallocate
end

-- Avoid table creation in loops
-- Bad
for i = 1, n do
    process({x = i, y = i * 2})
end

-- Better
local temp = {}
for i = 1, n do
    temp.x = i
    temp.y = i * 2
    process(temp)
end
```

## Lua vs Other Languages

### Lua vs Python
- **Lua Advantages**: Smaller, faster, better embedding API
- **Python Advantages**: Larger ecosystem, more libraries, broader use
- **Use Lua when**: Embedding scripting, game development, minimal footprint
- **Use Python when**: Data science, web development, general scripting

### Lua vs JavaScript
- **Lua Advantages**: Simpler language, smaller runtime, true integers
- **JavaScript Advantages**: Ubiquitous in web, larger ecosystem
- **Use Lua when**: Embedding in applications, game scripting
- **Use JavaScript when**: Web development, Node.js applications

### Lua vs Ruby
- **Lua Advantages**: Much faster, smaller footprint, simpler syntax
- **Ruby Advantages**: Richer standard library, Rails ecosystem
- **Use Lua when**: Performance matters, embedding needed
- **Use Ruby when**: Rapid web development, DSL creation

## Real-World Applications

### Game Development

Lua is widely used in game engines:
- World of Warcraft addons
- Roblox game scripts
- Love2D game framework
- Corona SDK for mobile games

### Network Applications

- OpenResty: nginx with Lua for high-performance web applications
- HAProxy: Configuration and extending with Lua
- Wireshark: Protocol dissectors in Lua

### Embedded Systems

- NodeMCU: Lua firmware for ESP8266/ESP32
- eLua: Embedded Lua for microcontrollers
- OpenWrt: Router firmware configuration

## gRPC Considerations for Lua

While Lua has some gRPC support through third-party libraries, the ecosystem is limited and not production-ready for most use cases.

### Available Options

#### lua-grpc

The `lua-grpc` library provides basic gRPC support but has significant limitations:

```lua
-- Theoretical implementation using lua-grpc
local grpc = require("grpc")
local pb = require("pb")

-- Load protobuf definitions
pb.loadfile("task.pb")

-- Create service implementation
local TaskService = {}

function TaskService:ListTasks(request, context)
    local tasks = {}
    
    -- Filter tasks based on request
    for _, task in ipairs(self.repository:get_all()) do
        if not request.status or task.status == request.status then
            table.insert(tasks, task)
        end
    end
    
    return {tasks = tasks}
end

-- Start server (simplified)
local server = grpc.server()
server:add_service("tasks.v1.TaskService", TaskService)
server:start("0.0.0.0:50051")
```

#### OpenResty with gRPC Gateway

A more practical approach is using OpenResty as a gRPC-to-REST gateway:

```nginx
location /grpc-gateway {
    content_by_lua_block {
        local cjson = require "cjson"
        local http = require "resty.http"
        
        -- Parse gRPC-Web request
        ngx.req.read_body()
        local body = ngx.req.get_body_data()
        
        -- Convert to REST call
        local httpc = http.new()
        local res, err = httpc:request_uri("http://grpc-backend:50051", {
            method = "POST",
            headers = {
                ["Content-Type"] = "application/grpc+json",
            },
            body = body
        })
        
        if res then
            ngx.say(res.body)
        else
            ngx.status = 502
            ngx.say(cjson.encode({error = "Gateway error"}))
        end
    }
}
```

### Protocol Buffers in Lua

Lua has better support for Protocol Buffers than full gRPC:

```lua
local pb = require "pb"
local protoc = require "protoc"

-- Load proto definition
assert(protoc:load [[
    syntax = "proto3";
    package tasks.v1;
    
    message Task {
        string id = 1;
        string title = 2;
        string description = 3;
        string status = 4;
        string priority = 5;
        repeated string tags = 6;
        string assigned_to = 7;
        string created_at = 8;
        string updated_at = 9;
    }
]])

-- Serialize
local task_data = {
    id = "task-123",
    title = "Example Task",
    status = "pending",
    tags = {"lua", "grpc"}
}

local bytes = assert(pb.encode("tasks.v1.Task", task_data))

-- Deserialize
local task = assert(pb.decode("tasks.v1.Task", bytes))
print(task.title)  -- "Example Task"
```

### FFI-based gRPC with LuaJIT

LuaJIT's FFI can theoretically interface with gRPC's C++ library:

```lua
local ffi = require("ffi")

-- Define C++ gRPC interface (simplified)
ffi.cdef[[
    typedef struct grpc_server grpc_server;
    typedef struct grpc_completion_queue grpc_completion_queue;
    
    grpc_server* grpc_server_create(void* args);
    void grpc_server_start(grpc_server* server);
    void grpc_server_shutdown(grpc_server* server);
]]

-- Load gRPC library
local grpc = ffi.load("grpc")

-- This approach requires extensive C++ binding work
-- and is not practical for most use cases
```

### Why gRPC is Challenging in Lua

1. **HTTP/2 Support**: Lua's HTTP libraries generally lack HTTP/2 support
2. **Streaming**: Bidirectional streaming requires complex coroutine coordination
3. **Code Generation**: No official protoc plugin for Lua service generation
4. **Library Maturity**: Existing libraries are experimental or abandoned
5. **Async I/O**: Limited async networking support in standard Lua

### Alternative RPC Solutions for Lua

#### JSON-RPC

```lua
local json_rpc = require("json-rpc")

-- Server
local server = json_rpc.server()

server:register("listTasks", function(params)
    return repository:list_tasks(params)
end)

server:listen(8080)

-- Client
local client = json_rpc.client("http://localhost:8080")
local result = client:call("listTasks", {status = "pending"})
```

#### MessagePack-RPC

```lua
local msgpack = require("msgpack")
local socket = require("socket")

-- Efficient binary protocol
function rpc_call(method, params)
    local request = msgpack.pack({
        type = "request",
        method = method,
        params = params,
        id = generate_id()
    })
    
    local sock = socket.tcp()
    sock:connect("localhost", 9090)
    sock:send(request)
    
    local response = sock:receive("*a")
    return msgpack.unpack(response)
end
```

#### REST with Protocol Buffers

```lua
-- Combine REST endpoints with protobuf serialization
local function handle_request(path, method, body)
    if path == "/api/tasks" and method == "POST" then
        -- Deserialize protobuf request
        local task_request = pb.decode("CreateTaskRequest", body)
        
        -- Process
        local task = create_task(task_request)
        
        -- Serialize protobuf response
        return pb.encode("Task", task), "application/x-protobuf"
    end
end
```

### Recommendations

For Lua projects requiring RPC:

1. **Use REST**: Mature HTTP libraries and OpenResty for production
2. **JSON-RPC or MessagePack-RPC**: Simpler than gRPC, good Lua support
3. **gRPC Gateway**: Use a gateway to translate gRPC to REST
4. **Protocol Buffers Only**: Use protobuf for serialization without full gRPC
5. **Consider Alternative Languages**: For gRPC-heavy services

### Future Possibilities

A proper Lua gRPC implementation would need:
- Native HTTP/2 support in Lua networking libraries
- Official protoc plugin for Lua
- Coroutine-based streaming abstractions
- Better async I/O primitives

Until these foundations exist, Lua developers should choose RPC solutions that align with the language's strengths: simplicity, embedding, and lightweight operation.

## Best Practices

1. **Use local variables**: They're faster than globals
2. **Leverage metatables**: But don't overuse them
3. **Understand nil**: It represents absence of value
4. **Use proper idioms**: Learn Lua way, not forcing other languages' patterns
5. **Profile before optimizing**: Lua is often fast enough
6. **Document module interfaces**: Clear public API
7. **Handle errors gracefully**: Use pcall for robust code
8. **Consider LuaJIT**: For performance-critical applications

## Conclusion

Lua proves that a language doesn't need to be complex to be powerful. Its elegant design, centered around tables and first-class functions, provides remarkable flexibility while maintaining simplicity. The language's small footprint and clean C API have made it the go-to choice for embedded scripting in countless applications.

Our REST API implementation demonstrates Lua's versatility, from pure Lua servers suitable for development to OpenResty configurations capable of handling production traffic. While Lua may not have the extensive library ecosystem of larger languages, its focused design and excellent performance make it ideal for specific domains like game development, embedded systems, and high-performance web applications.

For developers seeking a lightweight, fast, and embeddable scripting language, Lua offers an unmatched combination of simplicity and power. Its influence can be seen in many modern languages, and its continued use in performance-critical applications proves that sometimes, less truly is more.