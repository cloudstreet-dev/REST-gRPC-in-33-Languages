#!/usr/bin/env lua

-- Task Management REST API Server
-- Using lua-http library for HTTP server

local http_server = require "http.server"
local http_headers = require "http.headers"
local json = require "cjson"
local uuid = require "uuid"

-- Initialize UUID
uuid.randomseed(os.time())

-- Task model
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

-- Repository
local Repository = {}
Repository.__index = Repository

function Repository:new()
    local repo = setmetatable({}, Repository)
    repo.tasks = {}
    repo:load_sample_data()
    return repo
end

function Repository:load_sample_data()
    local task1 = Task:new({
        title = "Implement Lua REST API",
        description = "Build a REST API server using lua-http",
        status = "in_progress",
        priority = "high",
        tags = {"lua", "rest", "api"},
        assigned_to = "backend-team"
    })
    self.tasks[task1.id] = task1
    
    local task2 = Task:new({
        title = "Add OpenResty support",
        description = "Implement nginx-based Lua server with OpenResty",
        status = "pending",
        priority = "medium",
        tags = {"lua", "nginx", "openresty"},
        assigned_to = "devops-team"
    })
    self.tasks[task2.id] = task2
    
    local task3 = Task:new({
        title = "Write Busted tests",
        description = "Add comprehensive test coverage using Busted framework",
        status = "pending",
        priority = "high",
        tags = {"testing", "quality"},
        assigned_to = "qa-team"
    })
    self.tasks[task3.id] = task3
end

function Repository:list_tasks(filters)
    local tasks = {}
    for _, task in pairs(self.tasks) do
        local include = true
        
        -- Filter by status
        if filters.status and task.status ~= filters.status then
            include = false
        end
        
        -- Filter by assigned_to
        if filters.assigned_to and task.assigned_to ~= filters.assigned_to then
            include = false
        end
        
        -- Filter by tags
        if filters.tags then
            for _, required_tag in ipairs(filters.tags) do
                local has_tag = false
                for _, task_tag in ipairs(task.tags) do
                    if task_tag == required_tag then
                        has_tag = true
                        break
                    end
                end
                if not has_tag then
                    include = false
                    break
                end
            end
        end
        
        if include then
            table.insert(tasks, task:to_table())
        end
    end
    
    -- Sort tasks
    local sort_by = filters.sort_by or "created_at"
    local sort_order = filters.sort_order or "desc"
    
    table.sort(tasks, function(a, b)
        if sort_order == "asc" then
            return a[sort_by] < b[sort_by]
        else
            return a[sort_by] > b[sort_by]
        end
    end)
    
    -- Pagination
    local page_size = math.min(filters.page_size or 20, 100)
    local page_token = filters.page_token or 0
    local total = #tasks
    local start_idx = page_token + 1
    local end_idx = math.min(start_idx + page_size - 1, total)
    
    local paginated = {}
    for i = start_idx, end_idx do
        table.insert(paginated, tasks[i])
    end
    
    local next_token = nil
    if end_idx < total then
        next_token = end_idx
    end
    
    return {
        tasks = paginated,
        total_count = total,
        page_size = page_size,
        next_page_token = next_token
    }
end

function Repository:get_task(id)
    return self.tasks[id]
end

function Repository:create_task(data)
    local task = Task:new(data)
    self.tasks[task.id] = task
    return task
end

function Repository:update_task(id, data)
    local task = self.tasks[id]
    if task then
        task:update(data)
        return task
    end
    return nil
end

function Repository:delete_task(id)
    if self.tasks[id] then
        self.tasks[id] = nil
        return true
    end
    return false
end

function Repository:count()
    local count = 0
    for _ in pairs(self.tasks) do
        count = count + 1
    end
    return count
end

-- Initialize repository
local repository = Repository:new()

-- Helper functions
local function parse_query_string(query)
    local params = {}
    if query then
        for k, v in query:gmatch("([^&=]+)=([^&]+)") do
            params[k] = v:gsub("%%20", " "):gsub("%%2C", ",")
        end
    end
    return params
end

local function send_json_response(stream, status, data)
    local response_headers = http_headers.new()
    response_headers:append(":status", tostring(status))
    response_headers:append("content-type", "application/json")
    response_headers:append("access-control-allow-origin", "*")
    response_headers:append("access-control-allow-methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS")
    response_headers:append("access-control-allow-headers", "Content-Type")
    
    stream:write_headers(response_headers, false)
    stream:write_body_from_string(json.encode(data))
end

local function send_error(stream, status, message)
    send_json_response(stream, status, {error = message})
end

-- Route handlers
local function handle_request(stream)
    local headers = stream:get_headers()
    local method = headers:get(":method")
    local path = headers:get(":path")
    
    -- Parse path and query string
    local base_path, query_string = path:match("^([^?]+)%??(.*)$")
    local params = parse_query_string(query_string)
    
    -- Handle OPTIONS for CORS
    if method == "OPTIONS" then
        local response_headers = http_headers.new()
        response_headers:append(":status", "204")
        response_headers:append("access-control-allow-origin", "*")
        response_headers:append("access-control-allow-methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS")
        response_headers:append("access-control-allow-headers", "Content-Type")
        stream:write_headers(response_headers, true)
        return
    end
    
    -- Health check
    if base_path == "/health" and method == "GET" then
        send_json_response(stream, 200, {
            status = "healthy",
            service = "lua-task-api",
            task_count = repository:count()
        })
        return
    end
    
    -- List tasks
    if base_path == "/api/tasks" and method == "GET" then
        local filters = {
            status = params.status,
            assigned_to = params.assigned_to,
            page_size = tonumber(params.page_size),
            page_token = tonumber(params.page_token),
            sort_by = params.sort_by,
            sort_order = params.sort_order
        }
        
        if params.tags then
            filters.tags = {}
            for tag in params.tags:gmatch("[^,]+") do
                table.insert(filters.tags, tag)
            end
        end
        
        local result = repository:list_tasks(filters)
        send_json_response(stream, 200, result)
        return
    end
    
    -- Get specific task
    local task_id = base_path:match("^/api/tasks/([^/]+)$")
    if task_id and method == "GET" then
        local task = repository:get_task(task_id)
        if task then
            send_json_response(stream, 200, task:to_table())
        else
            send_error(stream, 404, "Task not found")
        end
        return
    end
    
    -- Create task
    if base_path == "/api/tasks" and method == "POST" then
        local body = stream:get_body_as_string()
        local ok, data = pcall(json.decode, body)
        
        if not ok or not data.title then
            send_error(stream, 400, "Title is required")
            return
        end
        
        local task = repository:create_task(data)
        send_json_response(stream, 201, task:to_table())
        return
    end
    
    -- Update task
    if task_id and method == "PUT" then
        local body = stream:get_body_as_string()
        local ok, data = pcall(json.decode, body)
        
        if not ok then
            send_error(stream, 400, "Invalid JSON")
            return
        end
        
        local task = repository:update_task(task_id, data)
        if task then
            send_json_response(stream, 200, task:to_table())
        else
            send_error(stream, 404, "Task not found")
        end
        return
    end
    
    -- Update task status
    local status_id = base_path:match("^/api/tasks/([^/]+)/status$")
    if status_id and method == "PATCH" then
        local body = stream:get_body_as_string()
        local ok, data = pcall(json.decode, body)
        
        if not ok or not data.status then
            send_error(stream, 400, "Status is required")
            return
        end
        
        local task = repository:update_task(status_id, {status = data.status})
        if task then
            send_json_response(stream, 200, task:to_table())
        else
            send_error(stream, 404, "Task not found")
        end
        return
    end
    
    -- Delete task
    if task_id and method == "DELETE" then
        if repository:delete_task(task_id) then
            local response_headers = http_headers.new()
            response_headers:append(":status", "204")
            response_headers:append("access-control-allow-origin", "*")
            stream:write_headers(response_headers, true)
        else
            send_error(stream, 404, "Task not found")
        end
        return
    end
    
    -- Not found
    send_error(stream, 404, "Not found")
end

-- Main server
local function main()
    print([[
╔════════════════════════════════════════════════╗
║          Lua Task Management REST API          ║
║            Built with lua-http                 ║
╚════════════════════════════════════════════════╝
]])
    
    local port = os.getenv("PORT") or 8080
    
    print(string.format("[INFO] Lua Task REST Server starting on port %d", port))
    print(string.format("[INFO] Visit http://localhost:%d/api/tasks\n", port))
    
    print("Available endpoints:")
    print("  GET    /api/tasks          - List all tasks")
    print("  GET    /api/tasks/{id}     - Get a specific task")
    print("  POST   /api/tasks          - Create a new task")
    print("  PUT    /api/tasks/{id}     - Update a task")
    print("  PATCH  /api/tasks/{id}/status - Update task status")
    print("  DELETE /api/tasks/{id}     - Delete a task")
    print("  GET    /health             - Health check\n")
    
    print("Sample requests:")
    print(string.format("  curl http://localhost:%d/api/tasks", port))
    print(string.format("  curl -X POST http://localhost:%d/api/tasks \\", port))
    print('    -H "Content-Type: application/json" \\')
    print('    -d \'{"title":"New Task","priority":"high"}\'\n')
    
    print("[INFO] Press Ctrl+C to stop the server\n")
    
    local server = http_server.listen {
        host = "0.0.0.0",
        port = port,
        onstream = handle_request,
        onerror = function(_, stream, err)
            print(string.format("[ERROR] %s", err))
            if stream then
                send_error(stream, 500, "Internal server error")
            end
        end
    }
    
    assert(server:listen())
    assert(server:loop())
end

-- Run server
main()