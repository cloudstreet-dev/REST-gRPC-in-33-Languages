#!/usr/bin/env lua

-- Task Management REST API Client
-- Using lua-http for HTTP requests

local http_request = require "http.request"
local json = require "cjson"
local headers = require "http.headers"

-- Configuration
local BASE_URL = os.getenv("API_URL") or "http://localhost:8080/api"

-- HTTP Client
local Client = {}
Client.__index = Client

function Client:new()
    local client = setmetatable({}, Client)
    return client
end

function Client:request(method, path, body)
    local url = BASE_URL .. path
    local req = http_request.new_from_uri(url)
    req.headers:upsert(":method", method)
    req.headers:upsert("content-type", "application/json")
    
    if body then
        req:set_body(json.encode(body))
    end
    
    local headers, stream = req:go(10)
    
    if not headers then
        return nil, stream
    end
    
    local status = tonumber(headers:get(":status"))
    local response_body = stream:get_body_as_string()
    
    if response_body and #response_body > 0 then
        local ok, data = pcall(json.decode, response_body)
        if ok then
            return data, nil, status
        end
    end
    
    if status == 204 then
        return true, nil, status
    end
    
    return nil, "Request failed", status
end

function Client:list_tasks(filters)
    local query = ""
    if filters then
        local params = {}
        for k, v in pairs(filters) do
            if type(v) == "table" then
                table.insert(params, k .. "=" .. table.concat(v, ","))
            else
                table.insert(params, k .. "=" .. tostring(v))
            end
        end
        if #params > 0 then
            query = "?" .. table.concat(params, "&")
        end
    end
    
    return self:request("GET", "/tasks" .. query)
end

function Client:get_task(id)
    return self:request("GET", "/tasks/" .. id)
end

function Client:create_task(data)
    return self:request("POST", "/tasks", data)
end

function Client:update_task(id, data)
    return self:request("PUT", "/tasks/" .. id, data)
end

function Client:update_task_status(id, status)
    return self:request("PATCH", "/tasks/" .. id .. "/status", {status = status})
end

function Client:delete_task(id)
    return self:request("DELETE", "/tasks/" .. id)
end

-- Demo function
local function run_demo()
    print([[
╔════════════════════════════════════════════════╗
║        Lua Task Management REST Client         ║
║            Testing API Operations              ║
╚════════════════════════════════════════════════╝
]])
    
    local client = Client:new()
    
    -- 1. List all tasks
    print("\n1. Listing all tasks...")
    local result, err = client:list_tasks()
    if result then
        print(string.format("   Found %d tasks", result.total_count))
        for _, task in ipairs(result.tasks) do
            print(string.format("   - [%s] %s (%s)", task.id, task.title, task.status))
        end
    else
        print("   Error: " .. tostring(err))
    end
    
    -- 2. Create a new task
    print("\n2. Creating a new task...")
    local new_task = {
        title = "Learn Lua metatables",
        description = "Understand and implement Lua metatable patterns",
        priority = "high",
        tags = {"lua", "metaprogramming", "advanced"},
        assigned_to = "lua-team"
    }
    
    local task, err = client:create_task(new_task)
    local task_id = nil
    if task then
        task_id = task.id
        print(string.format("   Created task: %s", task.title))
        print(string.format("   ID: %s", task.id))
        print(string.format("   Priority: %s", task.priority))
        print(string.format("   Tags: %s", table.concat(task.tags, ", ")))
    else
        print("   Error: " .. tostring(err))
    end
    
    if task_id then
        -- 3. Get task details
        print("\n3. Getting task details...")
        local task, err = client:get_task(task_id)
        if task then
            print(string.format("   Title: %s", task.title))
            print(string.format("   Description: %s", task.description))
            print(string.format("   Status: %s", task.status))
            print(string.format("   Assigned to: %s", task.assigned_to))
        else
            print("   Error: " .. tostring(err))
        end
        
        -- 4. Update task status
        print("\n4. Updating task status to 'in_progress'...")
        local task, err = client:update_task_status(task_id, "in_progress")
        if task then
            print(string.format("   Updated status to: %s", task.status))
        else
            print("   Error: " .. tostring(err))
        end
        
        -- 5. Update task details
        print("\n5. Updating task details...")
        local updates = {
            title = "Master Lua metatables and metamethods",
            priority = "urgent"
        }
        local task, err = client:update_task(task_id, updates)
        if task then
            print(string.format("   Updated title: %s", task.title))
            print(string.format("   Updated priority: %s", task.priority))
        else
            print("   Error: " .. tostring(err))
        end
        
        -- 6. Filter tasks by status
        print("\n6. Filtering tasks by status...")
        local result, err = client:list_tasks({status = "in_progress"})
        if result then
            print(string.format("   Found %d in-progress tasks", result.total_count))
            for _, task in ipairs(result.tasks) do
                print(string.format("   - %s", task.title))
            end
        else
            print("   Error: " .. tostring(err))
        end
        
        -- 7. Delete the task
        print("\n7. Deleting the task...")
        local success, err = client:delete_task(task_id)
        if success then
            print("   Task deleted successfully")
        else
            print("   Error: " .. tostring(err))
        end
        
        -- 8. Verify deletion
        print("\n8. Verifying deletion...")
        local task, err, status = client:get_task(task_id)
        if status == 404 then
            print("   Task not found (as expected)")
        elseif task then
            print("   Error: Task still exists")
        else
            print("   Error: " .. tostring(err))
        end
    end
    
    print("\n✅ Demo completed successfully!")
end

-- Run the demo
run_demo()