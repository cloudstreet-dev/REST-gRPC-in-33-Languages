module TaskServer

using HTTP
using JSON3
using StructTypes
using Dates
using UUIDs
using Sockets

# Type definitions
@enum TaskStatus begin
    PENDING
    IN_PROGRESS
    COMPLETED
    CANCELLED
end

@enum TaskPriority begin
    LOW
    MEDIUM
    HIGH
    URGENT
end

mutable struct Task
    id::String
    title::String
    description::Union{String, Nothing}
    status::TaskStatus
    priority::TaskPriority
    tags::Vector{String}
    assigned_to::Union{String, Nothing}
    created_at::Float64
    updated_at::Float64
end

struct CreateTaskRequest
    title::String
    description::Union{String, Nothing}
    priority::Union{String, Nothing}
    tags::Union{Vector{String}, Nothing}
    assigned_to::Union{String, Nothing}
end

struct UpdateTaskRequest
    title::Union{String, Nothing}
    description::Union{String, Nothing}
    status::Union{String, Nothing}
    priority::Union{String, Nothing}
    tags::Union{Vector{String}, Nothing}
    assigned_to::Union{String, Nothing}
end

struct UpdateStatusRequest
    status::String
end

struct TaskListResponse
    tasks::Vector{Task}
    total_count::Int
end

struct ErrorResponse
    error::String
end

# JSON serialization
StructTypes.StructType(::Type{Task}) = StructTypes.Mutable()
StructTypes.StructType(::Type{CreateTaskRequest}) = StructTypes.Struct()
StructTypes.StructType(::Type{UpdateTaskRequest}) = StructTypes.Struct()
StructTypes.StructType(::Type{UpdateStatusRequest}) = StructTypes.Struct()
StructTypes.StructType(::Type{TaskListResponse}) = StructTypes.Struct()
StructTypes.StructType(::Type{ErrorResponse}) = StructTypes.Struct()

# Custom JSON conversion for enums
function JSON3.write(io::IO, status::TaskStatus)
    status_str = if status == PENDING
        "pending"
    elseif status == IN_PROGRESS
        "in-progress"
    elseif status == COMPLETED
        "completed"
    else
        "cancelled"
    end
    JSON3.write(io, status_str)
end

function JSON3.write(io::IO, priority::TaskPriority)
    priority_str = if priority == LOW
        "low"
    elseif priority == MEDIUM
        "medium"
    elseif priority == HIGH
        "high"
    else
        "urgent"
    end
    JSON3.write(io, priority_str)
end

# Parse status from string
function parse_status(s::Union{String, Nothing})
    isnothing(s) && return nothing
    s_lower = lowercase(s)
    if s_lower == "pending"
        return PENDING
    elseif s_lower in ["in-progress", "in_progress"]
        return IN_PROGRESS
    elseif s_lower == "completed"
        return COMPLETED
    elseif s_lower == "cancelled"
        return CANCELLED
    else
        return nothing
    end
end

# Parse priority from string
function parse_priority(s::Union{String, Nothing})
    isnothing(s) && return nothing
    s_lower = lowercase(s)
    if s_lower == "low"
        return LOW
    elseif s_lower == "medium"
        return MEDIUM
    elseif s_lower == "high"
        return HIGH
    elseif s_lower == "urgent"
        return URGENT
    else
        return nothing
    end
end

# Global task store (thread-safe with lock)
const task_store = Dict{String, Task}()
const task_lock = ReentrantLock()
task_counter = Ref(0)

function generate_task_id()
    lock(task_lock) do
        task_counter[] += 1
        return "task-$(task_counter[])"
    end
end

function create_task(req::CreateTaskRequest)
    task_id = generate_task_id()
    now_time = time()
    
    priority = parse_priority(req.priority)
    if isnothing(priority)
        priority = MEDIUM
    end
    
    task = Task(
        task_id,
        req.title,
        req.description,
        PENDING,
        priority,
        isnothing(req.tags) ? String[] : req.tags,
        req.assigned_to,
        now_time,
        now_time
    )
    
    lock(task_lock) do
        task_store[task_id] = task
    end
    
    return task
end

function get_task(id::String)
    lock(task_lock) do
        return get(task_store, id, nothing)
    end
end

function update_task(id::String, updates::UpdateTaskRequest)
    lock(task_lock) do
        if !haskey(task_store, id)
            return nothing
        end
        
        task = task_store[id]
        
        if !isnothing(updates.title)
            task.title = updates.title
        end
        
        if !isnothing(updates.description)
            task.description = updates.description
        end
        
        if !isnothing(updates.status)
            status = parse_status(updates.status)
            if !isnothing(status)
                task.status = status
            end
        end
        
        if !isnothing(updates.priority)
            priority = parse_priority(updates.priority)
            if !isnothing(priority)
                task.priority = priority
            end
        end
        
        if !isnothing(updates.tags)
            task.tags = updates.tags
        end
        
        if !isnothing(updates.assigned_to)
            task.assigned_to = updates.assigned_to
        end
        
        task.updated_at = time()
        return task
    end
end

function update_task_status(id::String, status::TaskStatus)
    lock(task_lock) do
        if !haskey(task_store, id)
            return nothing
        end
        
        task = task_store[id]
        task.status = status
        task.updated_at = time()
        return task
    end
end

function delete_task(id::String)
    lock(task_lock) do
        if haskey(task_store, id)
            delete!(task_store, id)
            return true
        end
        return false
    end
end

function list_tasks(; status_filter=nothing, assigned_to_filter=nothing)
    lock(task_lock) do
        tasks = Task[]
        
        for task in values(task_store)
            include_task = true
            
            if !isnothing(status_filter)
                status = parse_status(status_filter)
                if !isnothing(status) && task.status != status
                    include_task = false
                end
            end
            
            if include_task && !isnothing(assigned_to_filter)
                if isnothing(task.assigned_to) || task.assigned_to != assigned_to_filter
                    include_task = false
                end
            end
            
            if include_task
                push!(tasks, task)
            end
        end
        
        # Sort by creation time (newest first)
        sort!(tasks, by=t -> -t.created_at)
        return tasks
    end
end

# Initialize with sample data
function init_sample_data()
    create_task(CreateTaskRequest(
        "Learn Julia",
        "Master scientific computing with Julia",
        "high",
        ["julia", "learning", "scientific"],
        "developer"
    ))
    
    create_task(CreateTaskRequest(
        "Build REST API",
        "Create REST API with HTTP.jl",
        "urgent",
        ["julia", "api", "rest"],
        "backend-team"
    ))
    
    create_task(CreateTaskRequest(
        "Write Tests",
        "Add unit tests with Test package",
        "medium",
        ["testing", "quality"],
        nothing
    ))
end

# HTTP Handlers
function health_handler(req::HTTP.Request)
    response = Dict(
        "status" => "healthy",
        "service" => "task-api",
        "version" => "1.0.0",
        "timestamp" => time()
    )
    return HTTP.Response(200, ["Content-Type" => "application/json"], JSON3.write(response))
end

function list_tasks_handler(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    status_filter = get(query, "status", nothing)
    assigned_to_filter = get(query, "assigned_to", nothing)
    
    tasks = list_tasks(status_filter=status_filter, assigned_to_filter=assigned_to_filter)
    response = TaskListResponse(tasks, length(tasks))
    
    return HTTP.Response(200, ["Content-Type" => "application/json"], JSON3.write(response))
end

function get_task_handler(req::HTTP.Request, id::String)
    task = get_task(id)
    
    if isnothing(task)
        error_response = ErrorResponse("Task not found")
        return HTTP.Response(404, ["Content-Type" => "application/json"], JSON3.write(error_response))
    end
    
    return HTTP.Response(200, ["Content-Type" => "application/json"], JSON3.write(task))
end

function create_task_handler(req::HTTP.Request)
    try
        body = String(req.body)
        json_data = JSON3.read(body)
        
        if !haskey(json_data, :title)
            error_response = ErrorResponse("Missing required field: title")
            return HTTP.Response(400, ["Content-Type" => "application/json"], JSON3.write(error_response))
        end
        
        create_req = CreateTaskRequest(
            json_data.title,
            get(json_data, :description, nothing),
            get(json_data, :priority, nothing),
            get(json_data, :tags, nothing),
            get(json_data, :assigned_to, nothing)
        )
        
        task = create_task(create_req)
        return HTTP.Response(201, ["Content-Type" => "application/json"], JSON3.write(task))
    catch e
        error_response = ErrorResponse("Invalid request: $(e)")
        return HTTP.Response(400, ["Content-Type" => "application/json"], JSON3.write(error_response))
    end
end

function update_task_handler(req::HTTP.Request, id::String)
    try
        body = String(req.body)
        json_data = JSON3.read(body)
        
        update_req = UpdateTaskRequest(
            get(json_data, :title, nothing),
            get(json_data, :description, nothing),
            get(json_data, :status, nothing),
            get(json_data, :priority, nothing),
            get(json_data, :tags, nothing),
            get(json_data, :assigned_to, nothing)
        )
        
        task = update_task(id, update_req)
        
        if isnothing(task)
            error_response = ErrorResponse("Task not found")
            return HTTP.Response(404, ["Content-Type" => "application/json"], JSON3.write(error_response))
        end
        
        return HTTP.Response(200, ["Content-Type" => "application/json"], JSON3.write(task))
    catch e
        error_response = ErrorResponse("Invalid request: $(e)")
        return HTTP.Response(400, ["Content-Type" => "application/json"], JSON3.write(error_response))
    end
end

function update_task_status_handler(req::HTTP.Request, id::String)
    try
        body = String(req.body)
        json_data = JSON3.read(body)
        
        if !haskey(json_data, :status)
            error_response = ErrorResponse("Missing required field: status")
            return HTTP.Response(400, ["Content-Type" => "application/json"], JSON3.write(error_response))
        end
        
        status = parse_status(json_data.status)
        if isnothing(status)
            error_response = ErrorResponse("Invalid status value")
            return HTTP.Response(400, ["Content-Type" => "application/json"], JSON3.write(error_response))
        end
        
        task = update_task_status(id, status)
        
        if isnothing(task)
            error_response = ErrorResponse("Task not found")
            return HTTP.Response(404, ["Content-Type" => "application/json"], JSON3.write(error_response))
        end
        
        return HTTP.Response(200, ["Content-Type" => "application/json"], JSON3.write(task))
    catch e
        error_response = ErrorResponse("Invalid request: $(e)")
        return HTTP.Response(400, ["Content-Type" => "application/json"], JSON3.write(error_response))
    end
end

function delete_task_handler(req::HTTP.Request, id::String)
    if delete_task(id)
        return HTTP.Response(204, ["Content-Type" => "application/json"], "")
    else
        error_response = ErrorResponse("Task not found")
        return HTTP.Response(404, ["Content-Type" => "application/json"], JSON3.write(error_response))
    end
end

# Router
function router(req::HTTP.Request)
    uri = HTTP.URI(req.target)
    path = uri.path
    method = req.method
    
    # Health check
    if path == "/health" && method == "GET"
        return health_handler(req)
    end
    
    # Task routes
    if startswith(path, "/api/tasks")
        path_parts = split(path, "/"; keepempty=false)
        
        if length(path_parts) == 2  # /api/tasks
            if method == "GET"
                return list_tasks_handler(req)
            elseif method == "POST"
                return create_task_handler(req)
            end
        elseif length(path_parts) == 3  # /api/tasks/{id}
            task_id = path_parts[3]
            if method == "GET"
                return get_task_handler(req, task_id)
            elseif method == "PUT"
                return update_task_handler(req, task_id)
            elseif method == "DELETE"
                return delete_task_handler(req, task_id)
            end
        elseif length(path_parts) == 4 && path_parts[4] == "status"  # /api/tasks/{id}/status
            task_id = path_parts[3]
            if method == "PATCH"
                return update_task_status_handler(req, task_id)
            end
        end
    end
    
    # 404 for unmatched routes
    error_response = ErrorResponse("Not found")
    return HTTP.Response(404, ["Content-Type" => "application/json"], JSON3.write(error_response))
end

function start_server(port=8080)
    # Initialize sample data
    init_sample_data()
    
    println("🚀 Julia Task REST API Server")
    println("📍 Listening on http://localhost:$port")
    println("🔍 Health check: http://localhost:$port/health\n")
    
    HTTP.serve(router, "0.0.0.0", port)
end

end # module