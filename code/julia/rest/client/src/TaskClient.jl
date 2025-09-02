module TaskClient

using HTTP
using JSON3

struct Task
    id::String
    title::String
    description::Union{String, Nothing}
    status::String
    priority::String
    tags::Vector{String}
    assigned_to::Union{String, Nothing}
    created_at::Float64
    updated_at::Float64
end

struct Client
    base_url::String
end

function list_tasks(client::Client; status=nothing, assigned_to=nothing)
    url = "$(client.base_url)/api/tasks"
    params = Dict{String, String}()
    
    if !isnothing(status)
        params["status"] = status
    end
    if !isnothing(assigned_to)
        params["assigned_to"] = assigned_to
    end
    
    if !isempty(params)
        query_string = join(["$k=$v" for (k, v) in params], "&")
        url = "$url?$query_string"
    end
    
    response = HTTP.get(url)
    
    if response.status == 200
        data = JSON3.read(String(response.body))
        return data.tasks
    else
        return nothing
    end
end

function get_task(client::Client, id::String)
    url = "$(client.base_url)/api/tasks/$id"
    response = HTTP.get(url)
    
    if response.status == 200
        return JSON3.read(String(response.body))
    else
        return nothing
    end
end

function create_task(client::Client, title::String; description=nothing, priority="medium", tags=String[], assigned_to=nothing)
    url = "$(client.base_url)/api/tasks"
    
    body = Dict(
        "title" => title,
        "priority" => priority,
        "tags" => tags
    )
    
    if !isnothing(description)
        body["description"] = description
    end
    if !isnothing(assigned_to)
        body["assigned_to"] = assigned_to
    end
    
    response = HTTP.post(
        url,
        ["Content-Type" => "application/json"],
        JSON3.write(body)
    )
    
    if response.status == 201
        return JSON3.read(String(response.body))
    else
        return nothing
    end
end

function update_task(client::Client, id::String; title=nothing, description=nothing, status=nothing, priority=nothing, tags=nothing, assigned_to=nothing)
    url = "$(client.base_url)/api/tasks/$id"
    
    body = Dict{String, Any}()
    
    if !isnothing(title)
        body["title"] = title
    end
    if !isnothing(description)
        body["description"] = description
    end
    if !isnothing(status)
        body["status"] = status
    end
    if !isnothing(priority)
        body["priority"] = priority
    end
    if !isnothing(tags)
        body["tags"] = tags
    end
    if !isnothing(assigned_to)
        body["assigned_to"] = assigned_to
    end
    
    response = HTTP.put(
        url,
        ["Content-Type" => "application/json"],
        JSON3.write(body)
    )
    
    if response.status == 200
        return JSON3.read(String(response.body))
    else
        return nothing
    end
end

function update_task_status(client::Client, id::String, status::String)
    url = "$(client.base_url)/api/tasks/$id/status"
    
    body = Dict("status" => status)
    
    response = HTTP.patch(
        url,
        ["Content-Type" => "application/json"],
        JSON3.write(body)
    )
    
    if response.status == 200
        return JSON3.read(String(response.body))
    else
        return nothing
    end
end

function delete_task(client::Client, id::String)
    url = "$(client.base_url)/api/tasks/$id"
    response = HTTP.delete(url)
    
    return response.status == 204
end

function print_banner()
    println("╔════════════════════════════════════════════════╗")
    println("║       Julia Task Management REST Client        ║")
    println("║            Testing API Operations              ║")
    println("╚════════════════════════════════════════════════╝")
    println()
end

function run_demo()
    client = Client("http://localhost:8080")
    
    # 1. List all tasks
    println("1. Listing all tasks...")
    tasks = list_tasks(client)
    if !isnothing(tasks)
        println("   Found $(length(tasks)) tasks")
        for task in tasks
            println("   - [$(task.id)] $(task.title) ($(task.status))")
        end
    else
        println("   Error: Failed to list tasks")
    end
    
    # 2. Create a new task
    println("\n2. Creating a new task...")
    new_task = create_task(
        client,
        "Learn Julia multiple dispatch",
        description="Master Julia's powerful type system and multiple dispatch",
        priority="high",
        tags=["julia", "dispatch", "types"],
        assigned_to="julia-team"
    )
    
    if !isnothing(new_task)
        println("   Created task: $(new_task.title)")
        println("   ID: $(new_task.id)")
        println("   Priority: $(new_task.priority)")
        println("   Tags: $(join(new_task.tags, ", "))")
        
        task_id = new_task.id
        
        # 3. Get task details
        println("\n3. Getting task details...")
        task_detail = get_task(client, task_id)
        if !isnothing(task_detail)
            println("   Title: $(task_detail.title)")
            if !isnothing(task_detail.description)
                println("   Description: $(task_detail.description)")
            end
            println("   Status: $(task_detail.status)")
            if !isnothing(task_detail.assigned_to)
                println("   Assigned to: $(task_detail.assigned_to)")
            end
        else
            println("   Error: Failed to get task")
        end
        
        # 4. Update task status
        println("\n4. Updating task status to 'in-progress'...")
        status_update = update_task_status(client, task_id, "in-progress")
        if !isnothing(status_update)
            println("   Updated status to: $(status_update.status)")
        else
            println("   Error: Failed to update status")
        end
        
        # 5. Update task details
        println("\n5. Updating task details...")
        task_update = update_task(
            client,
            task_id,
            title="Master Julia's metaprogramming",
            priority="urgent"
        )
        if !isnothing(task_update)
            println("   Updated title: $(task_update.title)")
            println("   Updated priority: $(task_update.priority)")
        else
            println("   Error: Failed to update task")
        end
        
        # 6. Filter tasks by status
        println("\n6. Filtering tasks by status...")
        in_progress_tasks = list_tasks(client, status="in-progress")
        if !isnothing(in_progress_tasks)
            println("   Found $(length(in_progress_tasks)) in-progress tasks")
            for task in in_progress_tasks
                println("   - $(task.title)")
            end
        else
            println("   Error: Failed to filter tasks")
        end
        
        # 7. Delete the task
        println("\n7. Deleting the task...")
        if delete_task(client, task_id)
            println("   Task deleted successfully")
        else
            println("   Error: Failed to delete task")
        end
        
        # 8. Verify deletion
        println("\n8. Verifying deletion...")
        verify_task = get_task(client, task_id)
        if isnothing(verify_task)
            println("   Task not found (as expected)")
        else
            println("   Error: Task still exists")
        end
    else
        println("   Error: Failed to create task")
    end
    
    println("\n✅ Demo completed successfully!")
end

end # module