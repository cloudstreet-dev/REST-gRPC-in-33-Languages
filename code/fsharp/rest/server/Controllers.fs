module TaskRestServer.Controllers

open System
open System.Linq
open Microsoft.AspNetCore.Mvc
open Microsoft.EntityFrameworkCore
open Microsoft.Extensions.Logging
open TaskRestServer.Models
open TaskRestServer.Database

[<ApiController>]
[<Route("api/[controller]")>]
type TasksController(context: TaskDbContext, logger: ILogger<TasksController>) =
    inherit ControllerBase()
    
    /// Get all tasks with optional filtering, sorting, and pagination
    [<HttpGet>]
    member this.GetTasks(
        [<FromQuery>] status: TaskStatus option,
        [<FromQuery>] assignedTo: string option,
        [<FromQuery>] tags: string option,
        [<FromQuery>] pageSize: int option,
        [<FromQuery>] pageToken: string option,
        [<FromQuery>] sortBy: string option,
        [<FromQuery>] sortOrder: string option
    ) = 
        async {
            try
                let pageSize = min (defaultArg pageSize 20) 100
                let sortBy = defaultArg sortBy "created_at"
                let sortOrder = defaultArg sortOrder "desc"
                
                let mutable query = context.Tasks.AsQueryable()

                // Apply filters using F# pattern matching
                query <- 
                    match status with
                    | Some s -> query.Where(fun t -> t.Status = s)
                    | None -> query

                query <- 
                    match assignedTo with
                    | Some assigned -> query.Where(fun t -> t.AssignedTo = Some assigned)
                    | None -> query

                query <- 
                    match tags with
                    | Some tagString when not (String.IsNullOrEmpty(tagString)) ->
                        let tagList = tagString.Split(',') |> Array.map (fun s -> s.Trim()) |> Array.toList
                        query.Where(fun t -> tagList |> List.exists (fun tag -> t.Tags.Contains(tag)))
                    | _ -> query

                // Apply sorting using pattern matching
                query <- 
                    match sortBy.ToLower(), sortOrder.ToLower() with
                    | "title", "asc" -> query.OrderBy(fun t -> t.Title)
                    | "title", _ -> query.OrderByDescending(fun t -> t.Title)
                    | "status", "asc" -> query.OrderBy(fun t -> t.Status)
                    | "status", _ -> query.OrderByDescending(fun t -> t.Status)
                    | "priority", "asc" -> query.OrderBy(fun t -> t.Priority)
                    | "priority", _ -> query.OrderByDescending(fun t -> t.Priority)
                    | "updated_at", "asc" -> query.OrderBy(fun t -> t.UpdatedAt)
                    | "updated_at", _ -> query.OrderByDescending(fun t -> t.UpdatedAt)
                    | _, "asc" -> query.OrderBy(fun t -> t.CreatedAt)
                    | _ -> query.OrderByDescending(fun t -> t.CreatedAt)

                // Apply pagination
                let skip = 
                    match pageToken with
                    | Some token -> 
                        match Int32.TryParse(token) with
                        | true, value -> value
                        | false, _ -> 0
                    | None -> 0

                let! totalCount = query.CountAsync() |> Async.AwaitTask
                let! tasks = query.Skip(skip).Take(pageSize).ToListAsync() |> Async.AwaitTask

                let response = {
                    Tasks = tasks |> List.ofSeq
                    TotalCount = totalCount
                    NextPageToken = 
                        if skip + pageSize < totalCount then
                            Some (string (skip + pageSize))
                        else None
                }

                return this.Ok(response) :> ActionResult<ListTasksResponse>
            with
            | ex ->
                logger.LogError(ex, "Error retrieving tasks")
                return this.StatusCode(500, { Message = "Internal server error"; Errors = None }) :> ActionResult<ListTasksResponse>
        } |> Async.StartAsTask

    /// Get a specific task by ID
    [<HttpGet("{id}")>]
    member this.GetTask(id: string) =
        async {
            try
                let! task = context.Tasks.FindAsync(id) |> Async.AwaitTask
                
                match task with
                | null -> 
                    return this.NotFound({ Message = sprintf "Task with ID '%s' not found" id; Errors = None }) :> ActionResult<Task>
                | task -> 
                    return this.Ok(task) :> ActionResult<Task>
            with
            | ex ->
                logger.LogError(ex, "Error retrieving task {TaskId}", id)
                return this.StatusCode(500, { Message = "Internal server error"; Errors = None }) :> ActionResult<Task>
        } |> Async.StartAsTask

    /// Create a new task
    [<HttpPost>]
    member this.CreateTask([<FromBody>] request: CreateTaskRequest) =
        async {
            try
                if not this.ModelState.IsValid then
                    return this.BadRequest(this.ModelState) :> ActionResult<Task>
                else
                    let task = {
                        Id = Guid.NewGuid().ToString()
                        Title = request.Title.Trim()
                        Description = request.Description |> Option.map (fun s -> s.Trim())
                        Status = TaskStatus.Pending
                        Priority = defaultArg request.Priority TaskPriority.Medium
                        Tags = defaultArg request.Tags []
                        AssignedTo = request.AssignedTo |> Option.map (fun s -> s.Trim())
                        CreatedBy = defaultArg (request.CreatedBy |> Option.map (fun s -> s.Trim())) "system"
                        CreatedAt = DateTime.UtcNow
                        UpdatedAt = DateTime.UtcNow
                        DueDate = request.DueDate
                    }

                    context.Tasks.Add(task) |> ignore
                    let! _ = context.SaveChangesAsync() |> Async.AwaitTask

                    logger.LogInformation("Created task {TaskId}: {Title}", task.Id, task.Title)
                    
                    return this.CreatedAtAction("GetTask", {| id = task.Id |}, task) :> ActionResult<Task>
            with
            | ex ->
                logger.LogError(ex, "Error creating task")
                return this.StatusCode(500, { Message = "Internal server error"; Errors = None }) :> ActionResult<Task>
        } |> Async.StartAsTask

    /// Update an existing task
    [<HttpPut("{id}")>]
    member this.UpdateTask(id: string, [<FromBody>] request: UpdateTaskRequest) =
        async {
            try
                if not this.ModelState.IsValid then
                    return this.BadRequest(this.ModelState) :> ActionResult<Task>
                else
                    let! existingTask = context.Tasks.FindAsync(id) |> Async.AwaitTask
                    
                    match existingTask with
                    | null ->
                        return this.NotFound({ Message = sprintf "Task with ID '%s' not found" id; Errors = None }) :> ActionResult<Task>
                    | task ->
                        // Update fields using F# option pattern matching
                        let updatedTask = {
                            task with
                                Title = defaultArg (request.Title |> Option.map (fun s -> s.Trim())) task.Title
                                Description = 
                                    match request.Description with
                                    | Some desc -> Some (desc.Trim())
                                    | None -> task.Description
                                Status = defaultArg request.Status task.Status
                                Priority = defaultArg request.Priority task.Priority
                                Tags = defaultArg request.Tags task.Tags
                                AssignedTo = 
                                    match request.AssignedTo with
                                    | Some assigned -> Some (assigned.Trim())
                                    | None -> task.AssignedTo
                                DueDate = request.DueDate |> Option.orElse task.DueDate
                                UpdatedAt = DateTime.UtcNow
                        }

                        context.Entry(task).CurrentValues.SetValues(updatedTask)
                        let! _ = context.SaveChangesAsync() |> Async.AwaitTask

                        logger.LogInformation("Updated task {TaskId}: {Title}", updatedTask.Id, updatedTask.Title)
                        
                        return this.Ok(updatedTask) :> ActionResult<Task>
            with
            | ex ->
                logger.LogError(ex, "Error updating task {TaskId}", id)
                return this.StatusCode(500, { Message = "Internal server error"; Errors = None }) :> ActionResult<Task>
        } |> Async.StartAsTask

    /// Update task status
    [<HttpPatch("{id}/status")>]
    member this.UpdateTaskStatus(id: string, [<FromBody>] status: TaskStatus) =
        async {
            try
                let! existingTask = context.Tasks.FindAsync(id) |> Async.AwaitTask
                
                match existingTask with
                | null ->
                    return this.NotFound({ Message = sprintf "Task with ID '%s' not found" id; Errors = None }) :> ActionResult<Task>
                | task ->
                    let updatedTask = { task with Status = status; UpdatedAt = DateTime.UtcNow }
                    context.Entry(task).CurrentValues.SetValues(updatedTask)
                    let! _ = context.SaveChangesAsync() |> Async.AwaitTask

                    logger.LogInformation("Updated task {TaskId} status to {Status}", updatedTask.Id, status)
                    
                    return this.Ok(updatedTask) :> ActionResult<Task>
            with
            | ex ->
                logger.LogError(ex, "Error updating task status {TaskId}", id)
                return this.StatusCode(500, { Message = "Internal server error"; Errors = None }) :> ActionResult<Task>
        } |> Async.StartAsTask

    /// Delete a task
    [<HttpDelete("{id}")>]
    member this.DeleteTask(id: string) =
        async {
            try
                let! task = context.Tasks.FindAsync(id) |> Async.AwaitTask
                
                match task with
                | null ->
                    return this.NotFound({ Message = sprintf "Task with ID '%s' not found" id; Errors = None }) :> IActionResult
                | task ->
                    context.Tasks.Remove(task) |> ignore
                    let! _ = context.SaveChangesAsync() |> Async.AwaitTask

                    logger.LogInformation("Deleted task {TaskId}: {Title}", task.Id, task.Title)
                    
                    return this.NoContent() :> IActionResult
            with
            | ex ->
                logger.LogError(ex, "Error deleting task {TaskId}", id)
                return this.StatusCode(500, { Message = "Internal server error"; Errors = None }) :> IActionResult
        } |> Async.StartAsTask