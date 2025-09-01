module TaskGrpcServer.TaskService

open System
open System.Collections.Concurrent
open System.Linq
open Microsoft.Extensions.Logging
open Grpc.Core
open Google.Protobuf.WellKnownTypes
open TaskNamespace

type TaskServiceImplementation(logger: ILogger<TaskServiceImplementation>) =
    inherit TaskService.TaskServiceBase()
    
    let tasks = ConcurrentDictionary<string, Task>()
    
    // Initialize with sample data using F# immutable approach
    let initializeSampleData () =
        let now = Timestamp.FromDateTime(DateTime.UtcNow)
        
        let sampleTasks = [
            Task(
                Id = Guid.NewGuid().ToString(),
                Title = "Complete F# project documentation",
                Description = "Write comprehensive documentation for the F# gRPC API",
                Status = TaskStatus.InProgress,
                Priority = TaskPriority.High,
                AssignedTo = "dev-team",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            ) |> fun task -> 
                task.Tags.AddRange(["documentation"; "fsharp"; "api"])
                task
            
            Task(
                Id = Guid.NewGuid().ToString(),
                Title = "Review functional programming patterns",
                Description = "Review and approve functional programming patterns in codebase",
                Status = TaskStatus.Pending,
                Priority = TaskPriority.Medium,
                AssignedTo = "senior-dev",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            ) |> fun task ->
                task.Tags.AddRange(["review"; "functional"; "patterns"])
                task
                
            Task(
                Id = Guid.NewGuid().ToString(),
                Title = "Deploy F# services to production",
                Description = "Deploy the F# microservices to production environment",
                Status = TaskStatus.Pending,
                Priority = TaskPriority.Critical,
                AssignedTo = "devops",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            ) |> fun task ->
                task.Tags.AddRange(["deployment"; "production"; "fsharp"])
                task
        ]
        
        sampleTasks 
        |> List.iter (fun task -> tasks.[task.Id] <- task)
    
    // Initialize sample data in constructor
    do initializeSampleData()
    
    // Helper functions using F# functional programming
    let validateTaskId (id: string) =
        match String.IsNullOrEmpty(id) with
        | true -> Error (RpcException(Status(StatusCode.InvalidArgument, "Task ID is required")))
        | false -> Ok id
    
    let validateTaskTitle (title: string) =
        match String.IsNullOrWhiteSpace(title) with
        | true -> Error (RpcException(Status(StatusCode.InvalidArgument, "Title is required")))
        | false when title.Length > 200 -> Error (RpcException(Status(StatusCode.InvalidArgument, "Title must be 200 characters or less")))
        | false -> Ok title
    
    let findTask (id: string) =
        match tasks.TryGetValue(id) with
        | true, task -> Ok task
        | false, _ -> Error (RpcException(Status(StatusCode.NotFound, sprintf "Task with ID '%s' not found" id)))
    
    let applyFilters (request: ListTasksRequest) (taskSeq: Task seq) =
        taskSeq
        |> fun tasks -> 
            if request.Status <> TaskStatus.Unspecified then
                tasks |> Seq.filter (fun t -> t.Status = request.Status)
            else tasks
        |> fun tasks ->
            if not (String.IsNullOrEmpty(request.AssignedTo)) then
                tasks |> Seq.filter (fun t -> t.AssignedTo = request.AssignedTo)
            else tasks
        |> fun tasks ->
            if request.Tags.Count > 0 then
                tasks |> Seq.filter (fun t -> 
                    request.Tags |> Seq.exists (fun tag -> t.Tags.Contains(tag)))
            else tasks
    
    let applySorting (request: ListTasksRequest) (taskSeq: Task seq) =
        match request.SortOrder with
        | SortOrder.CreatedAsc -> taskSeq |> Seq.sortBy (fun t -> t.CreatedAt.ToDateTime())
        | SortOrder.CreatedDesc -> taskSeq |> Seq.sortByDescending (fun t -> t.CreatedAt.ToDateTime())
        | SortOrder.UpdatedAsc -> taskSeq |> Seq.sortBy (fun t -> t.UpdatedAt.ToDateTime())
        | SortOrder.UpdatedDesc -> taskSeq |> Seq.sortByDescending (fun t -> t.UpdatedAt.ToDateTime())
        | SortOrder.PriorityDesc -> 
            taskSeq 
            |> Seq.sortByDescending (fun t -> int t.Priority)
            |> Seq.thenBy (fun t -> t.CreatedAt.ToDateTime())
        | _ -> taskSeq |> Seq.sortByDescending (fun t -> t.CreatedAt.ToDateTime())
    
    let applyPagination (request: ListTasksRequest) (taskSeq: Task seq) =
        let pageSize = if request.PageSize > 0 then min (int request.PageSize) 100 else 20
        let skip = 
            match String.IsNullOrEmpty(request.PageToken) with
            | true -> 0
            | false -> 
                match Int32.TryParse(request.PageToken) with
                | true, value -> value
                | false, _ -> 0
        
        taskSeq |> Seq.skip skip |> Seq.take pageSize
    
    override this.GetTask(request: GetTaskRequest, context: ServerCallContext) =
        async {
            logger.LogInformation("GetTask called with ID: {TaskId}", request.Id)
            
            return
                result {
                    let! _ = validateTaskId request.Id
                    let! task = findTask request.Id
                    return task
                }
                |> function
                | Ok task -> task
                | Error ex -> raise ex
        } |> Async.StartAsTask
    
    override this.ListTasks(request: ListTasksRequest, responseStream: IServerStreamWriter<Task>, context: ServerCallContext) =
        async {
            logger.LogInformation("ListTasks called")
            
            try
                let filteredTasks = 
                    tasks.Values
                    |> applyFilters request
                    |> applySorting request
                    |> applyPagination request
                
                for task in filteredTasks do
                    if context.CancellationToken.IsCancellationRequested then
                        break
                    do! responseStream.WriteAsync(task) |> Async.AwaitTask
                    
            with
            | ex -> 
                logger.LogError(ex, "Error in ListTasks")
                raise (RpcException(Status(StatusCode.Internal, "Internal server error")))
        } |> Async.StartAsTask :> System.Threading.Tasks.Task
    
    override this.CreateTask(request: CreateTaskRequest, context: ServerCallContext) =
        async {
            logger.LogInformation("CreateTask called")
            
            return
                result {
                    do! 
                        match request.Task with
                        | null -> Error (RpcException(Status(StatusCode.InvalidArgument, "Task is required")))
                        | _ -> Ok ()
                    
                    let! _ = validateTaskTitle request.Task.Title
                    
                    let now = Timestamp.FromDateTime(DateTime.UtcNow)
                    let task = Task(
                        Id = Guid.NewGuid().ToString(),
                        Title = request.Task.Title,
                        Description = (if String.IsNullOrEmpty(request.Task.Description) then "" else request.Task.Description),
                        Status = TaskStatus.Pending,
                        Priority = (if request.Task.Priority = TaskPriority.Unspecified then TaskPriority.Medium else request.Task.Priority),
                        AssignedTo = (if String.IsNullOrEmpty(request.Task.AssignedTo) then "" else request.Task.AssignedTo),
                        CreatedBy = (if String.IsNullOrEmpty(request.Task.CreatedBy) then "system" else request.Task.CreatedBy),
                        CreatedAt = now,
                        UpdatedAt = now,
                        DueDate = request.Task.DueDate
                    )
                    
                    task.Tags.AddRange(request.Task.Tags)
                    tasks.[task.Id] <- task
                    
                    logger.LogInformation("Created task {TaskId}: {Title}", task.Id, task.Title)
                    return task
                }
                |> function
                | Ok task -> task
                | Error ex -> raise ex
        } |> Async.StartAsTask
    
    override this.UpdateTask(request: UpdateTaskRequest, context: ServerCallContext) =
        async {
            logger.LogInformation("UpdateTask called with ID: {TaskId}", 
                (if request.Task <> null then request.Task.Id else "null"))
            
            return
                result {
                    do!
                        match request.Task with
                        | null -> Error (RpcException(Status(StatusCode.InvalidArgument, "Task with ID is required")))
                        | task when String.IsNullOrEmpty(task.Id) -> Error (RpcException(Status(StatusCode.InvalidArgument, "Task ID is required")))
                        | _ -> Ok ()
                    
                    let! existingTask = findTask request.Task.Id
                    
                    // Create updated task using functional approach
                    let updatedTask = Task()
                    updatedTask.Id <- existingTask.Id
                    updatedTask.Title <- existingTask.Title
                    updatedTask.Description <- existingTask.Description
                    updatedTask.Status <- existingTask.Status
                    updatedTask.Priority <- existingTask.Priority
                    updatedTask.AssignedTo <- existingTask.AssignedTo
                    updatedTask.CreatedBy <- existingTask.CreatedBy
                    updatedTask.CreatedAt <- existingTask.CreatedAt
                    updatedTask.UpdatedAt <- Timestamp.FromDateTime(DateTime.UtcNow)
                    updatedTask.DueDate <- existingTask.DueDate
                    updatedTask.Tags.AddRange(existingTask.Tags)
                    
                    // Apply updates based on update mask using pattern matching
                    request.UpdateMask
                    |> Seq.iter (fun field ->
                        match field with
                        | "title" -> 
                            match validateTaskTitle request.Task.Title with
                            | Ok _ -> updatedTask.Title <- request.Task.Title
                            | Error ex -> raise ex
                        | "description" -> updatedTask.Description <- request.Task.Description
                        | "status" -> updatedTask.Status <- request.Task.Status
                        | "priority" -> updatedTask.Priority <- request.Task.Priority
                        | "tags" -> 
                            updatedTask.Tags.Clear()
                            updatedTask.Tags.AddRange(request.Task.Tags)
                        | "assigned_to" -> updatedTask.AssignedTo <- request.Task.AssignedTo
                        | "due_date" -> updatedTask.DueDate <- request.Task.DueDate
                        | _ -> () // Ignore unknown fields
                    )
                    
                    tasks.[updatedTask.Id] <- updatedTask
                    
                    logger.LogInformation("Updated task {TaskId}: {Title}", updatedTask.Id, updatedTask.Title)
                    return updatedTask
                }
                |> function
                | Ok task -> task
                | Error ex -> raise ex
        } |> Async.StartAsTask
    
    override this.DeleteTask(request: DeleteTaskRequest, context: ServerCallContext) =
        async {
            logger.LogInformation("DeleteTask called with ID: {TaskId}", request.Id)
            
            return
                result {
                    let! _ = validateTaskId request.Id
                    
                    match tasks.TryRemove(request.Id) with
                    | true, removedTask -> 
                        logger.LogInformation("Deleted task {TaskId}: {Title}", removedTask.Id, removedTask.Title)
                        return Empty()
                    | false, _ -> 
                        return! Error (RpcException(Status(StatusCode.NotFound, sprintf "Task with ID '%s' not found" request.Id)))
                }
                |> function
                | Ok empty -> empty
                | Error ex -> raise ex
        } |> Async.StartAsTask
    
    override this.WatchTasks(requestStream: IAsyncStreamReader<WatchTasksRequest>, responseStream: IServerStreamWriter<TaskEvent>, context: ServerCallContext) =
        async {
            logger.LogInformation("WatchTasks called")
            
            try
                // F# async enumeration over the request stream
                let rec processRequests () = async {
                    let! hasNext = requestStream.MoveNext() |> Async.AwaitTask
                    if hasNext && not context.CancellationToken.IsCancellationRequested then
                        let request = requestStream.Current
                        let now = Timestamp.FromDateTime(DateTime.UtcNow)
                        
                        // Determine which tasks to send based on request using pattern matching
                        let tasksToSend = 
                            match request.WatchAll, request.TaskIds.Count, String.IsNullOrEmpty(request.AssignedTo) with
                            | true, _, _ -> 
                                tasks.Values |> Seq.toList
                            | false, count, _ when count > 0 ->
                                request.TaskIds 
                                |> Seq.choose (fun taskId -> 
                                    match tasks.TryGetValue(taskId) with
                                    | true, task -> Some task
                                    | false, _ -> None)
                                |> Seq.toList
                            | false, 0, false ->
                                tasks.Values 
                                |> Seq.filter (fun t -> t.AssignedTo = request.AssignedTo)
                                |> Seq.toList
                            | _ -> []
                        
                        // Send events for selected tasks
                        for task in tasksToSend do
                            let taskEvent = TaskEvent(
                                EventType = EventType.Updated,
                                Task = task,
                                Timestamp = now
                            )
                            do! responseStream.WriteAsync(taskEvent) |> Async.AwaitTask
                        
                        // Continue processing
                        return! processRequests()
                }
                
                do! processRequests()
                
            with
            | ex -> 
                logger.LogError(ex, "Error in WatchTasks")
                raise (RpcException(Status(StatusCode.Internal, "Internal server error")))
        } |> Async.StartAsTask :> System.Threading.Tasks.Task