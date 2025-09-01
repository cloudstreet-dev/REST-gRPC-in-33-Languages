open System
open System.CommandLine
open Grpc.Net.Client
open Google.Protobuf.WellKnownTypes
open TaskNamespace

// F# helper modules for gRPC operations
module TaskPrinter =
    let printTask (task: Task) =
        printfn "ID: %s" task.Id
        printfn "Title: %s" task.Title
        if not (String.IsNullOrEmpty(task.Description)) then
            printfn "Description: %s" task.Description
        printfn "Status: %A" task.Status
        printfn "Priority: %A" task.Priority
        if task.Tags.Count > 0 then
            printfn "Tags: %s" (String.concat ", " task.Tags)
        if not (String.IsNullOrEmpty(task.AssignedTo)) then
            printfn "Assigned To: %s" task.AssignedTo
        printfn "Created By: %s" task.CreatedBy
        printfn "Created: %s" (task.CreatedAt.ToDateTime().ToString("yyyy-MM-dd HH:mm:ss"))
        printfn "Updated: %s" (task.UpdatedAt.ToDateTime().ToString("yyyy-MM-dd HH:mm:ss"))
        if task.DueDate <> null then
            printfn "Due Date: %s" (task.DueDate.ToDateTime().ToString("yyyy-MM-dd HH:mm:ss"))

module GrpcHelpers =
    let createChannel (address: string) = 
        GrpcChannel.ForAddress(address)
    
    let createClient (channel: GrpcChannel) =
        new TaskService.TaskServiceClient(channel)
    
    let handleGrpcException (ex: Exception) =
        match ex with
        | :? Grpc.Core.RpcException as rpcEx ->
            printfn "gRPC Error (%A): %s" rpcEx.StatusCode rpcEx.Message
        | _ ->
            printfn "Error: %s" ex.Message
        Environment.Exit(1)

// Command line setup
let addressOption = 
    let option = Option<string>("--address", "The address of the gRPC server")
    option.SetDefaultValue("http://localhost:50051")
    option

let rootCommand = RootCommand("F# Task gRPC Client")
rootCommand.AddGlobalOption(addressOption)

// List tasks command
let listCommand = Command("list", "List all tasks with optional filtering")

let statusOption = Option<TaskStatus>("--status", "Filter by task status")
statusOption.SetDefaultValue(TaskStatus.Unspecified)
let assignedToOption = Option<string>("--assigned-to", "Filter by assignee")
let tagsOption = Option<string[]>("--tags", "Filter by tags")
tagsOption.AllowMultipleArgumentsPerToken <- true
let pageSizeOption = Option<int>("--page-size", "Number of tasks per page")
pageSizeOption.SetDefaultValue(20)
let pageTokenOption = Option<string>("--page-token", "Page token for pagination")

listCommand.AddOption(statusOption)
listCommand.AddOption(assignedToOption)
listCommand.AddOption(tagsOption)
listCommand.AddOption(pageSizeOption)
listCommand.AddOption(pageTokenOption)

listCommand.SetHandler(fun (address: string) (status: TaskStatus) (assignedTo: string) (tags: string[]) (pageSize: int) (pageToken: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            let request = ListTasksRequest(
                Status = status,
                PageSize = pageSize,
                PageToken = (if String.IsNullOrEmpty(pageToken) then "" else pageToken),
                AssignedTo = (if String.IsNullOrEmpty(assignedTo) then "" else assignedTo),
                SortOrder = SortOrder.CreatedDesc
            )
            
            if tags <> null then
                request.Tags.AddRange(tags)
            
            printfn "Listing tasks..."
            
            let call = client.ListTasks(request)
            
            // F# async iteration over gRPC stream
            let rec readTasks () = async {
                let! hasNext = call.ResponseStream.MoveNext() |> Async.AwaitTask
                if hasNext then
                    let task = call.ResponseStream.Current
                    TaskPrinter.printTask task
                    printfn ""
                    return! readTasks()
            }
            
            do! readTasks()
            
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption, statusOption, assignedToOption, tagsOption, pageSizeOption, pageTokenOption)

// Get task command
let getCommand = Command("get", "Get a specific task by ID")
let idArgument = Argument<string>("id", "The task ID")
getCommand.AddArgument(idArgument)

getCommand.SetHandler(fun (address: string) (id: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            let request = GetTaskRequest(Id = id)
            let! task = client.GetTaskAsync(request) |> Async.AwaitTask
            TaskPrinter.printTask task
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption, idArgument)

// Create task command
let createCommand = Command("create", "Create a new task")
let titleArgument = Argument<string>("title", "The task title")
let descriptionOption = Option<string>("--description", "The task description")
let priorityOption = Option<TaskPriority>("--priority", "The task priority")
priorityOption.SetDefaultValue(TaskPriority.Medium)
let createTagsOption = Option<string[]>("--tags", "Task tags")
createTagsOption.AllowMultipleArgumentsPerToken <- true
let createAssignedToOption = Option<string>("--assigned-to", "Assignee")
let createdByOption = Option<string>("--created-by", "Creator")

createCommand.AddArgument(titleArgument)
createCommand.AddOption(descriptionOption)
createCommand.AddOption(priorityOption)
createCommand.AddOption(createTagsOption)
createCommand.AddOption(createAssignedToOption)
createCommand.AddOption(createdByOption)

createCommand.SetHandler(fun (address: string) (title: string) (description: string) (priority: TaskPriority) (tags: string[]) (assignedTo: string) (createdBy: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            let task = Task(
                Title = title,
                Description = (if String.IsNullOrEmpty(description) then "" else description),
                Priority = priority,
                AssignedTo = (if String.IsNullOrEmpty(assignedTo) then "" else assignedTo),
                CreatedBy = (if String.IsNullOrEmpty(createdBy) then "grpc-client" else createdBy)
            )
            
            if tags <> null then
                task.Tags.AddRange(tags)
            
            let request = CreateTaskRequest(Task = task)
            let! createdTask = client.CreateTaskAsync(request) |> Async.AwaitTask
            
            printfn "Task created successfully:"
            TaskPrinter.printTask createdTask
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption, titleArgument, descriptionOption, priorityOption, createTagsOption, createAssignedToOption, createdByOption)

// Update task command
let updateCommand = Command("update", "Update an existing task")
let updateIdArgument = Argument<string>("id", "The task ID")
let updateTitleOption = Option<string>("--title", "New title")
let updateDescriptionOption = Option<string>("--description", "New description")
let updateStatusOption = Option<TaskStatus option>("--status", "New status")
let updatePriorityOption = Option<TaskPriority option>("--priority", "New priority")
let updateTagsOption = Option<string[]>("--tags", "New tags")
updateTagsOption.AllowMultipleArgumentsPerToken <- true
let updateAssignedToOption = Option<string>("--assigned-to", "New assignee")

updateCommand.AddArgument(updateIdArgument)
updateCommand.AddOption(updateTitleOption)
updateCommand.AddOption(updateDescriptionOption)
updateCommand.AddOption(updateStatusOption)
updateCommand.AddOption(updatePriorityOption)
updateCommand.AddOption(updateTagsOption)
updateCommand.AddOption(updateAssignedToOption)

updateCommand.SetHandler(fun (address: string) (id: string) (title: string) (description: string) (status: TaskStatus option) (priority: TaskPriority option) (tags: string[]) (assignedTo: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            let updateTask = Task(Id = id)
            let updateMask = ResizeArray<string>()
            
            // Build update mask based on provided options using pattern matching
            [
                (not (String.IsNullOrEmpty(title)), "title", fun () -> updateTask.Title <- title)
                (not (String.IsNullOrEmpty(description)), "description", fun () -> updateTask.Description <- description)
                (status.IsSome, "status", fun () -> updateTask.Status <- status.Value)
                (priority.IsSome, "priority", fun () -> updateTask.Priority <- priority.Value)
                (tags <> null && tags.Length > 0, "tags", fun () -> updateTask.Tags.AddRange(tags))
                (not (String.IsNullOrEmpty(assignedTo)), "assigned_to", fun () -> updateTask.AssignedTo <- assignedTo)
            ]
            |> List.iter (fun (condition, fieldName, updateAction) ->
                if condition then
                    updateMask.Add(fieldName)
                    updateAction()
            )
            
            let request = UpdateTaskRequest(Task = updateTask)
            request.UpdateMask.AddRange(updateMask)
            
            let! updatedTask = client.UpdateTaskAsync(request) |> Async.AwaitTask
            
            printfn "Task updated successfully:"
            TaskPrinter.printTask updatedTask
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption, updateIdArgument, updateTitleOption, updateDescriptionOption, updateStatusOption, updatePriorityOption, updateTagsOption, updateAssignedToOption)

// Delete task command
let deleteCommand = Command("delete", "Delete a task")
let deleteIdArgument = Argument<string>("id", "The task ID")
deleteCommand.AddArgument(deleteIdArgument)

deleteCommand.SetHandler(fun (address: string) (id: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            let request = DeleteTaskRequest(Id = id)
            let! _ = client.DeleteTaskAsync(request) |> Async.AwaitTask
            printfn "Task %s deleted successfully." id
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption, deleteIdArgument)

// Watch tasks command
let watchCommand = Command("watch", "Watch for task changes")
let watchAllOption = Option<bool>("--all", "Watch all tasks")
let watchTaskIdsOption = Option<string[]>("--task-ids", "Specific task IDs to watch")
watchTaskIdsOption.AllowMultipleArgumentsPerToken <- true
let watchAssignedToOption = Option<string>("--assigned-to", "Watch tasks assigned to user")

watchCommand.AddOption(watchAllOption)
watchCommand.AddOption(watchTaskIdsOption)
watchCommand.AddOption(watchAssignedToOption)

watchCommand.SetHandler(fun (address: string) (watchAll: bool) (taskIds: string[]) (assignedTo: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            use call = client.WatchTasks()
            
            let request = WatchTasksRequest(
                WatchAll = watchAll,
                AssignedTo = (if String.IsNullOrEmpty(assignedTo) then "" else assignedTo)
            )
            
            if taskIds <> null then
                request.TaskIds.AddRange(taskIds)
            
            do! call.RequestStream.WriteAsync(request) |> Async.AwaitTask
            do! call.RequestStream.CompleteAsync() |> Async.AwaitTask
            
            printfn "Watching for task events..."
            
            // F# async iteration over event stream
            let rec readEvents () = async {
                let! hasNext = call.ResponseStream.MoveNext() |> Async.AwaitTask
                if hasNext then
                    let taskEvent = call.ResponseStream.Current
                    printfn "Event: %A at %s" taskEvent.EventType (taskEvent.Timestamp.ToDateTime().ToString("yyyy-MM-dd HH:mm:ss"))
                    TaskPrinter.printTask taskEvent.Task
                    printfn ""
                    return! readEvents()
            }
            
            do! readEvents()
            
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption, watchAllOption, watchTaskIdsOption, watchAssignedToOption)

// Demo command
let demoCommand = Command("demo", "Run a demonstration of the gRPC API")

demoCommand.SetHandler(fun (address: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            printfn "F# Task gRPC Client Demo"
            printfn "========================"
            printfn ""

            // List initial tasks
            printfn "1. Listing initial tasks..."
            let listRequest = ListTasksRequest(
                PageSize = 10,
                SortOrder = SortOrder.CreatedDesc
            )
            
            let listCall = client.ListTasks(listRequest)
            let mutable taskCount = 0
            
            let rec countTasks () = async {
                let! hasNext = listCall.ResponseStream.MoveNext() |> Async.AwaitTask
                if hasNext then
                    taskCount <- taskCount + 1
                    return! countTasks()
            }
            
            do! countTasks()
            printfn "Found %d existing tasks" taskCount
            printfn ""

            // Create a new task
            printfn "2. Creating a new task..."
            let createRequest = CreateTaskRequest(
                Task = Task(
                    Title = "Test F# gRPC Client",
                    Description = "Testing the F# gRPC client implementation",
                    Priority = TaskPriority.High,
                    AssignedTo = "dev-team",
                    CreatedBy = "fsharp-grpc-client"
                )
            )
            createRequest.Task.Tags.AddRange(["test"; "fsharp"; "grpc"])

            let! newTask = client.CreateTaskAsync(createRequest) |> Async.AwaitTask
            printfn "Created task: %s - %s" newTask.Id newTask.Title
            printfn ""

            // Get the created task
            printfn "3. Retrieving the created task..."
            let getRequest = GetTaskRequest(Id = newTask.Id)
            let! retrievedTask = client.GetTaskAsync(getRequest) |> Async.AwaitTask
            printfn "Retrieved: %s (Status: %A)" retrievedTask.Title retrievedTask.Status
            printfn ""

            // Update task status
            printfn "4. Updating task status to InProgress..."
            let updateRequest = UpdateTaskRequest(
                Task = Task(Id = newTask.Id, Status = TaskStatus.InProgress)
            )
            updateRequest.UpdateMask.Add("status")

            let! updatedTask = client.UpdateTaskAsync(updateRequest) |> Async.AwaitTask
            printfn "Updated status: %A" updatedTask.Status
            printfn ""

            // Watch tasks
            printfn "5. Watching for task changes..."
            use watchCall = client.WatchTasks()

            let watchRequest = WatchTasksRequest(WatchAll = true)
            do! watchCall.RequestStream.WriteAsync(watchRequest) |> Async.AwaitTask
            do! watchCall.RequestStream.CompleteAsync() |> Async.AwaitTask

            let mutable eventCount = 0
            let rec watchEvents () = async {
                if eventCount < 3 then
                    let! hasNext = watchCall.ResponseStream.MoveNext() |> Async.AwaitTask
                    if hasNext then
                        let taskEvent = watchCall.ResponseStream.Current
                        printfn "Event: %A - Task: %s" taskEvent.EventType taskEvent.Task.Title
                        eventCount <- eventCount + 1
                        return! watchEvents()
            }
            
            do! watchEvents()
            printfn ""

            // Delete the task
            printfn "6. Deleting the test task..."
            let deleteRequest = DeleteTaskRequest(Id = newTask.Id)
            let! _ = client.DeleteTaskAsync(deleteRequest) |> Async.AwaitTask
            printfn "Task deleted successfully"
            printfn ""

            printfn "Demo completed successfully!"
        with
        | ex -> 
            printfn "Demo failed: %s" ex.Message
            Environment.Exit(1)
    } |> Async.RunSynchronously
, addressOption)

// Add all commands to root
rootCommand.AddCommand(listCommand)
rootCommand.AddCommand(getCommand)
rootCommand.AddCommand(createCommand)
rootCommand.AddCommand(updateCommand)
rootCommand.AddCommand(deleteCommand)
rootCommand.AddCommand(watchCommand)
rootCommand.AddCommand(demoCommand)

[<EntryPoint>]
let main args =
    rootCommand.Invoke(args)