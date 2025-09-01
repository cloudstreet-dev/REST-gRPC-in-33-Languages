open System
open System.CommandLine
open TaskRestClient.Models
open TaskRestClient.ApiClient

let baseUrlOption = 
    let option = Option<string>("--base-url", "The base URL of the Task API server")
    option.SetDefaultValue("http://localhost:8080")
    option

let rootCommand = RootCommand("F# Task REST API Client")
rootCommand.AddGlobalOption(baseUrlOption)

// List tasks command
let listCommand = Command("list", "List all tasks with optional filtering")

let statusOption = Option<TaskStatus option>("--status", "Filter by task status")
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

listCommand.SetHandler(fun (baseUrl: string) (status: TaskStatus option) (assignedTo: string) (tags: string[]) (pageSize: int) (pageToken: string) ->
    async {
        use client = new TaskApiClient(baseUrl)
        
        let tagsList = 
            match tags with
            | null -> None
            | [||] -> None
            | tags -> Some (Array.toList tags)
            
        let assignedToOpt = if String.IsNullOrEmpty(assignedTo) then None else Some assignedTo
        let pageTokenOpt = if String.IsNullOrEmpty(pageToken) then None else Some pageToken
        
        let! result = client.ListTasksAsync(?status=status, ?assignedTo=assignedToOpt, ?tags=tagsList, pageSize=pageSize, ?pageToken=pageTokenOpt)
        
        match result with
        | Ok response ->
            printfn "Found %d tasks:" response.TotalCount
            printfn ""
            
            response.Tasks
            |> List.iter (fun task ->
                TaskPrinter.printTask task
                printfn ""
            )
            
            match response.NextPageToken with
            | Some token -> printfn "Next page token: %s" token
            | None -> ()
        | Error error ->
            TaskPrinter.printError error
            Environment.Exit(1)
    } |> Async.RunSynchronously
, baseUrlOption, statusOption, assignedToOption, tagsOption, pageSizeOption, pageTokenOption)

// Get task command
let getCommand = Command("get", "Get a specific task by ID")
let idArgument = Argument<string>("id", "The task ID")
getCommand.AddArgument(idArgument)

getCommand.SetHandler(fun (baseUrl: string) (id: string) ->
    async {
        use client = new TaskApiClient(baseUrl)
        let! result = client.GetTaskAsync(id)
        
        match result with
        | Ok task -> TaskPrinter.printTask task
        | Error error ->
            TaskPrinter.printError error
            Environment.Exit(1)
    } |> Async.RunSynchronously
, baseUrlOption, idArgument)

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

createCommand.SetHandler(fun (baseUrl: string) (title: string) (description: string) (priority: TaskPriority) (tags: string[]) (assignedTo: string) (createdBy: string) ->
    async {
        use client = new TaskApiClient(baseUrl)
        
        let request = {
            Title = title
            Description = if String.IsNullOrEmpty(description) then None else Some description
            Priority = Some priority
            Tags = match tags with | null | [||] -> None | tags -> Some (Array.toList tags)
            AssignedTo = if String.IsNullOrEmpty(assignedTo) then None else Some assignedTo
            CreatedBy = if String.IsNullOrEmpty(createdBy) then None else Some createdBy
            DueDate = None
        }
        
        let! result = client.CreateTaskAsync(request)
        
        match result with
        | Ok task ->
            printfn "Task created successfully:"
            TaskPrinter.printTask task
        | Error error ->
            TaskPrinter.printError error
            Environment.Exit(1)
    } |> Async.RunSynchronously
, baseUrlOption, titleArgument, descriptionOption, priorityOption, createTagsOption, createAssignedToOption, createdByOption)

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

updateCommand.SetHandler(fun (baseUrl: string) (id: string) (title: string) (description: string) (status: TaskStatus option) (priority: TaskPriority option) (tags: string[]) (assignedTo: string) ->
    async {
        use client = new TaskApiClient(baseUrl)
        
        let request = {
            Title = if String.IsNullOrEmpty(title) then None else Some title
            Description = if String.IsNullOrEmpty(description) then None else Some description
            Status = status
            Priority = priority
            Tags = match tags with | null | [||] -> None | tags -> Some (Array.toList tags)
            AssignedTo = if String.IsNullOrEmpty(assignedTo) then None else Some assignedTo
            DueDate = None
        }
        
        let! result = client.UpdateTaskAsync(id, request)
        
        match result with
        | Ok task ->
            printfn "Task updated successfully:"
            TaskPrinter.printTask task
        | Error error ->
            TaskPrinter.printError error
            Environment.Exit(1)
    } |> Async.RunSynchronously
, baseUrlOption, updateIdArgument, updateTitleOption, updateDescriptionOption, updateStatusOption, updatePriorityOption, updateTagsOption, updateAssignedToOption)

// Delete task command
let deleteCommand = Command("delete", "Delete a task")
let deleteIdArgument = Argument<string>("id", "The task ID")
deleteCommand.AddArgument(deleteIdArgument)

deleteCommand.SetHandler(fun (baseUrl: string) (id: string) ->
    async {
        use client = new TaskApiClient(baseUrl)
        let! result = client.DeleteTaskAsync(id)
        
        match result with
        | Ok () -> printfn "Task %s deleted successfully." id
        | Error error ->
            TaskPrinter.printError error
            Environment.Exit(1)
    } |> Async.RunSynchronously
, baseUrlOption, deleteIdArgument)

// Demo command
let demoCommand = Command("demo", "Run a demonstration of the API")

demoCommand.SetHandler(fun (baseUrl: string) ->
    async {
        use client = new TaskApiClient(baseUrl)
        
        try
            printfn "F# Task REST Client Demo"
            printfn "======================="
            printfn ""

            // List initial tasks
            printfn "1. Listing initial tasks..."
            let! initialResult = client.ListTasksAsync()
            match initialResult with
            | Ok response -> printfn "Found %d tasks" response.TotalCount
            | Error error -> 
                TaskPrinter.printError error
                Environment.Exit(1)
            printfn ""

            // Create a new task
            printfn "2. Creating a new task..."
            let createRequest = {
                Title = "Test F# REST Client"
                Description = Some "Testing the F# REST client implementation"
                Priority = Some TaskPriority.High
                Tags = Some ["test"; "fsharp"; "rest"]
                AssignedTo = Some "dev-team"
                CreatedBy = Some "fsharp-client"
                DueDate = None
            }
            
            let! createResult = client.CreateTaskAsync(createRequest)
            let newTask = 
                match createResult with
                | Ok task -> 
                    printfn "Created task: %s - %s" task.Id task.Title
                    task
                | Error error -> 
                    TaskPrinter.printError error
                    Environment.Exit(1)
                    failwith "Unreachable"
            printfn ""

            // Get the created task
            printfn "3. Retrieving the created task..."
            let! getResult = client.GetTaskAsync(newTask.Id)
            match getResult with
            | Ok retrievedTask -> 
                printfn "Retrieved: %s (Status: %A)" retrievedTask.Title retrievedTask.Status
            | Error error -> 
                TaskPrinter.printError error
            printfn ""

            // Update task status
            printfn "4. Updating task status to InProgress..."
            let! statusResult = client.UpdateTaskStatusAsync(newTask.Id, TaskStatus.InProgress)
            match statusResult with
            | Ok updatedTask -> 
                printfn "Updated status: %A" updatedTask.Status
            | Error error -> 
                TaskPrinter.printError error
            printfn ""

            // Update task with more fields
            printfn "5. Updating task details..."
            let updateRequest = {
                Title = Some "Test F# REST Client - Updated"
                Description = None
                Status = Some TaskStatus.Completed
                Priority = Some TaskPriority.Critical
                Tags = None
                AssignedTo = None
                DueDate = None
            }
            
            let! updateResult = client.UpdateTaskAsync(newTask.Id, updateRequest)
            match updateResult with
            | Ok fullyUpdatedTask ->
                printfn "Updated task: %s" fullyUpdatedTask.Title
                printfn "Status: %A, Priority: %A" fullyUpdatedTask.Status fullyUpdatedTask.Priority
            | Error error -> 
                TaskPrinter.printError error
            printfn ""

            // List tasks with filtering
            printfn "6. Listing completed tasks..."
            let! completedResult = client.ListTasksAsync(status = TaskStatus.Completed)
            match completedResult with
            | Ok response -> 
                printfn "Found %d completed tasks" response.Tasks.Length
            | Error error -> 
                TaskPrinter.printError error
            printfn ""

            // Delete the task
            printfn "7. Deleting the test task..."
            let! deleteResult = client.DeleteTaskAsync(newTask.Id)
            match deleteResult with
            | Ok () -> printfn "Task deleted successfully"
            | Error error -> 
                TaskPrinter.printError error
            printfn ""

            printfn "Demo completed successfully!"
        with
        | ex ->
            printfn "Demo failed: %s" ex.Message
            Environment.Exit(1)
    } |> Async.RunSynchronously
, baseUrlOption)

rootCommand.AddCommand(listCommand)
rootCommand.AddCommand(getCommand)
rootCommand.AddCommand(createCommand)
rootCommand.AddCommand(updateCommand)
rootCommand.AddCommand(deleteCommand)
rootCommand.AddCommand(demoCommand)

[<EntryPoint>]
let main args =
    rootCommand.Invoke(args)