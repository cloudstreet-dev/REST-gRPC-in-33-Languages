# Chapter 14: F#

F# is Microsoft's functional-first programming language that brings the power of functional programming to the .NET ecosystem. As a multi-paradigm language that seamlessly blends functional, object-oriented, and imperative programming styles, F# offers a unique perspective on API development with its emphasis on immutability, type inference, and expressive syntax.

## Language Overview

F# combines functional programming principles with the robustness of the .NET platform:

- **Functional-First**: Immutable data structures and functions as first-class citizens
- **Type Inference**: Powerful type system that infers types automatically
- **Pattern Matching**: Comprehensive pattern matching for control flow
- **Concurrency**: Built-in async workflows and parallel processing
- **Interoperability**: Full integration with .NET and C# libraries
- **Brevity**: Concise syntax that reduces boilerplate code

### Key F# Features for API Development

F#'s functional nature brings unique advantages to API development:

```fsharp
// Immutable record types
type Task = {
    Id: string
    Title: string
    Description: string option
    Status: TaskStatus
    Priority: TaskPriority
    Tags: string list
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

// Discriminated unions for modeling domain states
type TaskStatus = 
    | Pending 
    | InProgress 
    | Completed

type ApiResult<'T> =
    | Success of 'T
    | Error of string
    | NotFound

// Pattern matching for control flow
let processTask task =
    match task.Status with
    | Pending -> "Task is waiting to be started"
    | InProgress -> "Task is currently being worked on"
    | Completed -> "Task has been finished"

// Pipeline operator for function composition
let processTaskRequest request =
    request
    |> validateRequest
    |> createTask
    |> saveToDatabase
    |> generateResponse
```

## Development Environment Setup

### Installing F# and .NET

F# is included with .NET SDK:

```bash
# Install .NET 8 SDK (includes F#)
# Windows
winget install Microsoft.DotNet.SDK.8

# macOS
brew install dotnet

# Linux (Ubuntu)
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
sudo apt update && sudo apt install dotnet-sdk-8.0

# Verify installation
dotnet --version
```

### F# Project Structure

F# projects follow a specific compilation order due to the language's dependency rules:

```
fsharp-project/
├── TaskApi.sln
├── src/
│   ├── TaskApi/
│   │   ├── TaskApi.fsproj
│   │   ├── Models.fs          # Domain models (compiled first)
│   │   ├── Database.fs        # Data access layer
│   │   ├── Controllers.fs     # API controllers
│   │   └── Program.fs         # Application entry point
│   └── TaskApi.Client/
│       ├── TaskApi.Client.fsproj
│       ├── Models.fs
│       ├── ApiClient.fs
│       └── Program.fs
└── tests/
    └── TaskApi.Tests/
        └── TaskApi.Tests.fsproj
```

### F# Interactive (FSI)

F# includes an interactive REPL for rapid development:

```fsharp
// Start F# Interactive
dotnet fsi

// Load and test functions
#load "Models.fs";;
open TaskApi.Models;;

let sampleTask = { 
    Id = "123"
    Title = "Sample Task"
    Description = Some "This is a test"
    Status = Pending
    Priority = High
    Tags = ["test"; "sample"]
    CreatedAt = DateTime.Now
    UpdatedAt = DateTime.Now
};;
```

## REST API Implementation with ASP.NET Core

F# integrates seamlessly with ASP.NET Core, bringing functional programming benefits to web API development.

### Server Implementation

The F# server leverages the language's type system and functional features:

```fsharp
// TaskApi.fsproj - F# compilation order matters
<ItemGroup>
  <Compile Include="Models.fs" />
  <Compile Include="Database.fs" />
  <Compile Include="Controllers.fs" />
  <Compile Include="Program.fs" />
</ItemGroup>
```

F# models use immutable records and discriminated unions:

```fsharp
module TaskRestServer.Models

open System
open System.ComponentModel.DataAnnotations
open System.Text.Json.Serialization

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type TaskStatus =
    | Pending = 0
    | InProgress = 1
    | Completed = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type TaskPriority =
    | Low = 0
    | Medium = 1
    | High = 2
    | Critical = 3

[<CLIMutable>] // Enables mutable properties for JSON serialization
type Task = {
    [<Key>]
    Id: string
    
    [<Required>]
    [<StringLength(200, ErrorMessage = "Title must be 200 characters or less")>]
    Title: string
    
    Description: string option
    Status: TaskStatus
    Priority: TaskPriority
    Tags: string list
    AssignedTo: string option
    CreatedBy: string
    CreatedAt: DateTime
    UpdatedAt: DateTime
    DueDate: DateTime option
}
```

The database module uses functional composition:

```fsharp
module TaskRestServer.Database

open System
open Microsoft.EntityFrameworkCore
open TaskRestServer.Models

type TaskDbContext(options: DbContextOptions<TaskDbContext>) =
    inherit DbContext(options)
    
    [<DefaultValue>]
    val mutable private _tasks: DbSet<Task>
    member this.Tasks 
        with get() = this._tasks
        and set v = this._tasks <- v

module DatabaseHelpers =
    let createSampleTasks () =
        let now = DateTime.UtcNow
        
        [
            {
                Id = Guid.NewGuid().ToString()
                Title = "Complete F# project documentation"
                Description = Some "Write comprehensive documentation for the F# REST API"
                Status = TaskStatus.InProgress
                Priority = TaskPriority.High
                Tags = ["documentation"; "fsharp"; "api"]
                AssignedTo = Some "dev-team"
                CreatedBy = "system"
                CreatedAt = now
                UpdatedAt = now
                DueDate = None
            }
            // Additional tasks...
        ]
    
    let initializeDatabase (context: TaskDbContext) =
        async {
            do! context.Database.EnsureCreatedAsync() |> Async.AwaitTask
            
            let! existingCount = 
                context.Tasks.CountAsync() 
                |> Async.AwaitTask
            
            if existingCount = 0 then
                let sampleTasks = createSampleTasks ()
                context.Tasks.AddRange(sampleTasks)
                let! _ = context.SaveChangesAsync() |> Async.AwaitTask
                ()
        }
```

The controller uses F#'s pattern matching and option types:

```fsharp
[<ApiController>]
[<Route("api/[controller]")>]
type TasksController(context: TaskDbContext, logger: ILogger<TasksController>) =
    inherit ControllerBase()
    
    [<HttpGet>]
    member this.GetTasks(
        [<FromQuery>] status: TaskStatus option,
        [<FromQuery>] assignedTo: string option,
        [<FromQuery>] pageSize: int option
    ) = 
        async {
            try
                let pageSize = min (defaultArg pageSize 20) 100
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

                let! totalCount = query.CountAsync() |> Async.AwaitTask
                let! tasks = query.Take(pageSize).ToListAsync() |> Async.AwaitTask

                let response = {
                    Tasks = tasks |> List.ofSeq
                    TotalCount = totalCount
                    NextPageToken = None
                }

                return this.Ok(response) :> ActionResult<ListTasksResponse>
            with
            | ex ->
                logger.LogError(ex, "Error retrieving tasks")
                return this.StatusCode(500) :> ActionResult<ListTasksResponse>
        } |> Async.StartAsTask

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

                    return this.CreatedAtAction("GetTask", {| id = task.Id |}, task) :> ActionResult<Task>
            with
            | ex ->
                logger.LogError(ex, "Error creating task")
                return this.StatusCode(500) :> ActionResult<Task>
        } |> Async.StartAsTask
```

### Client Implementation

The F# client demonstrates functional error handling and composition:

```fsharp
module TaskRestClient.ApiClient

type TaskApiError = 
    | InvalidUrl of string
    | InvalidResponse of string
    | ServerError of int * string
    | DecodingError of string
    | NetworkError of string

type TaskApiClient(baseUrl: string) =
    let httpClient = 
        let client = new HttpClient(BaseAddress = Uri(baseUrl))
        client.DefaultRequestHeaders.Add("Accept", "application/json")
        client
    
    let handleResponse<'T> (response: HttpResponseMessage) = 
        async {
            if response.IsSuccessStatusCode then
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                try
                    let result = JsonSerializer.Deserialize<'T>(content, jsonOptions)
                    return Ok result
                with
                | ex -> 
                    return Error (DecodingError (sprintf "Failed to deserialize: %s" ex.Message))
            else
                let! errorContent = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                return Error (ServerError (int response.StatusCode, errorContent))
        }

    member this.ListTasksAsync(?status, ?assignedTo, ?pageSize) = 
        async {
            try
                let parameters = [
                    ("page_size", pageSize |> Option.map string |> Option.defaultValue "20" |> Some)
                    ("status", status |> Option.map (fun s -> s.ToString().ToLower()))
                    ("assigned_to", assignedTo)
                ]
                
                let queryString = buildQueryString parameters
                let url = sprintf "api/tasks?%s" queryString
                
                let! response = httpClient.GetAsync(url) |> Async.AwaitTask
                return! handleResponse<ListTasksResponse> response
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }
```

### F# Program Structure

F# applications use a functional approach to configuration:

```fsharp
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open TaskRestServer.Database

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    // Configure services using F# pipeline
    builder.Services.AddControllers()
        .AddJsonOptions(fun options ->
            options.JsonSerializerOptions.Converters.Add(JsonStringEnumConverter())
        ) |> ignore

    let connectionString = 
        match builder.Configuration.GetConnectionString("DefaultConnection") with
        | null -> "Data Source=tasks.db"
        | conn -> conn

    builder.Services.AddDbContext<TaskDbContext>(fun options ->
        options.UseSqlite(connectionString) |> ignore
    ) |> ignore

    let app = builder.Build()

    // Configure pipeline
    if app.Environment.IsDevelopment() then
        app.UseSwagger() |> ignore
        app.UseSwaggerUI() |> ignore

    app.UseRouting() |> ignore
    app.MapControllers() |> ignore

    // Initialize database using F# async
    use scope = app.Services.CreateScope()
    let context = scope.ServiceProvider.GetRequiredService<TaskDbContext>()
    
    DatabaseHelpers.initializeDatabase context
    |> Async.RunSynchronously

    app.Run("http://localhost:8080")
    0
```

## gRPC Implementation with ASP.NET Core

F#'s functional programming model brings unique benefits to gRPC service implementation.

### Server Implementation

The F# gRPC server uses functional composition and pattern matching:

```fsharp
module TaskGrpcServer.TaskService

open System
open System.Collections.Concurrent
open Microsoft.Extensions.Logging
open Grpc.Core
open TaskNamespace

type TaskServiceImplementation(logger: ILogger<TaskServiceImplementation>) =
    inherit TaskService.TaskServiceBase()
    
    let tasks = ConcurrentDictionary<string, Task>()
    
    // Helper functions using F# functional programming
    let validateTaskId (id: string) =
        match String.IsNullOrEmpty(id) with
        | true -> Error (RpcException(Status(StatusCode.InvalidArgument, "Task ID is required")))
        | false -> Ok id
    
    let validateTaskTitle (title: string) =
        match String.IsNullOrWhiteSpace(title) with
        | true -> Error (RpcException(Status(StatusCode.InvalidArgument, "Title is required")))
        | false when title.Length > 200 -> Error (RpcException(Status(StatusCode.InvalidArgument, "Title too long")))
        | false -> Ok title
    
    let findTask (id: string) =
        match tasks.TryGetValue(id) with
        | true, task -> Ok task
        | false, _ -> Error (RpcException(Status(StatusCode.NotFound, sprintf "Task '%s' not found" id)))
    
    // Functional filtering pipeline
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

    override this.CreateTask(request: CreateTaskRequest, context: ServerCallContext) =
        async {
            return
                result {
                    do! 
                        match request.Task with
                        | null -> Error (RpcException(Status(StatusCode.InvalidArgument, "Task required")))
                        | _ -> Ok ()
                    
                    let! _ = validateTaskTitle request.Task.Title
                    
                    let now = Timestamp.FromDateTime(DateTime.UtcNow)
                    let task = Task(
                        Id = Guid.NewGuid().ToString(),
                        Title = request.Task.Title,
                        Status = TaskStatus.Pending,
                        CreatedAt = now,
                        UpdatedAt = now
                    )
                    
                    task.Tags.AddRange(request.Task.Tags)
                    tasks.[task.Id] <- task
                    
                    return task
                }
                |> function
                | Ok task -> task
                | Error ex -> raise ex
        } |> Async.StartAsTask
```

### Client Implementation

The F# gRPC client demonstrates functional async patterns:

```fsharp
open System
open System.CommandLine
open Grpc.Net.Client
open TaskNamespace

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

// Demo command using F# async workflows
let demoCommand = Command("demo", "Run gRPC API demonstration")

demoCommand.SetHandler(fun (address: string) ->
    async {
        use channel = GrpcHelpers.createChannel address
        let client = GrpcHelpers.createClient channel
        
        try
            printfn "F# Task gRPC Client Demo"
            
            // Create task using F# syntax
            let createRequest = CreateTaskRequest(
                Task = Task(
                    Title = "Test F# gRPC Client",
                    Priority = TaskPriority.High,
                    AssignedTo = "dev-team"
                )
            )
            
            let! newTask = client.CreateTaskAsync(createRequest) |> Async.AwaitTask
            printfn "Created task: %s" newTask.Title
            
            // List tasks using F# async sequence processing
            let listRequest = ListTasksRequest(PageSize = 10)
            let listCall = client.ListTasks(listRequest)
            
            let rec readTasks () = async {
                let! hasNext = listCall.ResponseStream.MoveNext() |> Async.AwaitTask
                if hasNext then
                    let task = listCall.ResponseStream.Current
                    printfn "Found task: %s" task.Title
                    return! readTasks()
            }
            
            do! readTasks()
            
        with
        | ex -> GrpcHelpers.handleGrpcException ex
    } |> Async.RunSynchronously
, addressOption)
```

## F#-Specific Features and Patterns

### Computation Expressions

F# computation expressions provide powerful abstractions:

```fsharp
// Result computation expression for error handling
type ResultBuilder() =
    member x.Bind(m, f) = 
        match m with
        | Ok value -> f value
        | Error e -> Error e
    
    member x.Return(value) = Ok value
    member x.ReturnFrom(m) = m

let result = ResultBuilder()

// Usage in API methods
let processTaskRequest request =
    result {
        let! validatedRequest = validateRequest request
        let! task = createTask validatedRequest
        let! savedTask = saveTask task
        return generateResponse savedTask
    }

// Async computation expression for concurrent operations
let fetchMultipleTasks taskIds =
    async {
        let! tasks = 
            taskIds
            |> List.map (fun id -> async { return! fetchTask id })
            |> Async.Parallel
        
        return tasks |> Array.toList
    }
```

### Type Providers

F# type providers enable compile-time code generation:

```fsharp
// JSON type provider for API schemas
type TaskSchema = JsonProvider<"""
{
    "id": "123",
    "title": "Sample Task",
    "status": "pending",
    "priority": "high",
    "tags": ["work", "urgent"],
    "created_at": "2024-01-01T00:00:00Z"
}
""">

// Usage with strong typing
let parseTaskJson (json: string) =
    let task = TaskSchema.Parse(json)
    {
        Id = task.Id
        Title = task.Title
        Status = parseStatus task.Status
        Priority = parsePriority task.Priority
        Tags = task.Tags |> Array.toList
        CreatedAt = task.CreatedAt
    }
```

### Active Patterns

Active patterns provide flexible pattern matching:

```fsharp
// Active patterns for HTTP status codes
let (|Success|ClientError|ServerError|) (statusCode: int) =
    match statusCode with
    | code when code >= 200 && code < 300 -> Success
    | code when code >= 400 && code < 500 -> ClientError code
    | code when code >= 500 -> ServerError code
    | _ -> ClientError statusCode

// Usage in API client
let handleResponse response =
    match int response.StatusCode with
    | Success -> 
        async { 
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Ok (deserialize content)
        }
    | ClientError code -> 
        async { return Error (sprintf "Client error: %d" code) }
    | ServerError code -> 
        async { return Error (sprintf "Server error: %d" code) }

// Active patterns for validation
let (|ValidEmail|InvalidEmail|) (email: string) =
    if email.Contains("@") && email.Contains(".") then
        ValidEmail email
    else
        InvalidEmail

let (|NonEmptyString|EmptyString|) (str: string) =
    if String.IsNullOrWhiteSpace(str) then EmptyString
    else NonEmptyString str.Trim()

// Validation with active patterns
let validateTaskRequest request =
    match request.Title, request.AssignedTo with
    | NonEmptyString title, Some (ValidEmail email) -> 
        Ok { request with Title = title; AssignedTo = Some email }
    | EmptyString, _ -> 
        Error "Title cannot be empty"
    | _, Some InvalidEmail -> 
        Error "Invalid email format"
    | NonEmptyString title, assignedTo -> 
        Ok { request with Title = title; AssignedTo = assignedTo }
```

### Units of Measure

F# units of measure provide compile-time dimensional analysis:

```fsharp
// Define units for API rate limiting
[<Measure>] type req // requests
[<Measure>] type sec // seconds
[<Measure>] type min // minutes

type RateLimit = {
    RequestsPerSecond: int<req/sec>
    RequestsPerMinute: int<req/min>
    BurstCapacity: int<req>
}

// Usage with type safety
let apiRateLimit = {
    RequestsPerSecond = 10<req/sec>
    RequestsPerMinute = 100<req/min>
    BurstCapacity = 20<req>
}

// Functions with units
let calculateWaitTime (currentRate: int<req/sec>) (limit: int<req/sec>) : float<sec> =
    if currentRate > limit then
        float (currentRate / limit) * 1.0<sec>
    else
        0.0<sec>
```

## Testing in F#

F# provides excellent testing capabilities with various frameworks:

### Unit Testing with Expecto

```fsharp
// Expecto tests with F# syntax
module TaskApiTests

open Expecto
open TaskRestServer.Models
open TaskRestServer.Controllers

let createSampleTask title =
    {
        Id = "test-id"
        Title = title
        Description = None
        Status = TaskStatus.Pending
        Priority = TaskPriority.Medium
        Tags = []
        AssignedTo = None
        CreatedBy = "test"
        CreatedAt = DateTime.UtcNow
        UpdatedAt = DateTime.UtcNow
        DueDate = None
    }

let taskValidationTests =
    testList "Task Validation" [
        test "Valid task should pass validation" {
            let task = createSampleTask "Valid Task"
            let result = validateTask task
            
            match result with
            | Ok _ -> ()
            | Error msg -> failtest msg
        }
        
        test "Empty title should fail validation" {
            let task = createSampleTask ""
            let result = validateTask task
            
            match result with
            | Error msg -> Expect.contains msg "Title" "Should mention title"
            | Ok _ -> failtest "Should have failed validation"
        }
        
        testParam "Title length validation" [""; "a"; String.replicate 201 "a"] <| fun title ->
            let task = createSampleTask title
            let result = validateTask task
            
            match title with
            | "" | _ when title.Length > 200 -> 
                match result with
                | Error _ -> ()
                | Ok _ -> failtest "Should have failed validation"
            | _ ->
                match result with
                | Ok _ -> ()
                | Error msg -> failtest msg
    ]

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
```

### Property-Based Testing with FsCheck

```fsharp
module PropertyTests

open FsCheck
open TaskRestServer.Models

// Generators for test data
let genTaskStatus = 
    Gen.elements [TaskStatus.Pending; TaskStatus.InProgress; TaskStatus.Completed]

let genTaskPriority = 
    Gen.elements [TaskPriority.Low; TaskPriority.Medium; TaskPriority.High; TaskPriority.Critical]

let genValidTitle = 
    Gen.choose (1, 200)
    |> Gen.map (fun length -> String.replicate length "a")

let genTask = gen {
    let! title = genValidTitle
    let! status = genTaskStatus  
    let! priority = genTaskPriority
    let! tags = Gen.listOf (Gen.choose (1, 10) |> Gen.map (fun i -> sprintf "tag%d" i))
    
    return {
        Id = Guid.NewGuid().ToString()
        Title = title
        Description = None
        Status = status
        Priority = priority
        Tags = tags
        AssignedTo = None
        CreatedBy = "test"
        CreatedAt = DateTime.UtcNow
        UpdatedAt = DateTime.UtcNow
        DueDate = None
    }
}

// Property-based tests
let properties = [
    "Valid tasks should always serialize and deserialize correctly", fun () ->
        Prop.forAll (Arb.fromGen genTask) (fun task ->
            let json = JsonSerializer.Serialize(task)
            let deserialized = JsonSerializer.Deserialize<Task>(json)
            task = deserialized
        )
        
    "Task creation should preserve all fields", fun () ->
        Prop.forAll (Arb.fromGen genTask) (fun originalTask ->
            let createdTask = createTaskFromRequest {
                Title = originalTask.Title
                Description = originalTask.Description
                Priority = Some originalTask.Priority
                Tags = Some originalTask.Tags
                AssignedTo = originalTask.AssignedTo
                CreatedBy = Some originalTask.CreatedBy
                DueDate = originalTask.DueDate
            }
            
            createdTask.Title = originalTask.Title &&
            createdTask.Priority = originalTask.Priority &&
            createdTask.Tags = originalTask.Tags
        )
]
```

### Integration Testing

```fsharp
module IntegrationTests

open Microsoft.AspNetCore.Mvc.Testing
open System.Net.Http
open System.Text.Json

let createTestApp () =
    let factory = new WebApplicationFactory<Program>()
    factory.CreateClient()

let integrationTests = testList "Integration Tests" [
    testAsync "GET /api/tasks should return task list" {
        use client = createTestApp()
        
        let! response = client.GetAsync("/api/tasks") |> Async.AwaitTask
        let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        
        response.EnsureSuccessStatusCode() |> ignore
        
        let tasks = JsonSerializer.Deserialize<ListTasksResponse>(content)
        Expect.isGreaterThanOrEqual tasks.Tasks.Length 0 "Should return task list"
    }
    
    testAsync "POST /api/tasks should create new task" {
        use client = createTestApp()
        
        let createRequest = {
            Title = "Integration Test Task"
            Description = Some "Created by integration test"
            Priority = Some TaskPriority.Medium
            Tags = Some ["test"; "integration"]
            AssignedTo = None
            CreatedBy = Some "test-runner"
            DueDate = None
        }
        
        let json = JsonSerializer.Serialize(createRequest)
        let content = new StringContent(json, Text.Encoding.UTF8, "application/json")
        
        let! response = client.PostAsync("/api/tasks", content) |> Async.AwaitTask
        let! responseContent = response.Content.ReadAsStringAsync() |> Async.AwaitTask
        
        Expect.equal (int response.StatusCode) 201 "Should return 201 Created"
        
        let createdTask = JsonSerializer.Deserialize<Task>(responseContent)
        Expect.equal createdTask.Title "Integration Test Task" "Should preserve title"
    }
]
```

## Performance and Optimization

### Memory Efficiency

F#'s immutable data structures provide predictable memory usage:

```fsharp
// Using F# collections for memory efficiency
module PerformantCollections =
    
    // Persistent data structures
    let addTaskToMap (tasks: Map<string, Task>) (task: Task) =
        Map.add task.Id task tasks
    
    // Sequence processing for large datasets
    let processLargeTaskSet tasks =
        tasks
        |> Seq.filter (fun task -> task.Status = TaskStatus.InProgress)
        |> Seq.map (fun task -> { task with UpdatedAt = DateTime.UtcNow })
        |> Seq.cache // Cache computed results
    
    // Lazy evaluation for expensive operations
    let expensiveTaskAnalysis tasks =
        lazy (
            tasks
            |> List.groupBy (fun task -> task.Priority)
            |> List.map (fun (priority, taskList) ->
                priority, taskList |> List.length)
            |> Map.ofList
        )

// Memory pool for frequent allocations
module ObjectPools =
    open System.Buffers
    
    let stringBuilderPool = ArrayPool<char>.Shared
    
    let buildTaskResponse (tasks: Task list) =
        let buffer = stringBuilderPool.Rent(1024)
        try
            // Build response using pooled buffer
            let json = JsonSerializer.Serialize(tasks)
            json
        finally
            stringBuilderPool.Return(buffer)
```

### Concurrent Processing

F# provides excellent concurrent programming primitives:

```fsharp
// Parallel processing with F# async
module ConcurrentProcessing =
    
    let processTasksConcurrently (tasks: Task list) =
        async {
            let! results = 
                tasks
                |> List.map (fun task -> 
                    async {
                        // Simulate processing
                        do! Async.Sleep(100)
                        return processTask task
                    })
                |> Async.Parallel
            
            return results |> Array.toList
        }
    
    // Actor-like pattern with MailboxProcessor
    type TaskProcessor() =
        let processor = MailboxProcessor<Task * AsyncReplyChannel<Result<Task, string>>>.Start(fun inbox ->
            let rec loop () = async {
                let! (task, reply) = inbox.Receive()
                
                try
                    let processedTask = { task with UpdatedAt = DateTime.UtcNow }
                    reply.Reply(Ok processedTask)
                with
                | ex -> reply.Reply(Error ex.Message)
                
                return! loop()
            }
            loop()
        )
        
        member this.ProcessTask(task: Task) =
            processor.PostAndAsyncReply(fun reply -> (task, reply))

// Resource pooling
module ResourceManagement =
    
    type ConnectionPool<'T>(createConnection: unit -> 'T, disposeConnection: 'T -> unit, poolSize: int) =
        let connections = System.Collections.Concurrent.ConcurrentBag<'T>()
        let semaphore = new System.Threading.SemaphoreSlim(poolSize, poolSize)
        
        member this.UseConnection<'U>(operation: 'T -> Async<'U>) =
            async {
                do! semaphore.WaitAsync() |> Async.AwaitTask
                
                let connection = 
                    match connections.TryTake() with
                    | true, conn -> conn
                    | false, _ -> createConnection()
                
                try
                    return! operation connection
                finally
                    connections.Add(connection)
                    semaphore.Release() |> ignore
            }
```

## Production Deployment

### Docker Configuration

F# applications can be efficiently containerized:

```dockerfile
# Dockerfile for F# application
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS base
WORKDIR /app
EXPOSE 8080
EXPOSE 50051

FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src

# Copy F# project files (order matters for F#)
COPY ["TaskApi.fsproj", "."]
RUN dotnet restore "./TaskApi.fsproj"

COPY . .
WORKDIR "/src/."
RUN dotnet build "TaskApi.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "TaskApi.fsproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .

# F# applications can benefit from ReadyToRun images
ENTRYPOINT ["dotnet", "TaskApi.dll"]
```

### Configuration Management

F# applications can use functional configuration patterns:

```fsharp
// Configuration using F# records
type DatabaseConfig = {
    ConnectionString: string
    CommandTimeout: int
    RetryCount: int
}

type ApiConfig = {
    Port: int
    AllowedOrigins: string list
    RateLimit: int
    EnableSwagger: bool
}

type AppConfig = {
    Database: DatabaseConfig
    Api: ApiConfig
    Environment: string
}

// Configuration builder using F# computation expression
type ConfigBuilder() =
    member x.Bind(config, f) = f config
    member x.Return(value) = value
    member x.Zero() = { Database = Unchecked.defaultof<_>; Api = Unchecked.defaultof<_>; Environment = "" }

let config = ConfigBuilder()

let buildConfiguration (configuration: IConfiguration) =
    config {
        let database = {
            ConnectionString = configuration.GetConnectionString("DefaultConnection") |> Option.defaultValue "Data Source=tasks.db"
            CommandTimeout = configuration.GetValue<int>("Database:CommandTimeout", 30)
            RetryCount = configuration.GetValue<int>("Database:RetryCount", 3)
        }
        
        let api = {
            Port = configuration.GetValue<int>("Api:Port", 8080)
            AllowedOrigins = configuration.GetSection("Api:AllowedOrigins").Get<string[]>() |> Option.ofObj |> Option.defaultValue [||] |> Array.toList
            RateLimit = configuration.GetValue<int>("Api:RateLimit", 100)
            EnableSwagger = configuration.GetValue<bool>("Api:EnableSwagger", true)
        }
        
        return {
            Database = database
            Api = api
            Environment = configuration.GetValue<string>("ASPNETCORE_ENVIRONMENT", "Development")
        }
    }
```

### Logging and Monitoring

F# integrates well with .NET logging infrastructure:

```fsharp
// Structured logging with F#
module Logging =
    open Microsoft.Extensions.Logging
    
    // F# logging helpers
    let logInfo (logger: ILogger) template ([<ParamArray>] args: obj[]) =
        logger.LogInformation(template, args)
    
    let logError (logger: ILogger) ex template ([<ParamArray>] args: obj[]) =
        logger.LogError(ex, template, args)
    
    // Usage in controllers
    let handleTaskCreation (logger: ILogger) (task: CreateTaskRequest) =
        async {
            try
                logInfo logger "Creating task with title: {Title}" [| task.Title |]
                
                let! result = createTask task
                
                match result with
                | Ok createdTask -> 
                    logInfo logger "Successfully created task {TaskId}" [| createdTask.Id |]
                    return Ok createdTask
                | Error error ->
                    logError logger null "Failed to create task: {Error}" [| error |]
                    return Error error
            with
            | ex ->
                logError logger ex "Exception occurred while creating task"
                return Error "Internal server error"
        }

// Performance monitoring
module Monitoring =
    open System.Diagnostics
    
    // Activity source for distributed tracing
    let activitySource = new ActivitySource("TaskApi")
    
    let withTracing operationName (operation: unit -> Async<'T>) =
        async {
            use activity = activitySource.StartActivity(operationName)
            
            try
                let! result = operation()
                activity.SetStatus(ActivityStatusCode.Ok) |> ignore
                return result
            with
            | ex ->
                activity.SetStatus(ActivityStatusCode.Error, ex.Message) |> ignore
                reraise()
        }
```

## Best Practices and Patterns

### Functional Domain Modeling

```fsharp
// Domain modeling with F# discriminated unions
module Domain =
    
    type TaskId = TaskId of Guid
    type UserId = UserId of string
    
    type TaskState =
        | Draft of title: string
        | Active of title: string * assignedTo: UserId * dueDate: DateTime option
        | Completed of title: string * completedAt: DateTime * completedBy: UserId
        | Cancelled of reason: string
    
    type TaskCommand =
        | CreateTask of title: string * description: string option
        | AssignTask of TaskId * UserId
        | CompleteTask of TaskId * UserId
        | CancelTask of TaskId * reason: string
    
    type TaskEvent =
        | TaskCreated of TaskId * string * DateTime
        | TaskAssigned of TaskId * UserId * DateTime
        | TaskCompleted of TaskId * UserId * DateTime
        | TaskCancelled of TaskId * string * DateTime
    
    // Command handlers
    let handleCommand (command: TaskCommand) (currentState: TaskState option) : Result<TaskEvent list, string> =
        match command, currentState with
        | CreateTask (title, description), None ->
            let taskId = TaskId (Guid.NewGuid())
            Ok [TaskCreated (taskId, title, DateTime.UtcNow)]
            
        | AssignTask (taskId, userId), Some (Draft title) ->
            Ok [TaskAssigned (taskId, userId, DateTime.UtcNow)]
            
        | CompleteTask (taskId, userId), Some (Active _) ->
            Ok [TaskCompleted (taskId, userId, DateTime.UtcNow)]
            
        | _ -> Error "Invalid command for current state"

// Railway-oriented programming for error handling
module RailwayOriented =
    
    let bind switchFunction twoTrackInput =
        match twoTrackInput with
        | Ok s -> switchFunction s
        | Error f -> Error f
    
    let map oneTrackFunction twoTrackInput =
        match twoTrackInput with
        | Ok s -> Ok (oneTrackFunction s)
        | Error f -> Error f
    
    // Custom operators
    let (>>=) x f = bind f x
    let (<!>) f x = map f x
    
    // Usage in API pipeline
    let processTaskRequest request =
        Ok request
        >>= validateRequest
        >>= enrichWithDefaults  
        >>= saveToDatabase
        <!> formatResponse
```

### Error Handling Patterns

```fsharp
module ErrorHandling =
    
    // Comprehensive error types
    type ApiError =
        | ValidationError of field: string * message: string
        | NotFoundError of entityType: string * id: string
        | ConflictError of message: string
        | InternalError of exn: Exception
    
    // Error conversion for HTTP responses
    let errorToHttpResponse = function
        | ValidationError (field, message) -> 
            (400, sprintf "Validation failed for %s: %s" field message)
        | NotFoundError (entityType, id) -> 
            (404, sprintf "%s with ID %s not found" entityType id)
        | ConflictError message -> 
            (409, message)
        | InternalError ex -> 
            (500, "Internal server error")
    
    // Result builder for clean error handling
    type AsyncResultBuilder() =
        member x.Bind(result, f) = 
            async {
                let! r = result
                match r with
                | Ok value -> return! f value
                | Error err -> return Error err
            }
        
        member x.Return(value) = async { return Ok value }
        member x.ReturnFrom(result) = result
    
    let asyncResult = AsyncResultBuilder()
    
    // Usage example
    let processTaskWithErrorHandling taskId =
        asyncResult {
            let! task = findTask taskId // Returns Async<Result<Task, ApiError>>
            let! validatedTask = validateTask task // Returns Async<Result<Task, ApiError>>  
            let! savedTask = saveTask validatedTask // Returns Async<Result<Task, ApiError>>
            return generateResponse savedTask
        }
```

## F# Ecosystem and Tooling

### Package Management

F# uses the same NuGet ecosystem as other .NET languages:

```fsharp
// Paket for advanced dependency management
// paket.dependencies
source https://api.nuget.org/v3/index.json

nuget Microsoft.AspNetCore.App
nuget FSharp.Core >= 6.0.0
nuget Expecto
nuget FsCheck

// paket.references in project directory
Microsoft.AspNetCore.App
FSharp.Core
Expecto
FsCheck
```

### Development Tools

```bash
# F# specific tools
dotnet tool install -g fantomas-tool    # F# code formatter
dotnet tool install -g fsharp-analyzers # Static analysis

# Usage
fantomas --recurse src/                  # Format all F# files
```

### FAKE Build System

F# has its own build system written in F#:

```fsharp
// build.fsx
#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem //"

open Fake.Core
open Fake.DotNet
open Fake.IO

Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs
)

Target.create "Build" (fun _ ->
    !! "src/**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Test" (fun _ ->
    !! "tests/**/*.*proj"
    |> Seq.iter (DotNet.test id)
)

Target.create "All"

"Clean"
  ==> "Build" 
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"
```

## Conclusion

F# brings the power of functional programming to API development while maintaining full interoperability with the .NET ecosystem. Its emphasis on immutability, type safety, and composable functions creates robust, maintainable APIs with less boilerplate code than traditional object-oriented approaches.

Key advantages of F# for API development:

- **Functional-First Design**: Immutable data structures prevent many common bugs
- **Type Safety**: Powerful type inference with compile-time guarantees
- **Conciseness**: Expressive syntax reduces boilerplate and increases readability
- **Pattern Matching**: Elegant control flow and data processing
- **Async Workflows**: Natural asynchronous programming model
- **.NET Integration**: Full access to the rich .NET ecosystem

F#'s unique approach to API development makes it particularly well-suited for scenarios requiring high reliability, complex data transformations, and mathematical computations. The language's emphasis on correctness and its powerful modeling capabilities make it an excellent choice for domain-rich applications.

Next, we'll explore **Chapter 15: C++**, examining how this systems programming language approaches modern API development with frameworks like Beast and comprehensive gRPC support.