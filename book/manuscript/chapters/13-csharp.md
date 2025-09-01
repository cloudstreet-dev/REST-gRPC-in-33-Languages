# Chapter 13: C#

C# (pronounced "C-sharp") is Microsoft's flagship programming language, designed as part of the .NET ecosystem. With its strong type system, modern language features, and excellent tooling, C# provides a robust foundation for building both REST APIs and gRPC services. The language's evolution from a Java-influenced object-oriented language to a multi-paradigm powerhouse makes it an excellent choice for enterprise API development.

## Language Overview

C# combines object-oriented principles with functional programming features and modern language constructs:

- **Strong Type System**: Static typing with nullable reference types
- **Memory Management**: Automatic garbage collection with deterministic disposal
- **Cross-Platform**: Runs on Windows, macOS, and Linux via .NET
- **Performance**: Just-in-time (JIT) compilation with ahead-of-time (AOT) options
- **Rich Ecosystem**: Extensive base class library and NuGet package manager
- **Language Features**: Generics, LINQ, async/await, pattern matching, and more

### Key C# Features for API Development

C#'s modern features excel in API development scenarios:

```csharp
// Nullable reference types for better null safety
public class Task
{
    public string Id { get; set; } = string.Empty;
    public string Title { get; set; } = string.Empty;
    public string? Description { get; set; } // Nullable
    public DateTime CreatedAt { get; set; }
}

// Pattern matching with switch expressions
public static string GetStatusDisplay(TaskStatus status) => status switch
{
    TaskStatus.Pending => "⏳ Pending",
    TaskStatus.InProgress => "🔄 In Progress", 
    TaskStatus.Completed => "✅ Completed",
    _ => "❓ Unknown"
};

// LINQ for data querying
var highPriorityTasks = tasks
    .Where(t => t.Priority == TaskPriority.High)
    .OrderBy(t => t.DueDate)
    .ToList();

// Async/await for non-blocking operations
public async Task<List<Task>> GetTasksAsync()
{
    return await _context.Tasks.ToListAsync();
}
```

## Development Environment Setup

### Installing .NET

**.NET 8 (recommended):**

```bash
# Windows (via winget)
winget install Microsoft.DotNet.SDK.8

# macOS (via Homebrew)
brew install dotnet

# Linux (Ubuntu)
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
sudo apt update && sudo apt install dotnet-sdk-8.0
```

**Verify installation:**
```bash
dotnet --version
# Should output: 8.0.x
```

### Project Structure

.NET uses a project file-based system with clear separation of concerns:

```
csharp-project/
├── TaskApi.sln                 # Solution file
├── src/
│   ├── TaskApi/
│   │   ├── TaskApi.csproj     # Project file
│   │   ├── Program.cs         # Application entry point
│   │   ├── appsettings.json   # Configuration
│   │   ├── Controllers/       # REST controllers
│   │   ├── Models/           # Data models
│   │   ├── Services/         # Business logic
│   │   └── Data/            # Data access layer
│   └── TaskApi.Client/
│       ├── TaskApi.Client.csproj
│       └── Program.cs
└── tests/
    └── TaskApi.Tests/
        └── TaskApi.Tests.csproj
```

## REST API Implementation with ASP.NET Core

ASP.NET Core provides a comprehensive framework for building REST APIs with built-in dependency injection, middleware pipeline, and extensive configuration options.

### Server Implementation

Our ASP.NET Core server uses Entity Framework Core for data persistence and follows RESTful conventions:

```csharp
// TaskApi.csproj
<Project Sdk="Microsoft.NET.Sdk.Web">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.EntityFrameworkCore.Sqlite" Version="8.0.0" />
    <PackageReference Include="Microsoft.EntityFrameworkCore.Tools" Version="8.0.0" />
    <PackageReference Include="Microsoft.AspNetCore.OpenApi" Version="8.0.0" />
    <PackageReference Include="Swashbuckle.AspNetCore" Version="6.4.0" />
  </ItemGroup>
</Project>
```

The Task model leverages C#'s rich type system and data annotations:

```csharp
public enum TaskStatus { Pending, InProgress, Completed }
public enum TaskPriority { Low, Medium, High, Critical }

public class Task
{
    public string Id { get; set; } = string.Empty;
    
    [Required]
    [StringLength(200, ErrorMessage = "Title must be 200 characters or less")]
    public string Title { get; set; } = string.Empty;
    
    [StringLength(1000, ErrorMessage = "Description must be 1000 characters or less")]
    public string? Description { get; set; }
    
    public TaskStatus Status { get; set; } = TaskStatus.Pending;
    public TaskPriority Priority { get; set; } = TaskPriority.Medium;
    public List<string> Tags { get; set; } = new();
    public string? AssignedTo { get; set; }
    public string CreatedBy { get; set; } = "system";
    public DateTime CreatedAt { get; set; }
    public DateTime UpdatedAt { get; set; }
    public DateTime? DueDate { get; set; }
}
```

The Entity Framework DbContext handles data persistence with SQLite:

```csharp
public class TaskDbContext : DbContext
{
    public TaskDbContext(DbContextOptions<TaskDbContext> options) : base(options) { }
    
    public DbSet<Task> Tasks { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<Task>(entity =>
        {
            entity.HasKey(e => e.Id);
            entity.Property(e => e.Id).HasMaxLength(36);
            
            entity.Property(e => e.Title)
                .IsRequired()
                .HasMaxLength(200);
                
            entity.Property(e => e.Status)
                .HasConversion<string>();
                
            entity.Property(e => e.Priority)
                .HasConversion<string>();
                
            // JSON serialization for Tags collection
            entity.Property(e => e.Tags)
                .HasConversion(
                    v => JsonSerializer.Serialize(v, (JsonSerializerOptions?)null),
                    v => JsonSerializer.Deserialize<List<string>>(v, (JsonSerializerOptions?)null) ?? new List<string>()
                );
        });
    }
}
```

The TasksController implements all REST endpoints with comprehensive error handling:

```csharp
[ApiController]
[Route("api/[controller]")]
public class TasksController : ControllerBase
{
    private readonly TaskDbContext _context;
    private readonly ILogger<TasksController> _logger;

    public TasksController(TaskDbContext context, ILogger<TasksController> logger)
    {
        _context = context;
        _logger = logger;
    }

    [HttpGet]
    public async Task<ActionResult<ListTasksResponse>> GetTasks(
        [FromQuery] TaskStatus? status,
        [FromQuery] string? assignedTo,
        [FromQuery] string? tags,
        [FromQuery] int pageSize = 20,
        [FromQuery] string? pageToken = null,
        [FromQuery] string sortBy = "created_at",
        [FromQuery] string sortOrder = "desc")
    {
        try
        {
            pageSize = Math.Min(pageSize, 100);
            var query = _context.Tasks.AsQueryable();

            // Apply filters
            if (status.HasValue)
                query = query.Where(t => t.Status == status.Value);

            if (!string.IsNullOrEmpty(assignedTo))
                query = query.Where(t => t.AssignedTo == assignedTo);

            if (!string.IsNullOrEmpty(tags))
            {
                var tagList = tags.Split(',', StringSplitOptions.RemoveEmptyEntries);
                query = query.Where(t => tagList.Any(tag => t.Tags.Contains(tag)));
            }

            // Apply sorting using switch expression
            query = sortBy.ToLower() switch
            {
                "title" => sortOrder.ToLower() == "asc" 
                    ? query.OrderBy(t => t.Title) 
                    : query.OrderByDescending(t => t.Title),
                "status" => sortOrder.ToLower() == "asc" 
                    ? query.OrderBy(t => t.Status) 
                    : query.OrderByDescending(t => t.Status),
                "priority" => sortOrder.ToLower() == "asc" 
                    ? query.OrderBy(t => t.Priority) 
                    : query.OrderByDescending(t => t.Priority),
                _ => sortOrder.ToLower() == "asc" 
                    ? query.OrderBy(t => t.CreatedAt) 
                    : query.OrderByDescending(t => t.CreatedAt)
            };

            // Apply pagination
            var skip = !string.IsNullOrEmpty(pageToken) && int.TryParse(pageToken, out var parsed) ? parsed : 0;
            
            var totalCount = await query.CountAsync();
            var tasks = await query.Skip(skip).Take(pageSize).ToListAsync();

            return Ok(new ListTasksResponse
            {
                Tasks = tasks,
                TotalCount = totalCount,
                NextPageToken = (skip + pageSize < totalCount) ? (skip + pageSize).ToString() : null
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error retrieving tasks");
            return StatusCode(500, new { message = "Internal server error" });
        }
    }

    [HttpPost]
    public async Task<ActionResult<Task>> CreateTask([FromBody] CreateTaskRequest request)
    {
        try
        {
            if (!ModelState.IsValid)
                return BadRequest(ModelState);

            var task = new Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = request.Title.Trim(),
                Description = request.Description?.Trim(),
                Priority = request.Priority,
                Tags = request.Tags ?? new List<string>(),
                AssignedTo = request.AssignedTo?.Trim(),
                CreatedBy = request.CreatedBy?.Trim() ?? "system",
                CreatedAt = DateTime.UtcNow,
                UpdatedAt = DateTime.UtcNow,
                DueDate = request.DueDate
            };

            _context.Tasks.Add(task);
            await _context.SaveChangesAsync();

            return CreatedAtAction(nameof(GetTask), new { id = task.Id }, task);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error creating task");
            return StatusCode(500, new { message = "Internal server error" });
        }
    }
}
```

The application startup configuration in Program.cs:

```csharp
var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddControllers()
    .AddJsonOptions(options =>
    {
        options.JsonSerializerOptions.Converters.Add(new JsonStringEnumConverter());
        options.JsonSerializerOptions.PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower;
    });

// Configure Entity Framework with SQLite
builder.Services.AddDbContext<TaskDbContext>(options =>
    options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection") ?? "Data Source=tasks.db"));

// Add CORS and API documentation
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll", policy =>
        policy.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader());
});

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseCors("AllowAll");
app.UseRouting();
app.MapControllers();

app.Run("http://localhost:8080");
```

### Client Implementation

The C# REST client uses HttpClient with System.Text.Json for serialization:

```csharp
public class TaskApiClient : IDisposable
{
    private readonly HttpClient _httpClient;
    private readonly JsonSerializerOptions _jsonOptions;

    public TaskApiClient(string baseUrl)
    {
        _httpClient = new HttpClient
        {
            BaseAddress = new Uri(baseUrl),
            Timeout = TimeSpan.FromSeconds(30)
        };
        
        _httpClient.DefaultRequestHeaders.Add("Accept", "application/json");
        
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower,
            WriteIndented = true
        };
        _jsonOptions.Converters.Add(new JsonStringEnumConverter());
    }

    public async Task<ListTasksResponse> ListTasksAsync(
        TaskStatus? status = null,
        string? assignedTo = null,
        List<string>? tags = null,
        int pageSize = 20,
        string? pageToken = null,
        CancellationToken cancellationToken = default)
    {
        var queryParams = new List<string> { $"page_size={pageSize}" };

        if (status.HasValue)
            queryParams.Add($"status={status.Value.ToString().ToLower()}");
        
        if (!string.IsNullOrEmpty(assignedTo))
            queryParams.Add($"assigned_to={Uri.EscapeDataString(assignedTo)}");

        if (tags != null && tags.Count > 0)
            queryParams.Add($"tags={Uri.EscapeDataString(string.Join(",", tags))}");

        if (!string.IsNullOrEmpty(pageToken))
            queryParams.Add($"page_token={Uri.EscapeDataString(pageToken)}");

        var query = string.Join("&", queryParams);
        var response = await _httpClient.GetAsync($"api/tasks?{query}", cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
        
        var json = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<ListTasksResponse>(json, _jsonOptions)
               ?? throw new InvalidOperationException("Failed to deserialize response");
    }

    public async Task<Models.Task> CreateTaskAsync(CreateTaskRequest request, CancellationToken cancellationToken = default)
    {
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        var json = JsonSerializer.Serialize(request, _jsonOptions);
        var content = new StringContent(json, Encoding.UTF8, "application/json");
        
        var response = await _httpClient.PostAsync("api/tasks", content, cancellationToken);
        await EnsureSuccessStatusCodeAsync(response);
        
        var responseJson = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<Models.Task>(responseJson, _jsonOptions)
               ?? throw new InvalidOperationException("Failed to deserialize created task");
    }

    private async Task EnsureSuccessStatusCodeAsync(HttpResponseMessage response)
    {
        if (response.IsSuccessStatusCode) return;

        var errorContent = await response.Content.ReadAsStringAsync();
        
        // Custom exception handling based on status code
        throw response.StatusCode switch
        {
            System.Net.HttpStatusCode.BadRequest => new ArgumentException($"Bad request: {errorContent}"),
            System.Net.HttpStatusCode.NotFound => new InvalidOperationException($"Resource not found: {errorContent}"),
            System.Net.HttpStatusCode.Unauthorized => new UnauthorizedAccessException($"Unauthorized: {errorContent}"),
            _ => new HttpRequestException($"Request failed ({response.StatusCode}): {errorContent}")
        };
    }

    public void Dispose() => _httpClient?.Dispose();
}
```

The client application uses System.CommandLine for CLI functionality:

```csharp
var baseUrlOption = new Option<string>(
    "--base-url",
    getDefaultValue: () => "http://localhost:8080",
    description: "The base URL of the Task API server");

var rootCommand = new RootCommand("C# Task REST API Client") { baseUrlOption };

// Create task command
var createCommand = new Command("create", "Create a new task");
var titleArgument = new Argument<string>("title", "The task title");
var descriptionOption = new Option<string?>("--description", "The task description");
var priorityOption = new Option<TaskPriority>("--priority", 
    getDefaultValue: () => TaskPriority.Medium, "The task priority");

createCommand.AddArgument(titleArgument);
createCommand.AddOption(descriptionOption);
createCommand.AddOption(priorityOption);

createCommand.SetHandler(async (baseUrl, title, description, priority) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        var request = new CreateTaskRequest
        {
            Title = title,
            Description = description,
            Priority = priority
        };
        
        var task = await client.CreateTaskAsync(request);
        Console.WriteLine("Task created successfully:");
        PrintTask(task);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, baseUrlOption, titleArgument, descriptionOption, priorityOption);
```

## gRPC Implementation with ASP.NET Core

ASP.NET Core provides first-class gRPC support with high performance and strong typing through Protocol Buffers.

### Server Implementation

The gRPC server uses the same Protocol Buffer definition and implements the service:

```csharp
// TaskGrpcServer.csproj
<Project Sdk="Microsoft.NET.Sdk.Web">
  <ItemGroup>
    <PackageReference Include="Grpc.AspNetCore" Version="2.57.0" />
    <PackageReference Include="Google.Protobuf" Version="3.25.1" />
    <PackageReference Include="Grpc.Tools" Version="2.59.0" PrivateAssets="All" />
  </ItemGroup>

  <ItemGroup>
    <Protobuf Include="../../../shared/protos/tasks.proto" GrpcServices="Server" />
  </ItemGroup>
</Project>
```

The TaskService implementation inherits from the generated base class:

```csharp
public class TaskService : Task.TaskService.TaskServiceBase
{
    private readonly ConcurrentDictionary<string, Task.Task> _tasks = new();
    private readonly ILogger<TaskService> _logger;

    public TaskService(ILogger<TaskService> logger)
    {
        _logger = logger;
        InitializeSampleData();
    }

    public override async Task<Task.Task> GetTask(Task.GetTaskRequest request, ServerCallContext context)
    {
        _logger.LogInformation("GetTask called with ID: {TaskId}", request.Id);

        if (string.IsNullOrEmpty(request.Id))
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Task ID is required"));

        if (!_tasks.TryGetValue(request.Id, out var task))
            throw new RpcException(new Status(StatusCode.NotFound, $"Task with ID '{request.Id}' not found"));

        return await System.Threading.Tasks.Task.FromResult(task);
    }

    public override async Task ListTasks(Task.ListTasksRequest request, 
                                          IServerStreamWriter<Task.Task> responseStream, 
                                          ServerCallContext context)
    {
        _logger.LogInformation("ListTasks called");

        var tasks = _tasks.Values.AsEnumerable();

        // Apply filters
        if (request.Status != Task.TaskStatus.Unspecified)
            tasks = tasks.Where(t => t.Status == request.Status);

        if (!string.IsNullOrEmpty(request.AssignedTo))
            tasks = tasks.Where(t => t.AssignedTo == request.AssignedTo);

        if (request.Tags.Count > 0)
            tasks = tasks.Where(t => request.Tags.Any(tag => t.Tags.Contains(tag)));

        // Apply sorting using switch expression
        tasks = request.SortOrder switch
        {
            Task.SortOrder.CreatedAsc => tasks.OrderBy(t => t.CreatedAt.ToDateTime()),
            Task.SortOrder.CreatedDesc => tasks.OrderByDescending(t => t.CreatedAt.ToDateTime()),
            Task.SortOrder.UpdatedAsc => tasks.OrderBy(t => t.UpdatedAt.ToDateTime()),
            Task.SortOrder.UpdatedDesc => tasks.OrderByDescending(t => t.UpdatedAt.ToDateTime()),
            Task.SortOrder.PriorityDesc => tasks.OrderByDescending(t => (int)t.Priority)
                                                .ThenBy(t => t.CreatedAt.ToDateTime()),
            _ => tasks.OrderByDescending(t => t.CreatedAt.ToDateTime())
        };

        // Apply pagination
        var pageSize = request.PageSize > 0 ? Math.Min((int)request.PageSize, 100) : 20;
        var skip = !string.IsNullOrEmpty(request.PageToken) && int.TryParse(request.PageToken, out var parsed) ? parsed : 0;

        foreach (var task in tasks.Skip(skip).Take(pageSize))
        {
            if (context.CancellationToken.IsCancellationRequested) break;
            await responseStream.WriteAsync(task);
        }
    }

    public override async Task<Task.Task> CreateTask(Task.CreateTaskRequest request, ServerCallContext context)
    {
        if (request.Task == null)
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Task is required"));

        var taskRequest = request.Task;

        // Validation
        if (string.IsNullOrWhiteSpace(taskRequest.Title))
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Title is required"));

        if (taskRequest.Title.Length > 200)
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Title must be 200 characters or less"));

        var now = Timestamp.FromDateTime(DateTime.UtcNow);
        var task = new Task.Task
        {
            Id = Guid.NewGuid().ToString(),
            Title = taskRequest.Title,
            Description = taskRequest.Description ?? "",
            Status = Task.TaskStatus.Pending,
            Priority = taskRequest.Priority != Task.TaskPriority.Unspecified ? taskRequest.Priority : Task.TaskPriority.Medium,
            AssignedTo = taskRequest.AssignedTo ?? "",
            CreatedBy = !string.IsNullOrEmpty(taskRequest.CreatedBy) ? taskRequest.CreatedBy : "system",
            CreatedAt = now,
            UpdatedAt = now,
            DueDate = taskRequest.DueDate
        };

        task.Tags.AddRange(taskRequest.Tags);
        _tasks[task.Id] = task;

        return await System.Threading.Tasks.Task.FromResult(task);
    }

    public override async Task WatchTasks(IAsyncStreamReader<Task.WatchTasksRequest> requestStream,
                                           IServerStreamWriter<Task.TaskEvent> responseStream,
                                           ServerCallContext context)
    {
        _logger.LogInformation("WatchTasks called");

        await foreach (var request in requestStream.ReadAllAsync())
        {
            if (context.CancellationToken.IsCancellationRequested) break;

            var now = Timestamp.FromDateTime(DateTime.UtcNow);
            var tasks = _tasks.Values.AsEnumerable();

            if (request.WatchAll)
            {
                foreach (var task in tasks)
                {
                    var taskEvent = new Task.TaskEvent
                    {
                        EventType = Task.EventType.Updated,
                        Task = task,
                        Timestamp = now
                    };
                    await responseStream.WriteAsync(taskEvent);
                }
            }
            // Handle other watch scenarios...
        }
    }
}
```

Program.cs configuration for the gRPC server:

```csharp
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddGrpc();
builder.Logging.AddConsole();

var app = builder.Build();

app.MapGrpcService<TaskService>();
app.MapGet("/", () => "Communication with gRPC endpoints must be made through a gRPC client.");

app.Run("http://localhost:50051");
```

### Client Implementation

The gRPC client uses Grpc.Net.Client with the generated client stub:

```csharp
// TaskGrpcClient.csproj
<ItemGroup>
  <PackageReference Include="Grpc.Net.Client" Version="2.57.0" />
  <PackageReference Include="Google.Protobuf" Version="3.25.1" />
  <PackageReference Include="Grpc.Tools" Version="2.59.0" PrivateAssets="All" />
  <PackageReference Include="System.CommandLine" Version="2.0.0-beta4.22272.1" />
</ItemGroup>

<ItemGroup>
  <Protobuf Include="../../../shared/protos/tasks.proto" GrpcServices="Client" />
</ItemGroup>
```

Client implementation with command-line interface:

```csharp
// Demo command implementation
var demoCommand = new Command("demo", "Run a demonstration of the gRPC API");

demoCommand.SetHandler(async (address) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        Console.WriteLine("C# Task gRPC Client Demo");
        Console.WriteLine("========================");

        // Create a new task
        var createRequest = new Task.CreateTaskRequest
        {
            Task = new Task.Task
            {
                Title = "Test C# gRPC Client",
                Description = "Testing the C# gRPC client implementation",
                Priority = Task.TaskPriority.High,
                AssignedTo = "dev-team",
                CreatedBy = "csharp-grpc-client"
            }
        };
        createRequest.Task.Tags.AddRange(new[] { "test", "csharp", "grpc" });

        var newTask = await client.CreateTaskAsync(createRequest);
        Console.WriteLine($"Created task: {newTask.Id} - {newTask.Title}");

        // Update task status
        var updateRequest = new Task.UpdateTaskRequest
        {
            Task = new Task.Task { Id = newTask.Id, Status = Task.TaskStatus.InProgress }
        };
        updateRequest.UpdateMask.Add("status");

        var updatedTask = await client.UpdateTaskAsync(updateRequest);
        Console.WriteLine($"Updated status: {updatedTask.Status}");

        // Watch for task changes
        using var watchCall = client.WatchTasks();
        var watchRequest = new Task.WatchTasksRequest { WatchAll = true };
        await watchCall.RequestStream.WriteAsync(watchRequest);
        await watchCall.RequestStream.CompleteAsync();

        await foreach (var taskEvent in watchCall.ResponseStream.ReadAllAsync())
        {
            Console.WriteLine($"Event: {taskEvent.EventType} - Task: {taskEvent.Task.Title}");
            break; // Just show one event for demo
        }

        Console.WriteLine("Demo completed successfully!");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Demo failed: {ex.Message}");
    }
}, addressOption);
```

## C#-Specific Features

### Nullable Reference Types

C# 8.0 introduced nullable reference types, providing compile-time null safety:

```csharp
// Enable nullable reference types
#nullable enable

public class TaskRequest
{
    public string Title { get; set; } = string.Empty;  // Non-null
    public string? Description { get; set; }           // Nullable
    
    public void ProcessTask()
    {
        // Compiler warns about potential null reference
        if (Description?.Length > 0)  // Safe null check
        {
            Console.WriteLine($"Description: {Description}");
        }
    }
}
```

### Pattern Matching

Modern C# pattern matching simplifies complex conditional logic:

```csharp
// Switch expressions with pattern matching
public static decimal GetPriorityMultiplier(TaskPriority priority, DateTime dueDate) =>
    (priority, DateTime.Now.CompareTo(dueDate)) switch
    {
        (TaskPriority.Critical, > 0) => 2.0m,        // Overdue critical
        (TaskPriority.Critical, _) => 1.5m,          // Critical
        (TaskPriority.High, > 0) => 1.3m,           // Overdue high
        (TaskPriority.High, _) => 1.2m,             // High priority
        (TaskPriority.Medium, > 0) => 1.1m,         // Overdue medium
        _ => 1.0m                                    // Normal
    };

// Property pattern matching
public static bool IsUrgentTask(Task task) => task switch
{
    { Priority: TaskPriority.Critical } => true,
    { Priority: TaskPriority.High, DueDate: var due } when due < DateTime.Now.AddDays(1) => true,
    _ => false
};
```

### LINQ and Functional Programming

LINQ provides powerful data querying capabilities:

```csharp
// Complex data queries with LINQ
public async Task<TaskSummaryReport> GenerateTaskSummaryAsync()
{
    var tasks = await _context.Tasks.ToListAsync();
    
    var summary = tasks
        .GroupBy(t => t.Status)
        .ToDictionary(
            g => g.Key,
            g => new StatusSummary
            {
                Count = g.Count(),
                HighPriorityCount = g.Count(t => t.Priority >= TaskPriority.High),
                AverageAge = g.Average(t => (DateTime.UtcNow - t.CreatedAt).TotalDays),
                TopAssignees = g.GroupBy(t => t.AssignedTo)
                               .OrderByDescending(ag => ag.Count())
                               .Take(3)
                               .Select(ag => new { Assignee = ag.Key, Count = ag.Count() })
                               .ToList()
            });
    
    return new TaskSummaryReport { StatusSummaries = summary };
}
```

### Async/Await and Cancellation

C#'s async/await model with cancellation support:

```csharp
public class TaskService
{
    public async Task<List<Task>> GetTasksBatchAsync(
        List<string> taskIds, 
        CancellationToken cancellationToken = default)
    {
        // Process tasks concurrently with cancellation support
        var tasks = taskIds.Select(async id =>
        {
            cancellationToken.ThrowIfCancellationRequested();
            return await GetTaskAsync(id, cancellationToken);
        });
        
        return (await System.Threading.Tasks.Task.WhenAll(tasks))
               .Where(task => task != null)
               .ToList()!;
    }
    
    // Timeout handling with CancellationTokenSource
    public async Task<Task?> GetTaskWithTimeoutAsync(string id, TimeSpan timeout)
    {
        using var cts = new CancellationTokenSource(timeout);
        
        try
        {
            return await GetTaskAsync(id, cts.Token);
        }
        catch (OperationCanceledException) when (cts.Token.IsCancellationRequested)
        {
            _logger.LogWarning("GetTask operation timed out for ID: {TaskId}", id);
            return null;
        }
    }
}
```

### Dependency Injection

ASP.NET Core's built-in dependency injection container:

```csharp
// Service registration
builder.Services.AddScoped<ITaskRepository, TaskRepository>();
builder.Services.AddScoped<ITaskService, TaskService>();
builder.Services.AddSingleton<ITaskCache, MemoryTaskCache>();
builder.Services.AddTransient<IEmailService, SmtpEmailService>();

// Configuration pattern
builder.Services.Configure<TaskApiOptions>(builder.Configuration.GetSection("TaskApi"));

// Factory pattern registration
builder.Services.AddSingleton<Func<string, ITaskNotificationService>>(provider =>
    (notificationType) => notificationType switch
    {
        "email" => provider.GetRequiredService<IEmailNotificationService>(),
        "sms" => provider.GetRequiredService<ISmsNotificationService>(),
        _ => provider.GetRequiredService<IDefaultNotificationService>()
    });

// Usage in controller
[ApiController]
public class TasksController : ControllerBase
{
    private readonly ITaskService _taskService;
    private readonly ILogger<TasksController> _logger;
    private readonly TaskApiOptions _options;

    public TasksController(
        ITaskService taskService,
        ILogger<TasksController> logger,
        IOptions<TaskApiOptions> options)
    {
        _taskService = taskService;
        _logger = logger;
        _options = options.Value;
    }
}
```

## Testing

C# has excellent testing support with multiple frameworks:

### Unit Testing with xUnit

```csharp
public class TaskServiceTests
{
    private readonly ITestOutputHelper _output;
    private readonly TaskService _taskService;
    private readonly Mock<ITaskRepository> _mockRepository;

    public TaskServiceTests(ITestOutputHelper output)
    {
        _output = output;
        _mockRepository = new Mock<ITaskRepository>();
        _taskService = new TaskService(_mockRepository.Object);
    }

    [Fact]
    public async Task CreateTask_ValidInput_ReturnsTask()
    {
        // Arrange
        var request = new CreateTaskRequest
        {
            Title = "Test Task",
            Priority = TaskPriority.High
        };

        _mockRepository
            .Setup(r => r.CreateAsync(It.IsAny<Task>()))
            .ReturnsAsync((Task t) => t);

        // Act
        var result = await _taskService.CreateTaskAsync(request);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("Test Task", result.Title);
        Assert.Equal(TaskPriority.High, result.Priority);
        
        _mockRepository.Verify(r => r.CreateAsync(It.IsAny<Task>()), Times.Once);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(null)]
    public async Task CreateTask_InvalidTitle_ThrowsArgumentException(string? title)
    {
        // Arrange
        var request = new CreateTaskRequest { Title = title! };

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(
            () => _taskService.CreateTaskAsync(request));
    }
}
```

### Integration Testing

```csharp
public class TasksControllerIntegrationTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly WebApplicationFactory<Program> _factory;
    private readonly HttpClient _client;

    public TasksControllerIntegrationTests(WebApplicationFactory<Program> factory)
    {
        _factory = factory;
        _client = _factory.CreateClient();
    }

    [Fact]
    public async Task GetTasks_ReturnsTaskList()
    {
        // Act
        var response = await _client.GetAsync("/api/tasks");

        // Assert
        response.EnsureSuccessStatusCode();
        
        var content = await response.Content.ReadAsStringAsync();
        var tasks = JsonSerializer.Deserialize<ListTasksResponse>(content, new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower
        });

        Assert.NotNull(tasks);
        Assert.True(tasks.Tasks.Count >= 0);
    }

    [Fact]
    public async Task CreateTask_ValidData_ReturnsCreated()
    {
        // Arrange
        var newTask = new CreateTaskRequest
        {
            Title = "Integration Test Task",
            Description = "Created by integration test",
            Priority = TaskPriority.Medium
        };

        var json = JsonSerializer.Serialize(newTask, new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower
        });
        
        var content = new StringContent(json, Encoding.UTF8, "application/json");

        // Act
        var response = await _client.PostAsync("/api/tasks", content);

        // Assert
        Assert.Equal(HttpStatusCode.Created, response.StatusCode);
        
        var responseContent = await response.Content.ReadAsStringAsync();
        var createdTask = JsonSerializer.Deserialize<Task>(responseContent, new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower
        });

        Assert.NotNull(createdTask);
        Assert.Equal("Integration Test Task", createdTask.Title);
    }
}
```

## Performance Considerations

### Memory Management

C#'s garbage collector handles memory management, but understanding its behavior is crucial:

```csharp
// Dispose pattern implementation
public class TaskService : ITaskService, IDisposable
{
    private readonly HttpClient _httpClient;
    private readonly Timer _cleanupTimer;
    private bool _disposed;

    public TaskService(HttpClient httpClient)
    {
        _httpClient = httpClient;
        _cleanupTimer = new Timer(CleanupCallback, null, TimeSpan.Zero, TimeSpan.FromMinutes(5));
    }

    protected virtual void Dispose(bool disposing)
    {
        if (!_disposed)
        {
            if (disposing)
            {
                _httpClient?.Dispose();
                _cleanupTimer?.Dispose();
            }
            _disposed = true;
        }
    }

    public void Dispose()
    {
        Dispose(true);
        GC.SuppressFinalize(this);
    }
}

// Using statements for automatic disposal
public async Task<List<Task>> ProcessTasksAsync(List<string> taskIds)
{
    using var httpClient = new HttpClient();
    using var response = await httpClient.GetAsync("api/tasks");
    using var stream = await response.Content.ReadAsStreamAsync();
    
    return await JsonSerializer.DeserializeAsync<List<Task>>(stream);
}
```

### Async Best Practices

```csharp
// Avoid async void (except for event handlers)
public async Task ProcessTaskAsync(string taskId)  // Good
{
    await DoWorkAsync(taskId);
}

// Configure await behavior
public async Task<Task> GetTaskOptimizedAsync(string id)
{
    var task = await _repository.GetAsync(id).ConfigureAwait(false);  // Avoid capturing context
    return task;
}

// Parallel processing
public async Task<List<TaskResult>> ProcessTasksInParallelAsync(List<string> taskIds)
{
    var tasks = taskIds.Select(async id =>
    {
        return await ProcessTaskAsync(id);
    });
    
    return (await System.Threading.Tasks.Task.WhenAll(tasks)).ToList();
}
```

### Entity Framework Optimization

```csharp
public class OptimizedTaskRepository : ITaskRepository
{
    private readonly TaskDbContext _context;

    public async Task<List<Task>> GetTasksOptimizedAsync(TaskFilters filters)
    {
        var query = _context.Tasks.AsNoTracking();  // Read-only queries

        // Avoid N+1 queries with explicit loading
        if (filters.IncludeComments)
        {
            query = query.Include(t => t.Comments);
        }

        // Use compiled queries for frequently executed queries
        return await GetTasksByStatusCompiled(_context, filters.Status);
    }

    private static readonly Func<TaskDbContext, TaskStatus, Task<List<Task>>> 
        GetTasksByStatusCompiled = EF.CompileAsyncQuery(
            (TaskDbContext ctx, TaskStatus status) => 
                ctx.Tasks.Where(t => t.Status == status).ToList());

    // Bulk operations for better performance
    public async Task UpdateTasksStatusAsync(List<string> taskIds, TaskStatus newStatus)
    {
        await _context.Tasks
            .Where(t => taskIds.Contains(t.Id))
            .ExecuteUpdateAsync(t => t.SetProperty(p => p.Status, newStatus));
    }
}
```

## Production Deployment

### Docker Deployment

```dockerfile
# Dockerfile
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS base
WORKDIR /app
EXPOSE 8080
EXPOSE 50051

FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src
COPY ["TaskApi.csproj", "."]
RUN dotnet restore "./TaskApi.csproj"
COPY . .
WORKDIR "/src/."
RUN dotnet build "TaskApi.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "TaskApi.csproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "TaskApi.dll"]
```

### Configuration Management

```csharp
// appsettings.json hierarchy
{
  "ConnectionStrings": {
    "DefaultConnection": "Server=localhost;Database=TasksDb;Trusted_Connection=true;"
  },
  "TaskApi": {
    "MaxPageSize": 100,
    "DefaultPageSize": 20,
    "EnableSwagger": true,
    "CacheExpirationMinutes": 30
  },
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft.AspNetCore": "Warning"
    }
  }
}

// Configuration binding
public class TaskApiOptions
{
    public int MaxPageSize { get; set; } = 100;
    public int DefaultPageSize { get; set; } = 20;
    public bool EnableSwagger { get; set; }
    public int CacheExpirationMinutes { get; set; } = 30;
}

// Environment-specific configuration
public static void Main(string[] args)
{
    var builder = WebApplication.CreateBuilder(args);
    
    // Add configuration sources
    builder.Configuration
        .AddJsonFile("appsettings.json", optional: false)
        .AddJsonFile($"appsettings.{builder.Environment.EnvironmentName}.json", optional: true)
        .AddEnvironmentVariables()
        .AddCommandLine(args);
    
    // Configure services based on environment
    if (builder.Environment.IsDevelopment())
    {
        builder.Services.AddDbContext<TaskDbContext>(options =>
            options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection")));
    }
    else
    {
        builder.Services.AddDbContext<TaskDbContext>(options =>
            options.UseSqlServer(builder.Configuration.GetConnectionString("ProductionConnection")));
    }
}
```

### Health Checks and Monitoring

```csharp
// Health checks configuration
builder.Services.AddHealthChecks()
    .AddCheck("self", () => HealthCheckResult.Healthy())
    .AddDbContextCheck<TaskDbContext>()
    .AddUrlGroup(new Uri("http://external-service/health"), name: "external-service")
    .AddCheck<TaskServiceHealthCheck>("task-service");

// Custom health check
public class TaskServiceHealthCheck : IHealthCheck
{
    private readonly ITaskRepository _repository;

    public TaskServiceHealthCheck(ITaskRepository repository)
    {
        _repository = repository;
    }

    public async Task<HealthCheckResult> CheckHealthAsync(
        HealthCheckContext context,
        CancellationToken cancellationToken = default)
    {
        try
        {
            await _repository.GetHealthCheckAsync(cancellationToken);
            return HealthCheckResult.Healthy("Task service is healthy");
        }
        catch (Exception ex)
        {
            return HealthCheckResult.Unhealthy("Task service is unhealthy", ex);
        }
    }
}

// Middleware pipeline
app.UseHealthChecks("/health", new HealthCheckOptions
{
    ResponseWriter = UIResponseWriter.WriteHealthCheckUIResponse
});

// Structured logging
builder.Services.AddLogging(logging =>
{
    logging.AddConsole();
    logging.AddFile("logs/taskapi-{Date}.txt");
    logging.AddApplicationInsights();  // For Azure Application Insights
});

// Usage in controller
[HttpGet]
public async Task<ActionResult<ListTasksResponse>> GetTasks(...)
{
    using var activity = TaskApiTelemetry.ActivitySource.StartActivity("GetTasks");
    activity?.SetTag("page_size", pageSize.ToString());
    
    _logger.LogInformation("Getting tasks with page size {PageSize}", pageSize);
    
    var stopwatch = Stopwatch.StartNew();
    try
    {
        var result = await _taskService.GetTasksAsync(...);
        _logger.LogInformation("Retrieved {TaskCount} tasks in {ElapsedMs}ms", 
                              result.Tasks.Count, stopwatch.ElapsedMilliseconds);
        return Ok(result);
    }
    catch (Exception ex)
    {
        _logger.LogError(ex, "Error retrieving tasks");
        throw;
    }
}
```

## C# Ecosystem and Tools

### Package Management with NuGet

```xml
<!-- Central Package Management (Directory.Packages.props) -->
<Project>
  <PropertyGroup>
    <ManagePackageVersionsCentrally>true</ManagePackageVersionsCentrally>
  </PropertyGroup>
  
  <ItemGroup>
    <PackageVersion Include="Microsoft.AspNetCore.OpenApi" Version="8.0.0" />
    <PackageVersion Include="Microsoft.EntityFrameworkCore.Sqlite" Version="8.0.0" />
    <PackageVersion Include="Swashbuckle.AspNetCore" Version="6.4.0" />
  </ItemGroup>
</Project>

<!-- Project file references the centrally managed versions -->
<Project Sdk="Microsoft.NET.Sdk.Web">
  <ItemGroup>
    <PackageReference Include="Microsoft.AspNetCore.OpenApi" />
    <PackageReference Include="Microsoft.EntityFrameworkCore.Sqlite" />
    <PackageReference Include="Swashbuckle.AspNetCore" />
  </ItemGroup>
</Project>
```

### Build and CI/CD

```yaml
# GitHub Actions workflow
name: .NET Build and Test

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Setup .NET
      uses: actions/setup-dotnet@v3
      with:
        dotnet-version: 8.0.x
        
    - name: Restore dependencies
      run: dotnet restore
      
    - name: Build
      run: dotnet build --no-restore --configuration Release
      
    - name: Test
      run: dotnet test --no-build --configuration Release --verbosity normal --collect:"XPlat Code Coverage"
      
    - name: Upload coverage reports
      uses: codecov/codecov-action@v3
      
    - name: Publish
      run: dotnet publish --no-build --configuration Release --output ./publish
      
    - name: Build Docker image
      run: docker build -t taskapi:${{ github.sha }} .
```

### Development Tools

```bash
# Essential .NET CLI commands
dotnet new webapi -n TaskApi                    # Create new Web API project
dotnet add package Microsoft.EntityFrameworkCore.Sqlite  # Add NuGet package
dotnet ef migrations add InitialCreate          # Entity Framework migrations
dotnet ef database update                       # Apply database migrations
dotnet run --launch-profile Development         # Run with specific profile
dotnet test --logger trx --results-directory ./TestResults  # Run tests with results
dotnet publish -c Release -r linux-x64 --self-contained  # Publish for Linux
```

## Best Practices

### API Design

```csharp
// Consistent error responses
[ApiController]
public class TasksController : ControllerBase
{
    [HttpGet]
    [ProducesResponseType(typeof(ListTasksResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ListTasksResponse>> GetTasks(...)
    {
        // Implementation
    }
}

// Global exception handling
public class GlobalExceptionMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<GlobalExceptionMiddleware> _logger;

    public async Task InvokeAsync(HttpContext context)
    {
        try
        {
            await _next(context);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "An unhandled exception occurred");
            await HandleExceptionAsync(context, ex);
        }
    }

    private static async Task HandleExceptionAsync(HttpContext context, Exception exception)
    {
        var response = exception switch
        {
            ValidationException => (StatusCodes.Status400BadRequest, "Validation failed"),
            NotFoundException => (StatusCodes.Status404NotFound, "Resource not found"),
            _ => (StatusCodes.Status500InternalServerError, "Internal server error")
        };

        context.Response.StatusCode = response.Item1;
        await context.Response.WriteAsJsonAsync(new { message = response.Item2 });
    }
}
```

### Security

```csharp
// JWT Authentication
builder.Services.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
    .AddJwtBearer(options =>
    {
        options.TokenValidationParameters = new TokenValidationParameters
        {
            ValidateIssuer = true,
            ValidateAudience = true,
            ValidateLifetime = true,
            ValidateIssuerSigningKey = true,
            ValidIssuer = builder.Configuration["Jwt:Issuer"],
            ValidAudience = builder.Configuration["Jwt:Audience"],
            IssuerSigningKey = new SymmetricSecurityKey(
                Encoding.UTF8.GetBytes(builder.Configuration["Jwt:Key"]!))
        };
    });

// Authorization policies
builder.Services.AddAuthorization(options =>
{
    options.AddPolicy("RequireAdminRole", policy =>
        policy.RequireRole("Admin"));
    options.AddPolicy("RequireTaskOwner", policy =>
        policy.Requirements.Add(new TaskOwnerRequirement()));
});

// Rate limiting (available in .NET 7+)
builder.Services.AddRateLimiter(options =>
{
    options.RejectionStatusCode = StatusCodes.Status429TooManyRequests;
    options.AddFixedWindowLimiter("api", limiterOptions =>
    {
        limiterOptions.PermitLimit = 100;
        limiterOptions.Window = TimeSpan.FromMinutes(1);
    });
});

app.UseRateLimiter();
```

## Conclusion

C# provides a mature, feature-rich environment for API development with strong typing, excellent tooling, and comprehensive framework support. The combination of ASP.NET Core for REST APIs and first-class gRPC support makes it an excellent choice for building production-ready services.

Key advantages of C# for API development:

- **Strong Type System**: Compile-time error detection and IntelliSense support
- **Rich Framework**: ASP.NET Core provides comprehensive features out of the box
- **Performance**: Excellent runtime performance with JIT and AOT compilation options
- **Ecosystem**: Vast NuGet package ecosystem and enterprise tooling
- **Cross-Platform**: Runs on Windows, macOS, and Linux
- **Modern Language Features**: Pattern matching, nullable reference types, and async/await

C#'s enterprise focus, combined with Microsoft's continued investment in the .NET platform, makes it a solid choice for organizations building scalable, maintainable API services. The language's evolution continues to embrace modern development practices while maintaining backward compatibility and performance.

Next, we'll explore **Chapter 14: F#**, examining how this functional-first language approaches API development with its unique perspective on immutability and type safety.