using Grpc.Net.Client;
using System.CommandLine;
using Google.Protobuf.WellKnownTypes;
using Task = TaskNamespace;

var addressOption = new Option<string>(
    "--address",
    getDefaultValue: () => "http://localhost:50051",
    description: "The address of the gRPC server");

var rootCommand = new RootCommand("C# Task gRPC Client")
{
    addressOption
};

// List tasks command
var listCommand = new Command("list", "List all tasks with optional filtering");
var statusOption = new Option<Task.TaskStatus>("--status", getDefaultValue: () => Task.TaskStatus.Unspecified, "Filter by task status");
var assignedToOption = new Option<string?>("--assigned-to", "Filter by assignee");
var tagsOption = new Option<string[]?>("--tags", "Filter by tags") { AllowMultipleArgumentsPerToken = true };
var pageSizeOption = new Option<int>("--page-size", getDefaultValue: () => 20, "Number of tasks per page");
var pageTokenOption = new Option<string?>("--page-token", "Page token for pagination");

listCommand.AddOption(statusOption);
listCommand.AddOption(assignedToOption);
listCommand.AddOption(tagsOption);
listCommand.AddOption(pageSizeOption);
listCommand.AddOption(pageTokenOption);

listCommand.SetHandler(async (address, status, assignedTo, tags, pageSize, pageToken) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        var request = new Task.ListTasksRequest
        {
            Status = status,
            PageSize = pageSize,
            PageToken = pageToken ?? "",
            AssignedTo = assignedTo ?? "",
            SortOrder = Task.SortOrder.CreatedDesc
        };

        if (tags != null)
        {
            request.Tags.AddRange(tags);
        }

        Console.WriteLine("Listing tasks...");

        var call = client.ListTasks(request);
        await foreach (var task in call.ResponseStream.ReadAllAsync())
        {
            PrintTask(task);
            Console.WriteLine();
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption, statusOption, assignedToOption, tagsOption, pageSizeOption, pageTokenOption);

// Get task command
var getCommand = new Command("get", "Get a specific task by ID");
var idArgument = new Argument<string>("id", "The task ID");
getCommand.AddArgument(idArgument);

getCommand.SetHandler(async (address, id) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        var request = new Task.GetTaskRequest { Id = id };
        var task = await client.GetTaskAsync(request);
        PrintTask(task);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption, idArgument);

// Create task command
var createCommand = new Command("create", "Create a new task");
var titleArgument = new Argument<string>("title", "The task title");
var descriptionOption = new Option<string?>("--description", "The task description");
var priorityOption = new Option<Task.TaskPriority>("--priority", getDefaultValue: () => Task.TaskPriority.Medium, "The task priority");
var createTagsOption = new Option<string[]?>("--tags", "Task tags") { AllowMultipleArgumentsPerToken = true };
var createAssignedToOption = new Option<string?>("--assigned-to", "Assignee");
var createdByOption = new Option<string?>("--created-by", "Creator");

createCommand.AddArgument(titleArgument);
createCommand.AddOption(descriptionOption);
createCommand.AddOption(priorityOption);
createCommand.AddOption(createTagsOption);
createCommand.AddOption(createAssignedToOption);
createCommand.AddOption(createdByOption);

createCommand.SetHandler(async (address, title, description, priority, tags, assignedTo, createdBy) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        var task = new Task.Task
        {
            Title = title,
            Description = description ?? "",
            Priority = priority,
            AssignedTo = assignedTo ?? "",
            CreatedBy = createdBy ?? "grpc-client"
        };

        if (tags != null)
        {
            task.Tags.AddRange(tags);
        }

        var request = new Task.CreateTaskRequest { Task = task };
        var createdTask = await client.CreateTaskAsync(request);

        Console.WriteLine("Task created successfully:");
        PrintTask(createdTask);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption, titleArgument, descriptionOption, priorityOption, createTagsOption, createAssignedToOption, createdByOption);

// Update task command
var updateCommand = new Command("update", "Update an existing task");
var updateIdArgument = new Argument<string>("id", "The task ID");
var updateTitleOption = new Option<string?>("--title", "New title");
var updateDescriptionOption = new Option<string?>("--description", "New description");
var updateStatusOption = new Option<Task.TaskStatus?>("--status", "New status");
var updatePriorityOption = new Option<Task.TaskPriority?>("--priority", "New priority");
var updateTagsOption = new Option<string[]?>("--tags", "New tags") { AllowMultipleArgumentsPerToken = true };
var updateAssignedToOption = new Option<string?>("--assigned-to", "New assignee");

updateCommand.AddArgument(updateIdArgument);
updateCommand.AddOption(updateTitleOption);
updateCommand.AddOption(updateDescriptionOption);
updateCommand.AddOption(updateStatusOption);
updateCommand.AddOption(updatePriorityOption);
updateCommand.AddOption(updateTagsOption);
updateCommand.AddOption(updateAssignedToOption);

updateCommand.SetHandler(async (address, id, title, description, status, priority, tags, assignedTo) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        var updateTask = new Task.Task { Id = id };
        var updateMask = new List<string>();

        if (title != null)
        {
            updateTask.Title = title;
            updateMask.Add("title");
        }

        if (description != null)
        {
            updateTask.Description = description;
            updateMask.Add("description");
        }

        if (status.HasValue)
        {
            updateTask.Status = status.Value;
            updateMask.Add("status");
        }

        if (priority.HasValue)
        {
            updateTask.Priority = priority.Value;
            updateMask.Add("priority");
        }

        if (tags != null)
        {
            updateTask.Tags.AddRange(tags);
            updateMask.Add("tags");
        }

        if (assignedTo != null)
        {
            updateTask.AssignedTo = assignedTo;
            updateMask.Add("assigned_to");
        }

        var request = new Task.UpdateTaskRequest 
        { 
            Task = updateTask
        };
        request.UpdateMask.AddRange(updateMask);

        var updatedTask = await client.UpdateTaskAsync(request);

        Console.WriteLine("Task updated successfully:");
        PrintTask(updatedTask);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption, updateIdArgument, updateTitleOption, updateDescriptionOption, updateStatusOption, updatePriorityOption, updateTagsOption, updateAssignedToOption);

// Delete task command
var deleteCommand = new Command("delete", "Delete a task");
var deleteIdArgument = new Argument<string>("id", "The task ID");
deleteCommand.AddArgument(deleteIdArgument);

deleteCommand.SetHandler(async (address, id) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        var request = new Task.DeleteTaskRequest { Id = id };
        await client.DeleteTaskAsync(request);
        Console.WriteLine($"Task {id} deleted successfully.");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption, deleteIdArgument);

// Watch tasks command
var watchCommand = new Command("watch", "Watch for task changes");
var watchAllOption = new Option<bool>("--all", "Watch all tasks");
var watchTaskIdsOption = new Option<string[]?>("--task-ids", "Specific task IDs to watch") { AllowMultipleArgumentsPerToken = true };
var watchAssignedToOption = new Option<string?>("--assigned-to", "Watch tasks assigned to user");

watchCommand.AddOption(watchAllOption);
watchCommand.AddOption(watchTaskIdsOption);
watchCommand.AddOption(watchAssignedToOption);

watchCommand.SetHandler(async (address, watchAll, taskIds, assignedTo) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        using var call = client.WatchTasks();

        var request = new Task.WatchTasksRequest
        {
            WatchAll = watchAll,
            AssignedTo = assignedTo ?? ""
        };

        if (taskIds != null)
        {
            request.TaskIds.AddRange(taskIds);
        }

        await call.RequestStream.WriteAsync(request);
        await call.RequestStream.CompleteAsync();

        Console.WriteLine("Watching for task events...");

        await foreach (var taskEvent in call.ResponseStream.ReadAllAsync())
        {
            Console.WriteLine($"Event: {taskEvent.EventType} at {taskEvent.Timestamp.ToDateTime():yyyy-MM-dd HH:mm:ss}");
            PrintTask(taskEvent.Task);
            Console.WriteLine();
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption, watchAllOption, watchTaskIdsOption, watchAssignedToOption);

// Demo command
var demoCommand = new Command("demo", "Run a demonstration of the gRPC API");

demoCommand.SetHandler(async (address) =>
{
    using var channel = GrpcChannel.ForAddress(address);
    var client = new Task.TaskService.TaskServiceClient(channel);

    try
    {
        Console.WriteLine("C# Task gRPC Client Demo");
        Console.WriteLine("========================");
        Console.WriteLine();

        // List initial tasks
        Console.WriteLine("1. Listing initial tasks...");
        var listRequest = new Task.ListTasksRequest
        {
            PageSize = 10,
            SortOrder = Task.SortOrder.CreatedDesc
        };

        var listCall = client.ListTasks(listRequest);
        var taskCount = 0;
        await foreach (var task in listCall.ResponseStream.ReadAllAsync())
        {
            taskCount++;
        }
        Console.WriteLine($"Found {taskCount} existing tasks");
        Console.WriteLine();

        // Create a new task
        Console.WriteLine("2. Creating a new task...");
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
        Console.WriteLine();

        // Get the created task
        Console.WriteLine("3. Retrieving the created task...");
        var getRequest = new Task.GetTaskRequest { Id = newTask.Id };
        var retrievedTask = await client.GetTaskAsync(getRequest);
        Console.WriteLine($"Retrieved: {retrievedTask.Title} (Status: {retrievedTask.Status})");
        Console.WriteLine();

        // Update task status
        Console.WriteLine("4. Updating task status to InProgress...");
        var updateRequest = new Task.UpdateTaskRequest
        {
            Task = new Task.Task { Id = newTask.Id, Status = Task.TaskStatus.InProgress }
        };
        updateRequest.UpdateMask.Add("status");

        var updatedTask = await client.UpdateTaskAsync(updateRequest);
        Console.WriteLine($"Updated status: {updatedTask.Status}");
        Console.WriteLine();

        // Watch tasks
        Console.WriteLine("5. Watching for task changes...");
        using var watchCall = client.WatchTasks();

        var watchRequest = new Task.WatchTasksRequest { WatchAll = true };
        await watchCall.RequestStream.WriteAsync(watchRequest);
        await watchCall.RequestStream.CompleteAsync();

        var eventCount = 0;
        await foreach (var taskEvent in watchCall.ResponseStream.ReadAllAsync())
        {
            Console.WriteLine($"Event: {taskEvent.EventType} - Task: {taskEvent.Task.Title}");
            eventCount++;
            if (eventCount >= 3) break; // Limit events for demo
        }
        Console.WriteLine();

        // Delete the task
        Console.WriteLine("6. Deleting the test task...");
        var deleteRequest = new Task.DeleteTaskRequest { Id = newTask.Id };
        await client.DeleteTaskAsync(deleteRequest);
        Console.WriteLine("Task deleted successfully");
        Console.WriteLine();

        Console.WriteLine("Demo completed successfully!");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Demo failed: {ex.Message}");
        Environment.Exit(1);
    }
}, addressOption);

rootCommand.AddCommand(listCommand);
rootCommand.AddCommand(getCommand);
rootCommand.AddCommand(createCommand);
rootCommand.AddCommand(updateCommand);
rootCommand.AddCommand(deleteCommand);
rootCommand.AddCommand(watchCommand);
rootCommand.AddCommand(demoCommand);

return await rootCommand.InvokeAsync(args);

static void PrintTask(Task.Task task)
{
    Console.WriteLine($"ID: {task.Id}");
    Console.WriteLine($"Title: {task.Title}");
    if (!string.IsNullOrEmpty(task.Description))
        Console.WriteLine($"Description: {task.Description}");
    Console.WriteLine($"Status: {task.Status}");
    Console.WriteLine($"Priority: {task.Priority}");
    if (task.Tags.Count > 0)
        Console.WriteLine($"Tags: {string.Join(", ", task.Tags)}");
    if (!string.IsNullOrEmpty(task.AssignedTo))
        Console.WriteLine($"Assigned To: {task.AssignedTo}");
    Console.WriteLine($"Created By: {task.CreatedBy}");
    Console.WriteLine($"Created: {task.CreatedAt.ToDateTime():yyyy-MM-dd HH:mm:ss}");
    Console.WriteLine($"Updated: {task.UpdatedAt.ToDateTime():yyyy-MM-dd HH:mm:ss}");
    if (task.DueDate != null)
        Console.WriteLine($"Due Date: {task.DueDate.ToDateTime():yyyy-MM-dd HH:mm:ss}");
}