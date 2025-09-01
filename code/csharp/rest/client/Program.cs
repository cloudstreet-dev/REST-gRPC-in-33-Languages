using System.CommandLine;
using TaskRestClient;
using TaskRestClient.Models;

var baseUrlOption = new Option<string>(
    "--base-url",
    getDefaultValue: () => "http://localhost:8080",
    description: "The base URL of the Task API server");

var rootCommand = new RootCommand("C# Task REST API Client")
{
    baseUrlOption
};

// List tasks command
var listCommand = new Command("list", "List all tasks with optional filtering");
var statusOption = new Option<TaskStatus?>("--status", "Filter by task status");
var assignedToOption = new Option<string?>("--assigned-to", "Filter by assignee");
var tagsOption = new Option<string[]?>("--tags", "Filter by tags (comma-separated)") { AllowMultipleArgumentsPerToken = true };
var pageSizeOption = new Option<int>("--page-size", getDefaultValue: () => 20, "Number of tasks per page");
var pageTokenOption = new Option<string?>("--page-token", "Page token for pagination");

listCommand.AddOption(statusOption);
listCommand.AddOption(assignedToOption);
listCommand.AddOption(tagsOption);
listCommand.AddOption(pageSizeOption);
listCommand.AddOption(pageTokenOption);

listCommand.SetHandler(async (baseUrl, status, assignedTo, tags, pageSize, pageToken) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        var tagsList = tags?.ToList();
        var response = await client.ListTasksAsync(status, assignedTo, tagsList, pageSize, pageToken);
        
        Console.WriteLine($"Found {response.TotalCount} tasks:");
        Console.WriteLine();
        
        foreach (var task in response.Tasks)
        {
            PrintTask(task);
            Console.WriteLine();
        }
        
        if (!string.IsNullOrEmpty(response.NextPageToken))
        {
            Console.WriteLine($"Next page token: {response.NextPageToken}");
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, baseUrlOption, statusOption, assignedToOption, tagsOption, pageSizeOption, pageTokenOption);

// Get task command
var getCommand = new Command("get", "Get a specific task by ID");
var idArgument = new Argument<string>("id", "The task ID");
getCommand.AddArgument(idArgument);

getCommand.SetHandler(async (baseUrl, id) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        var task = await client.GetTaskAsync(id);
        PrintTask(task);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, baseUrlOption, idArgument);

// Create task command
var createCommand = new Command("create", "Create a new task");
var titleArgument = new Argument<string>("title", "The task title");
var descriptionOption = new Option<string?>("--description", "The task description");
var priorityOption = new Option<TaskPriority>("--priority", getDefaultValue: () => TaskPriority.Medium, "The task priority");
var createTagsOption = new Option<string[]?>("--tags", "Task tags") { AllowMultipleArgumentsPerToken = true };
var createAssignedToOption = new Option<string?>("--assigned-to", "Assignee");
var createdByOption = new Option<string?>("--created-by", "Creator");

createCommand.AddArgument(titleArgument);
createCommand.AddOption(descriptionOption);
createCommand.AddOption(priorityOption);
createCommand.AddOption(createTagsOption);
createCommand.AddOption(createAssignedToOption);
createCommand.AddOption(createdByOption);

createCommand.SetHandler(async (baseUrl, title, description, priority, tags, assignedTo, createdBy) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        var request = new CreateTaskRequest
        {
            Title = title,
            Description = description,
            Priority = priority,
            Tags = tags?.ToList(),
            AssignedTo = assignedTo,
            CreatedBy = createdBy
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
}, baseUrlOption, titleArgument, descriptionOption, priorityOption, createTagsOption, createAssignedToOption, createdByOption);

// Update task command
var updateCommand = new Command("update", "Update an existing task");
var updateIdArgument = new Argument<string>("id", "The task ID");
var updateTitleOption = new Option<string?>("--title", "New title");
var updateDescriptionOption = new Option<string?>("--description", "New description");
var updateStatusOption = new Option<TaskStatus?>("--status", "New status");
var updatePriorityOption = new Option<TaskPriority?>("--priority", "New priority");
var updateTagsOption = new Option<string[]?>("--tags", "New tags") { AllowMultipleArgumentsPerToken = true };
var updateAssignedToOption = new Option<string?>("--assigned-to", "New assignee");

updateCommand.AddArgument(updateIdArgument);
updateCommand.AddOption(updateTitleOption);
updateCommand.AddOption(updateDescriptionOption);
updateCommand.AddOption(updateStatusOption);
updateCommand.AddOption(updatePriorityOption);
updateCommand.AddOption(updateTagsOption);
updateCommand.AddOption(updateAssignedToOption);

updateCommand.SetHandler(async (baseUrl, id, title, description, status, priority, tags, assignedTo) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        var request = new UpdateTaskRequest
        {
            Title = title,
            Description = description,
            Status = status,
            Priority = priority,
            Tags = tags?.ToList(),
            AssignedTo = assignedTo
        };
        
        var task = await client.UpdateTaskAsync(id, request);
        Console.WriteLine("Task updated successfully:");
        PrintTask(task);
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, baseUrlOption, updateIdArgument, updateTitleOption, updateDescriptionOption, updateStatusOption, updatePriorityOption, updateTagsOption, updateAssignedToOption);

// Delete task command
var deleteCommand = new Command("delete", "Delete a task");
var deleteIdArgument = new Argument<string>("id", "The task ID");
deleteCommand.AddArgument(deleteIdArgument);

deleteCommand.SetHandler(async (baseUrl, id) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        await client.DeleteTaskAsync(id);
        Console.WriteLine($"Task {id} deleted successfully.");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Error: {ex.Message}");
        Environment.Exit(1);
    }
}, baseUrlOption, deleteIdArgument);

// Demo command
var demoCommand = new Command("demo", "Run a demonstration of the API");

demoCommand.SetHandler(async (baseUrl) =>
{
    using var client = new TaskApiClient(baseUrl);
    
    try
    {
        Console.WriteLine("C# Task REST Client Demo");
        Console.WriteLine("========================");
        Console.WriteLine();

        // List initial tasks
        Console.WriteLine("1. Listing initial tasks...");
        var initialTasks = await client.ListTasksAsync();
        Console.WriteLine($"Found {initialTasks.TotalCount} tasks");
        Console.WriteLine();

        // Create a new task
        Console.WriteLine("2. Creating a new task...");
        var createRequest = new CreateTaskRequest
        {
            Title = "Test C# REST Client",
            Description = "Testing the C# REST client implementation",
            Priority = TaskPriority.High,
            Tags = new List<string> { "test", "csharp", "rest" },
            AssignedTo = "dev-team",
            CreatedBy = "csharp-client"
        };
        
        var newTask = await client.CreateTaskAsync(createRequest);
        Console.WriteLine($"Created task: {newTask.Id} - {newTask.Title}");
        Console.WriteLine();

        // Get the created task
        Console.WriteLine("3. Retrieving the created task...");
        var retrievedTask = await client.GetTaskAsync(newTask.Id);
        Console.WriteLine($"Retrieved: {retrievedTask.Title} (Status: {retrievedTask.Status})");
        Console.WriteLine();

        // Update task status
        Console.WriteLine("4. Updating task status to InProgress...");
        var updatedTask = await client.UpdateTaskStatusAsync(newTask.Id, TaskStatus.InProgress);
        Console.WriteLine($"Updated status: {updatedTask.Status}");
        Console.WriteLine();

        // Update task with more fields
        Console.WriteLine("5. Updating task details...");
        var updateRequest = new UpdateTaskRequest
        {
            Title = "Test C# REST Client - Updated",
            Priority = TaskPriority.Critical,
            Status = TaskStatus.Completed
        };
        
        var fullyUpdatedTask = await client.UpdateTaskAsync(newTask.Id, updateRequest);
        Console.WriteLine($"Updated task: {fullyUpdatedTask.Title}");
        Console.WriteLine($"Status: {fullyUpdatedTask.Status}, Priority: {fullyUpdatedTask.Priority}");
        Console.WriteLine();

        // List tasks with filtering
        Console.WriteLine("6. Listing completed tasks...");
        var completedTasks = await client.ListTasksAsync(status: TaskStatus.Completed);
        Console.WriteLine($"Found {completedTasks.Tasks.Count} completed tasks");
        Console.WriteLine();

        // Delete the task
        Console.WriteLine("7. Deleting the test task...");
        await client.DeleteTaskAsync(newTask.Id);
        Console.WriteLine("Task deleted successfully");
        Console.WriteLine();

        Console.WriteLine("Demo completed successfully!");
    }
    catch (Exception ex)
    {
        Console.WriteLine($"Demo failed: {ex.Message}");
        Environment.Exit(1);
    }
}, baseUrlOption);

rootCommand.AddCommand(listCommand);
rootCommand.AddCommand(getCommand);
rootCommand.AddCommand(createCommand);
rootCommand.AddCommand(updateCommand);
rootCommand.AddCommand(deleteCommand);
rootCommand.AddCommand(demoCommand);

return await rootCommand.InvokeAsync(args);

static void PrintTask(Models.Task task)
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
    Console.WriteLine($"Created: {task.CreatedAt:yyyy-MM-dd HH:mm:ss}");
    Console.WriteLine($"Updated: {task.UpdatedAt:yyyy-MM-dd HH:mm:ss}");
    if (task.DueDate.HasValue)
        Console.WriteLine($"Due Date: {task.DueDate:yyyy-MM-dd HH:mm:ss}");
}