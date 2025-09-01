using Grpc.Core;
using Google.Protobuf.WellKnownTypes;
using System.Collections.Concurrent;
using Task = TaskNamespace;

namespace TaskGrpcServer.Services;

public class TaskService : Task.TaskService.TaskServiceBase
{
    private readonly ConcurrentDictionary<string, Task.Task> _tasks = new();
    private readonly ILogger<TaskService> _logger;

    public TaskService(ILogger<TaskService> logger)
    {
        _logger = logger;
        InitializeSampleData();
    }

    private void InitializeSampleData()
    {
        var now = Timestamp.FromDateTime(DateTime.UtcNow);
        
        var tasks = new[]
        {
            new Task.Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = "Complete project documentation",
                Description = "Write comprehensive documentation for the gRPC API",
                Status = Task.TaskStatus.InProgress,
                Priority = Task.TaskPriority.High,
                Tags = { "documentation", "api" },
                AssignedTo = "dev-team",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            },
            new Task.Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = "Review pull requests",
                Description = "Review and approve pending pull requests",
                Status = Task.TaskStatus.Pending,
                Priority = Task.TaskPriority.Medium,
                Tags = { "review", "code" },
                AssignedTo = "senior-dev",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            },
            new Task.Task
            {
                Id = Guid.NewGuid().ToString(),
                Title = "Deploy to production",
                Description = "Deploy the latest version to production environment",
                Status = Task.TaskStatus.Pending,
                Priority = Task.TaskPriority.Critical,
                Tags = { "deployment", "production" },
                AssignedTo = "devops",
                CreatedBy = "system",
                CreatedAt = now,
                UpdatedAt = now
            }
        };

        foreach (var task in tasks)
        {
            _tasks[task.Id] = task;
        }
    }

    public override async Task<Task.Task> GetTask(Task.GetTaskRequest request, ServerCallContext context)
    {
        _logger.LogInformation("GetTask called with ID: {TaskId}", request.Id);

        if (string.IsNullOrEmpty(request.Id))
        {
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Task ID is required"));
        }

        if (!_tasks.TryGetValue(request.Id, out var task))
        {
            throw new RpcException(new Status(StatusCode.NotFound, $"Task with ID '{request.Id}' not found"));
        }

        return await System.Threading.Tasks.Task.FromResult(task);
    }

    public override async Task ListTasks(Task.ListTasksRequest request, IServerStreamWriter<Task.Task> responseStream, ServerCallContext context)
    {
        _logger.LogInformation("ListTasks called");

        var tasks = _tasks.Values.AsEnumerable();

        // Apply filters
        if (request.Status != Task.TaskStatus.Unspecified)
        {
            tasks = tasks.Where(t => t.Status == request.Status);
        }

        if (!string.IsNullOrEmpty(request.AssignedTo))
        {
            tasks = tasks.Where(t => t.AssignedTo == request.AssignedTo);
        }

        if (request.Tags.Count > 0)
        {
            tasks = tasks.Where(t => request.Tags.Any(tag => t.Tags.Contains(tag)));
        }

        // Apply sorting
        tasks = request.SortOrder switch
        {
            Task.SortOrder.CreatedAsc => tasks.OrderBy(t => t.CreatedAt.ToDateTime()),
            Task.SortOrder.CreatedDesc => tasks.OrderByDescending(t => t.CreatedAt.ToDateTime()),
            Task.SortOrder.UpdatedAsc => tasks.OrderBy(t => t.UpdatedAt.ToDateTime()),
            Task.SortOrder.UpdatedDesc => tasks.OrderByDescending(t => t.UpdatedAt.ToDateTime()),
            Task.SortOrder.PriorityDesc => tasks.OrderByDescending(t => (int)t.Priority).ThenBy(t => t.CreatedAt.ToDateTime()),
            _ => tasks.OrderByDescending(t => t.CreatedAt.ToDateTime())
        };

        // Apply pagination
        var pageSize = request.PageSize > 0 ? Math.Min((int)request.PageSize, 100) : 20;
        var skip = 0;
        
        if (!string.IsNullOrEmpty(request.PageToken) && int.TryParse(request.PageToken, out var parsedToken))
        {
            skip = parsedToken;
        }

        var paginatedTasks = tasks.Skip(skip).Take(pageSize);

        foreach (var task in paginatedTasks)
        {
            if (context.CancellationToken.IsCancellationRequested)
                break;

            await responseStream.WriteAsync(task);
        }
    }

    public override async Task<Task.Task> CreateTask(Task.CreateTaskRequest request, ServerCallContext context)
    {
        _logger.LogInformation("CreateTask called");

        if (request.Task == null)
        {
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Task is required"));
        }

        var taskRequest = request.Task;

        // Validate
        if (string.IsNullOrWhiteSpace(taskRequest.Title))
        {
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Title is required"));
        }

        if (taskRequest.Title.Length > 200)
        {
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Title must be 200 characters or less"));
        }

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

        _logger.LogInformation("Created task {TaskId}: {Title}", task.Id, task.Title);

        return await System.Threading.Tasks.Task.FromResult(task);
    }

    public override async Task<Task.Task> UpdateTask(Task.UpdateTaskRequest request, ServerCallContext context)
    {
        _logger.LogInformation("UpdateTask called with ID: {TaskId}", request.Task?.Id);

        if (request.Task == null || string.IsNullOrEmpty(request.Task.Id))
        {
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Task with ID is required"));
        }

        if (!_tasks.TryGetValue(request.Task.Id, out var existingTask))
        {
            throw new RpcException(new Status(StatusCode.NotFound, $"Task with ID '{request.Task.Id}' not found"));
        }

        // Create a copy of the existing task
        var updatedTask = existingTask.Clone();

        // Apply updates based on update mask
        foreach (var field in request.UpdateMask)
        {
            switch (field)
            {
                case "title":
                    if (string.IsNullOrWhiteSpace(request.Task.Title))
                    {
                        throw new RpcException(new Status(StatusCode.InvalidArgument, "Title cannot be empty"));
                    }
                    updatedTask.Title = request.Task.Title;
                    break;
                case "description":
                    updatedTask.Description = request.Task.Description;
                    break;
                case "status":
                    updatedTask.Status = request.Task.Status;
                    break;
                case "priority":
                    updatedTask.Priority = request.Task.Priority;
                    break;
                case "tags":
                    updatedTask.Tags.Clear();
                    updatedTask.Tags.AddRange(request.Task.Tags);
                    break;
                case "assigned_to":
                    updatedTask.AssignedTo = request.Task.AssignedTo;
                    break;
                case "due_date":
                    updatedTask.DueDate = request.Task.DueDate;
                    break;
            }
        }

        updatedTask.UpdatedAt = Timestamp.FromDateTime(DateTime.UtcNow);
        _tasks[updatedTask.Id] = updatedTask;

        _logger.LogInformation("Updated task {TaskId}: {Title}", updatedTask.Id, updatedTask.Title);

        return await System.Threading.Tasks.Task.FromResult(updatedTask);
    }

    public override async Task<Empty> DeleteTask(Task.DeleteTaskRequest request, ServerCallContext context)
    {
        _logger.LogInformation("DeleteTask called with ID: {TaskId}", request.Id);

        if (string.IsNullOrEmpty(request.Id))
        {
            throw new RpcException(new Status(StatusCode.InvalidArgument, "Task ID is required"));
        }

        if (!_tasks.TryRemove(request.Id, out var removedTask))
        {
            throw new RpcException(new Status(StatusCode.NotFound, $"Task with ID '{request.Id}' not found"));
        }

        _logger.LogInformation("Deleted task {TaskId}: {Title}", removedTask.Id, removedTask.Title);

        return await System.Threading.Tasks.Task.FromResult(new Empty());
    }

    public override async Task WatchTasks(IAsyncStreamReader<Task.WatchTasksRequest> requestStream, 
                                           IServerStreamWriter<Task.TaskEvent> responseStream, 
                                           ServerCallContext context)
    {
        _logger.LogInformation("WatchTasks called");

        await foreach (var request in requestStream.ReadAllAsync())
        {
            if (context.CancellationToken.IsCancellationRequested)
                break;

            var now = Timestamp.FromDateTime(DateTime.UtcNow);
            var tasks = _tasks.Values.AsEnumerable();

            if (request.WatchAll)
            {
                // Send all tasks as events
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
            else if (request.TaskIds.Count > 0)
            {
                // Send specific tasks
                foreach (var taskId in request.TaskIds)
                {
                    if (_tasks.TryGetValue(taskId, out var task))
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
            }
            else if (!string.IsNullOrEmpty(request.AssignedTo))
            {
                // Send tasks assigned to specific user
                var assignedTasks = tasks.Where(t => t.AssignedTo == request.AssignedTo);
                
                foreach (var task in assignedTasks)
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

            // For demo purposes, we'll send a limited number of events
            // In a real implementation, this would maintain persistent connections
            // and send events as they occur
        }
    }
}