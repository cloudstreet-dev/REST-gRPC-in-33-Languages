using System.ComponentModel.DataAnnotations;
using System.Text.Json.Serialization;

namespace TaskRestServer.Models;

public enum TaskStatus
{
    Pending,
    InProgress,
    Completed
}

public enum TaskPriority
{
    Low,
    Medium,
    High,
    Critical
}

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

public class CreateTaskRequest
{
    [Required]
    [StringLength(200, ErrorMessage = "Title must be 200 characters or less")]
    public string Title { get; set; } = string.Empty;
    
    [StringLength(1000, ErrorMessage = "Description must be 1000 characters or less")]
    public string? Description { get; set; }
    
    public TaskPriority Priority { get; set; } = TaskPriority.Medium;
    
    public List<string>? Tags { get; set; }
    
    public string? AssignedTo { get; set; }
    
    public string? CreatedBy { get; set; }
    
    public DateTime? DueDate { get; set; }
}

public class UpdateTaskRequest
{
    public string? Title { get; set; }
    
    [StringLength(1000, ErrorMessage = "Description must be 1000 characters or less")]
    public string? Description { get; set; }
    
    public TaskStatus? Status { get; set; }
    
    public TaskPriority? Priority { get; set; }
    
    public List<string>? Tags { get; set; }
    
    public string? AssignedTo { get; set; }
    
    public DateTime? DueDate { get; set; }
}

public class ListTasksResponse
{
    public List<Task> Tasks { get; set; } = new();
    
    public string? NextPageToken { get; set; }
    
    public int TotalCount { get; set; }
}

public class TaskFilters
{
    public TaskStatus? Status { get; set; }
    
    public string? AssignedTo { get; set; }
    
    public List<string>? Tags { get; set; }
    
    public int PageSize { get; set; } = 20;
    
    public string? PageToken { get; set; }
    
    public string SortBy { get; set; } = "created_at";
    
    public string SortOrder { get; set; } = "desc";
}