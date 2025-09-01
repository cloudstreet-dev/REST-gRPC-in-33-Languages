using System.Text.Json.Serialization;

namespace TaskRestClient.Models;

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum TaskStatus
{
    Pending,
    InProgress,
    Completed
}

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum TaskPriority
{
    Low,
    Medium,
    High,
    Critical
}

public class Task
{
    [JsonPropertyName("id")]
    public string Id { get; set; } = string.Empty;
    
    [JsonPropertyName("title")]
    public string Title { get; set; } = string.Empty;
    
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    
    [JsonPropertyName("status")]
    public TaskStatus Status { get; set; } = TaskStatus.Pending;
    
    [JsonPropertyName("priority")]
    public TaskPriority Priority { get; set; } = TaskPriority.Medium;
    
    [JsonPropertyName("tags")]
    public List<string> Tags { get; set; } = new();
    
    [JsonPropertyName("assigned_to")]
    public string? AssignedTo { get; set; }
    
    [JsonPropertyName("created_by")]
    public string CreatedBy { get; set; } = string.Empty;
    
    [JsonPropertyName("created_at")]
    public DateTime CreatedAt { get; set; }
    
    [JsonPropertyName("updated_at")]
    public DateTime UpdatedAt { get; set; }
    
    [JsonPropertyName("due_date")]
    public DateTime? DueDate { get; set; }
}

public class CreateTaskRequest
{
    [JsonPropertyName("title")]
    public string Title { get; set; } = string.Empty;
    
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    
    [JsonPropertyName("priority")]
    public TaskPriority Priority { get; set; } = TaskPriority.Medium;
    
    [JsonPropertyName("tags")]
    public List<string>? Tags { get; set; }
    
    [JsonPropertyName("assigned_to")]
    public string? AssignedTo { get; set; }
    
    [JsonPropertyName("created_by")]
    public string? CreatedBy { get; set; }
    
    [JsonPropertyName("due_date")]
    public DateTime? DueDate { get; set; }
}

public class UpdateTaskRequest
{
    [JsonPropertyName("title")]
    public string? Title { get; set; }
    
    [JsonPropertyName("description")]
    public string? Description { get; set; }
    
    [JsonPropertyName("status")]
    public TaskStatus? Status { get; set; }
    
    [JsonPropertyName("priority")]
    public TaskPriority? Priority { get; set; }
    
    [JsonPropertyName("tags")]
    public List<string>? Tags { get; set; }
    
    [JsonPropertyName("assigned_to")]
    public string? AssignedTo { get; set; }
    
    [JsonPropertyName("due_date")]
    public DateTime? DueDate { get; set; }
}

public class ListTasksResponse
{
    [JsonPropertyName("tasks")]
    public List<Task> Tasks { get; set; } = new();
    
    [JsonPropertyName("next_page_token")]
    public string? NextPageToken { get; set; }
    
    [JsonPropertyName("total_count")]
    public int TotalCount { get; set; }
}

public class ErrorResponse
{
    [JsonPropertyName("message")]
    public string Message { get; set; } = string.Empty;
    
    [JsonPropertyName("errors")]
    public Dictionary<string, List<string>>? Errors { get; set; }
}