module TaskRestClient.Models

open System
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

[<CLIMutable>]
type Task = {
    [<JsonPropertyName("id")>]
    Id: string
    
    [<JsonPropertyName("title")>]
    Title: string
    
    [<JsonPropertyName("description")>]
    Description: string option
    
    [<JsonPropertyName("status")>]
    Status: TaskStatus
    
    [<JsonPropertyName("priority")>]
    Priority: TaskPriority
    
    [<JsonPropertyName("tags")>]
    Tags: string list
    
    [<JsonPropertyName("assigned_to")>]
    AssignedTo: string option
    
    [<JsonPropertyName("created_by")>]
    CreatedBy: string
    
    [<JsonPropertyName("created_at")>]
    CreatedAt: DateTime
    
    [<JsonPropertyName("updated_at")>]
    UpdatedAt: DateTime
    
    [<JsonPropertyName("due_date")>]
    DueDate: DateTime option
}

[<CLIMutable>]
type CreateTaskRequest = {
    [<JsonPropertyName("title")>]
    Title: string
    
    [<JsonPropertyName("description")>]
    Description: string option
    
    [<JsonPropertyName("priority")>]
    Priority: TaskPriority option
    
    [<JsonPropertyName("tags")>]
    Tags: string list option
    
    [<JsonPropertyName("assigned_to")>]
    AssignedTo: string option
    
    [<JsonPropertyName("created_by")>]
    CreatedBy: string option
    
    [<JsonPropertyName("due_date")>]
    DueDate: DateTime option
}

[<CLIMutable>]
type UpdateTaskRequest = {
    [<JsonPropertyName("title")>]
    Title: string option
    
    [<JsonPropertyName("description")>]
    Description: string option
    
    [<JsonPropertyName("status")>]
    Status: TaskStatus option
    
    [<JsonPropertyName("priority")>]
    Priority: TaskPriority option
    
    [<JsonPropertyName("tags")>]
    Tags: string list option
    
    [<JsonPropertyName("assigned_to")>]
    AssignedTo: string option
    
    [<JsonPropertyName("due_date")>]
    DueDate: DateTime option
}

[<CLIMutable>]
type ListTasksResponse = {
    [<JsonPropertyName("tasks")>]
    Tasks: Task list
    
    [<JsonPropertyName("next_page_token")>]
    NextPageToken: string option
    
    [<JsonPropertyName("total_count")>]
    TotalCount: int
}

[<CLIMutable>]
type ErrorResponse = {
    [<JsonPropertyName("message")>]
    Message: string
    
    [<JsonPropertyName("errors")>]
    Errors: Map<string, string list> option
}