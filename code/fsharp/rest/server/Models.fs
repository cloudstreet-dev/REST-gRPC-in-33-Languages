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

[<CLIMutable>]
type Task = {
    [<Key>]
    Id: string
    
    [<Required>]
    [<StringLength(200, ErrorMessage = "Title must be 200 characters or less")>]
    Title: string
    
    [<StringLength(1000, ErrorMessage = "Description must be 1000 characters or less")>]
    Description: string option
    
    Status: TaskStatus
    Priority: TaskPriority
    Tags: string list
    AssignedTo: string option
    
    [<Required>]
    CreatedBy: string
    
    CreatedAt: DateTime
    UpdatedAt: DateTime
    DueDate: DateTime option
}

[<CLIMutable>]
type CreateTaskRequest = {
    [<Required>]
    [<StringLength(200, ErrorMessage = "Title must be 200 characters or less")>]
    Title: string
    
    [<StringLength(1000, ErrorMessage = "Description must be 1000 characters or less")>]
    Description: string option
    
    Priority: TaskPriority option
    Tags: string list option
    AssignedTo: string option
    CreatedBy: string option
    DueDate: DateTime option
}

[<CLIMutable>]
type UpdateTaskRequest = {
    Title: string option
    
    [<StringLength(1000, ErrorMessage = "Description must be 1000 characters or less")>]
    Description: string option
    
    Status: TaskStatus option
    Priority: TaskPriority option
    Tags: string list option
    AssignedTo: string option
    DueDate: DateTime option
}

[<CLIMutable>]
type ListTasksResponse = {
    Tasks: Task list
    NextPageToken: string option
    TotalCount: int
}

[<CLIMutable>]
type TaskFilters = {
    Status: TaskStatus option
    AssignedTo: string option
    Tags: string list option
    PageSize: int option
    PageToken: string option
    SortBy: string option
    SortOrder: string option
}

[<CLIMutable>]
type ErrorResponse = {
    Message: string
    Errors: Map<string, string list> option
}