module TaskRestClient.ApiClient

open System
open System.Net.Http
open System.Text
open System.Text.Json
open TaskRestClient.Models

type TaskApiError = 
    | InvalidUrl of string
    | InvalidResponse of string
    | ServerError of int * string
    | DecodingError of string
    | NetworkError of string

type TaskApiClient(baseUrl: string) =
    let httpClient = 
        let client = new HttpClient(BaseAddress = Uri(baseUrl), Timeout = TimeSpan.FromSeconds(30.0))
        client.DefaultRequestHeaders.Add("Accept", "application/json")
        client
    
    let jsonOptions = 
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.SnakeCaseLower
        options.WriteIndented <- true
        options.Converters.Add(JsonStringEnumConverter())
        options

    let handleResponse<'T> (response: HttpResponseMessage) = 
        async {
            if response.IsSuccessStatusCode then
                let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                try
                    let result = JsonSerializer.Deserialize<'T>(content, jsonOptions)
                    return Ok result
                with
                | ex -> 
                    return Error (DecodingError (sprintf "Failed to deserialize response: %s" ex.Message))
            else
                let! errorContent = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                let errorMessage = 
                    try
                        let errorResponse = JsonSerializer.Deserialize<ErrorResponse>(errorContent, jsonOptions)
                        errorResponse.Message
                    with
                    | _ -> sprintf "HTTP %d: %s" (int response.StatusCode) response.ReasonPhrase
                
                return Error (ServerError (int response.StatusCode, errorMessage))
        }

    let buildQueryString (parameters: (string * string option) list) =
        parameters
        |> List.choose (fun (key, value) -> 
            match value with 
            | Some v when not (String.IsNullOrEmpty(v)) -> Some (sprintf "%s=%s" key (Uri.EscapeDataString(v)))
            | _ -> None)
        |> String.concat "&"

    member this.ListTasksAsync(
        ?status: TaskStatus,
        ?assignedTo: string,
        ?tags: string list,
        ?pageSize: int,
        ?pageToken: string,
        ?sortBy: string,
        ?sortOrder: string
    ) = 
        async {
            try
                let parameters = [
                    ("page_size", pageSize |> Option.map string |> Option.defaultValue "20" |> Some)
                    ("sort_by", sortBy |> Option.defaultValue "created_at" |> Some)
                    ("sort_order", sortOrder |> Option.defaultValue "desc" |> Some)
                    ("status", status |> Option.map (fun s -> s.ToString().ToLower()))
                    ("assigned_to", assignedTo)
                    ("tags", tags |> Option.map (String.concat ","))
                    ("page_token", pageToken)
                ]
                
                let queryString = buildQueryString parameters
                let url = sprintf "api/tasks?%s" queryString
                
                let! response = httpClient.GetAsync(url) |> Async.AwaitTask
                return! handleResponse<ListTasksResponse> response
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }

    member this.GetTaskAsync(id: string) = 
        async {
            try
                if String.IsNullOrEmpty(id) then
                    return Error (InvalidResponse "Task ID cannot be null or empty")
                else
                    let url = sprintf "api/tasks/%s" (Uri.EscapeDataString(id))
                    let! response = httpClient.GetAsync(url) |> Async.AwaitTask
                    return! handleResponse<Task> response
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }

    member this.CreateTaskAsync(request: CreateTaskRequest) = 
        async {
            try
                if String.IsNullOrWhiteSpace(request.Title) then
                    return Error (InvalidResponse "Title is required")
                else
                    let json = JsonSerializer.Serialize(request, jsonOptions)
                    let content = new StringContent(json, Encoding.UTF8, "application/json")
                    
                    let! response = httpClient.PostAsync("api/tasks", content) |> Async.AwaitTask
                    return! handleResponse<Task> response
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }

    member this.UpdateTaskAsync(id: string, request: UpdateTaskRequest) = 
        async {
            try
                if String.IsNullOrEmpty(id) then
                    return Error (InvalidResponse "Task ID cannot be null or empty")
                else
                    let json = JsonSerializer.Serialize(request, jsonOptions)
                    let content = new StringContent(json, Encoding.UTF8, "application/json")
                    let url = sprintf "api/tasks/%s" (Uri.EscapeDataString(id))
                    
                    let! response = httpClient.PutAsync(url, content) |> Async.AwaitTask
                    return! handleResponse<Task> response
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }

    member this.UpdateTaskStatusAsync(id: string, status: TaskStatus) = 
        async {
            try
                if String.IsNullOrEmpty(id) then
                    return Error (InvalidResponse "Task ID cannot be null or empty")
                else
                    let json = JsonSerializer.Serialize(status, jsonOptions)
                    let content = new StringContent(json, Encoding.UTF8, "application/json")
                    let url = sprintf "api/tasks/%s/status" (Uri.EscapeDataString(id))
                    
                    let! response = httpClient.PatchAsync(url, content) |> Async.AwaitTask
                    return! handleResponse<Task> response
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }

    member this.DeleteTaskAsync(id: string) = 
        async {
            try
                if String.IsNullOrEmpty(id) then
                    return Error (InvalidResponse "Task ID cannot be null or empty")
                else
                    let url = sprintf "api/tasks/%s" (Uri.EscapeDataString(id))
                    let! response = httpClient.DeleteAsync(url) |> Async.AwaitTask
                    
                    if response.IsSuccessStatusCode then
                        return Ok ()
                    else
                        let! errorContent = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                        return Error (ServerError (int response.StatusCode, errorContent))
            with
            | ex -> 
                return Error (NetworkError (sprintf "Network error: %s" ex.Message))
        }

    interface IDisposable with
        member this.Dispose() = 
            httpClient.Dispose()

module TaskPrinter =
    let printTask (task: Task) =
        printfn "ID: %s" task.Id
        printfn "Title: %s" task.Title
        match task.Description with
        | Some desc -> printfn "Description: %s" desc
        | None -> ()
        printfn "Status: %A" task.Status
        printfn "Priority: %A" task.Priority
        if not task.Tags.IsEmpty then
            printfn "Tags: %s" (String.concat ", " task.Tags)
        match task.AssignedTo with
        | Some assignee -> printfn "Assigned To: %s" assignee
        | None -> ()
        printfn "Created By: %s" task.CreatedBy
        printfn "Created: %s" (task.CreatedAt.ToString("yyyy-MM-dd HH:mm:ss"))
        printfn "Updated: %s" (task.UpdatedAt.ToString("yyyy-MM-dd HH:mm:ss"))
        match task.DueDate with
        | Some due -> printfn "Due Date: %s" (due.ToString("yyyy-MM-dd HH:mm:ss"))
        | None -> ()

    let printError (error: TaskApiError) =
        match error with
        | InvalidUrl msg -> printfn "Invalid URL: %s" msg
        | InvalidResponse msg -> printfn "Invalid response: %s" msg
        | ServerError (code, msg) -> printfn "Server error (%d): %s" code msg
        | DecodingError msg -> printfn "Decoding error: %s" msg
        | NetworkError msg -> printfn "Network error: %s" msg