using System.Text;
using System.Text.Json;
using TaskRestClient.Models;

namespace TaskRestClient;

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
        string sortBy = "created_at",
        string sortOrder = "desc",
        CancellationToken cancellationToken = default)
    {
        var queryParams = new List<string>
        {
            $"page_size={pageSize}",
            $"sort_by={sortBy}",
            $"sort_order={sortOrder}"
        };

        if (status.HasValue)
        {
            queryParams.Add($"status={status.Value.ToString().ToLower()}");
        }

        if (!string.IsNullOrEmpty(assignedTo))
        {
            queryParams.Add($"assigned_to={Uri.EscapeDataString(assignedTo)}");
        }

        if (tags != null && tags.Count > 0)
        {
            queryParams.Add($"tags={Uri.EscapeDataString(string.Join(",", tags))}");
        }

        if (!string.IsNullOrEmpty(pageToken))
        {
            queryParams.Add($"page_token={Uri.EscapeDataString(pageToken)}");
        }

        var query = string.Join("&", queryParams);
        var response = await _httpClient.GetAsync($"api/tasks?{query}", cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
        
        var json = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<ListTasksResponse>(json, _jsonOptions) 
               ?? throw new InvalidOperationException("Failed to deserialize response");
    }

    public async Task<Models.Task> GetTaskAsync(string id, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrEmpty(id))
            throw new ArgumentException("Task ID cannot be null or empty", nameof(id));

        var response = await _httpClient.GetAsync($"api/tasks/{Uri.EscapeDataString(id)}", cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
        
        var json = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<Models.Task>(json, _jsonOptions) 
               ?? throw new InvalidOperationException("Failed to deserialize task");
    }

    public async Task<Models.Task> CreateTaskAsync(CreateTaskRequest request, CancellationToken cancellationToken = default)
    {
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        if (string.IsNullOrWhiteSpace(request.Title))
            throw new ArgumentException("Title is required", nameof(request));

        var json = JsonSerializer.Serialize(request, _jsonOptions);
        var content = new StringContent(json, Encoding.UTF8, "application/json");
        
        var response = await _httpClient.PostAsync("api/tasks", content, cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
        
        var responseJson = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<Models.Task>(responseJson, _jsonOptions) 
               ?? throw new InvalidOperationException("Failed to deserialize created task");
    }

    public async Task<Models.Task> UpdateTaskAsync(string id, UpdateTaskRequest request, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrEmpty(id))
            throw new ArgumentException("Task ID cannot be null or empty", nameof(id));
        
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        var json = JsonSerializer.Serialize(request, _jsonOptions);
        var content = new StringContent(json, Encoding.UTF8, "application/json");
        
        var response = await _httpClient.PutAsync($"api/tasks/{Uri.EscapeDataString(id)}", content, cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
        
        var responseJson = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<Models.Task>(responseJson, _jsonOptions) 
               ?? throw new InvalidOperationException("Failed to deserialize updated task");
    }

    public async Task<Models.Task> UpdateTaskStatusAsync(string id, TaskStatus status, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrEmpty(id))
            throw new ArgumentException("Task ID cannot be null or empty", nameof(id));

        var json = JsonSerializer.Serialize(status, _jsonOptions);
        var content = new StringContent(json, Encoding.UTF8, "application/json");
        
        var response = await _httpClient.PatchAsync($"api/tasks/{Uri.EscapeDataString(id)}/status", content, cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
        
        var responseJson = await response.Content.ReadAsStringAsync(cancellationToken);
        return JsonSerializer.Deserialize<Models.Task>(responseJson, _jsonOptions) 
               ?? throw new InvalidOperationException("Failed to deserialize updated task");
    }

    public async Task DeleteTaskAsync(string id, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrEmpty(id))
            throw new ArgumentException("Task ID cannot be null or empty", nameof(id));

        var response = await _httpClient.DeleteAsync($"api/tasks/{Uri.EscapeDataString(id)}", cancellationToken);
        
        await EnsureSuccessStatusCodeAsync(response);
    }

    private async Task EnsureSuccessStatusCodeAsync(HttpResponseMessage response)
    {
        if (response.IsSuccessStatusCode)
            return;

        var errorContent = await response.Content.ReadAsStringAsync();
        ErrorResponse? errorResponse = null;

        try
        {
            errorResponse = JsonSerializer.Deserialize<ErrorResponse>(errorContent, _jsonOptions);
        }
        catch
        {
            // If we can't deserialize the error, use the raw content
        }

        var message = errorResponse?.Message ?? $"HTTP {(int)response.StatusCode}: {response.ReasonPhrase}";
        
        throw response.StatusCode switch
        {
            System.Net.HttpStatusCode.BadRequest => new ArgumentException(message),
            System.Net.HttpStatusCode.NotFound => new InvalidOperationException($"Task not found: {message}"),
            System.Net.HttpStatusCode.Unauthorized => new UnauthorizedAccessException(message),
            System.Net.HttpStatusCode.Forbidden => new UnauthorizedAccessException(message),
            _ => new HttpRequestException($"Request failed: {message}")
        };
    }

    public void Dispose()
    {
        _httpClient?.Dispose();
    }
}