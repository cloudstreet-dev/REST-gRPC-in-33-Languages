using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using TaskRestServer.Data;
using TaskRestServer.Models;
using System.ComponentModel.DataAnnotations;

namespace TaskRestServer.Controllers;

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

    /// <summary>
    /// Get all tasks with optional filtering, sorting, and pagination
    /// </summary>
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
            pageSize = Math.Min(pageSize, 100); // Limit page size
            
            var query = _context.Tasks.AsQueryable();

            // Apply filters
            if (status.HasValue)
            {
                query = query.Where(t => t.Status == status.Value);
            }

            if (!string.IsNullOrEmpty(assignedTo))
            {
                query = query.Where(t => t.AssignedTo == assignedTo);
            }

            if (!string.IsNullOrEmpty(tags))
            {
                var tagList = tags.Split(',', StringSplitOptions.RemoveEmptyEntries).ToList();
                query = query.Where(t => tagList.Any(tag => t.Tags.Contains(tag)));
            }

            // Apply sorting
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
                "updated_at" => sortOrder.ToLower() == "asc" 
                    ? query.OrderBy(t => t.UpdatedAt) 
                    : query.OrderByDescending(t => t.UpdatedAt),
                "created_at" or _ => sortOrder.ToLower() == "asc" 
                    ? query.OrderBy(t => t.CreatedAt) 
                    : query.OrderByDescending(t => t.CreatedAt)
            };

            // Apply pagination
            var skip = 0;
            if (!string.IsNullOrEmpty(pageToken) && int.TryParse(pageToken, out var parsedToken))
            {
                skip = parsedToken;
            }

            var totalCount = await query.CountAsync();
            var tasks = await query.Skip(skip).Take(pageSize).ToListAsync();

            var response = new ListTasksResponse
            {
                Tasks = tasks,
                TotalCount = totalCount,
                NextPageToken = (skip + pageSize < totalCount) ? (skip + pageSize).ToString() : null
            };

            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error retrieving tasks");
            return StatusCode(500, new { message = "Internal server error" });
        }
    }

    /// <summary>
    /// Get a specific task by ID
    /// </summary>
    [HttpGet("{id}")]
    public async Task<ActionResult<Models.Task>> GetTask(string id)
    {
        try
        {
            var task = await _context.Tasks.FindAsync(id);
            
            if (task == null)
            {
                return NotFound(new { message = $"Task with ID '{id}' not found" });
            }

            return Ok(task);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error retrieving task {TaskId}", id);
            return StatusCode(500, new { message = "Internal server error" });
        }
    }

    /// <summary>
    /// Create a new task
    /// </summary>
    [HttpPost]
    public async Task<ActionResult<Models.Task>> CreateTask([FromBody] CreateTaskRequest request)
    {
        try
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var task = new Models.Task
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

            _logger.LogInformation("Created task {TaskId}: {Title}", task.Id, task.Title);
            
            return CreatedAtAction(nameof(GetTask), new { id = task.Id }, task);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error creating task");
            return StatusCode(500, new { message = "Internal server error" });
        }
    }

    /// <summary>
    /// Update an existing task
    /// </summary>
    [HttpPut("{id}")]
    public async Task<ActionResult<Models.Task>> UpdateTask(string id, [FromBody] UpdateTaskRequest request)
    {
        try
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var task = await _context.Tasks.FindAsync(id);
            
            if (task == null)
            {
                return NotFound(new { message = $"Task with ID '{id}' not found" });
            }

            // Update only provided fields
            if (request.Title != null)
            {
                task.Title = request.Title.Trim();
            }
            
            if (request.Description != null)
            {
                task.Description = request.Description.Trim();
            }
            
            if (request.Status.HasValue)
            {
                task.Status = request.Status.Value;
            }
            
            if (request.Priority.HasValue)
            {
                task.Priority = request.Priority.Value;
            }
            
            if (request.Tags != null)
            {
                task.Tags = request.Tags;
            }
            
            if (request.AssignedTo != null)
            {
                task.AssignedTo = request.AssignedTo.Trim();
            }
            
            if (request.DueDate.HasValue)
            {
                task.DueDate = request.DueDate;
            }

            task.UpdatedAt = DateTime.UtcNow;

            await _context.SaveChangesAsync();

            _logger.LogInformation("Updated task {TaskId}: {Title}", task.Id, task.Title);
            
            return Ok(task);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error updating task {TaskId}", id);
            return StatusCode(500, new { message = "Internal server error" });
        }
    }

    /// <summary>
    /// Update task status
    /// </summary>
    [HttpPatch("{id}/status")]
    public async Task<ActionResult<Models.Task>> UpdateTaskStatus(string id, [FromBody] TaskStatus status)
    {
        try
        {
            var task = await _context.Tasks.FindAsync(id);
            
            if (task == null)
            {
                return NotFound(new { message = $"Task with ID '{id}' not found" });
            }

            task.Status = status;
            task.UpdatedAt = DateTime.UtcNow;

            await _context.SaveChangesAsync();

            _logger.LogInformation("Updated task {TaskId} status to {Status}", task.Id, status);
            
            return Ok(task);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error updating task status {TaskId}", id);
            return StatusCode(500, new { message = "Internal server error" });
        }
    }

    /// <summary>
    /// Delete a task
    /// </summary>
    [HttpDelete("{id}")]
    public async Task<IActionResult> DeleteTask(string id)
    {
        try
        {
            var task = await _context.Tasks.FindAsync(id);
            
            if (task == null)
            {
                return NotFound(new { message = $"Task with ID '{id}' not found" });
            }

            _context.Tasks.Remove(task);
            await _context.SaveChangesAsync();

            _logger.LogInformation("Deleted task {TaskId}: {Title}", task.Id, task.Title);
            
            return NoContent();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error deleting task {TaskId}", id);
            return StatusCode(500, new { message = "Internal server error" });
        }
    }
}