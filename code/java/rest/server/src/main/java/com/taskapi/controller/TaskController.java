package com.taskapi.controller;

import com.taskapi.model.*;
import com.taskapi.service.TaskService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/v1/tasks")
@RequiredArgsConstructor
@Tag(name = "Tasks", description = "Task management endpoints")
@CrossOrigin(origins = "*")
public class TaskController {
    
    private final TaskService taskService;
    
    @GetMapping
    @Operation(summary = "List all tasks", description = "Retrieve a paginated list of tasks with optional filters")
    @ApiResponse(responseCode = "200", description = "Tasks retrieved successfully")
    public ResponseEntity<PageResponse<Task>> listTasks(
            @Parameter(description = "Number of items per page")
            @RequestParam(defaultValue = "20") int pageSize,
            
            @Parameter(description = "Token for the next page")
            @RequestParam(required = false) String pageToken,
            
            @Parameter(description = "Filter by task status")
            @RequestParam(required = false) TaskStatus status,
            
            @Parameter(description = "Filter by assigned user")
            @RequestParam(required = false) String assignedTo,
            
            @Parameter(description = "Filter by tags (comma-separated)")
            @RequestParam(required = false) String tags,
            
            @Parameter(description = "Sort order")
            @RequestParam(required = false) String sortOrder) {
        
        List<String> tagList = null;
        if (tags != null && !tags.isEmpty()) {
            tagList = Arrays.asList(tags.split(","));
        }
        
        PageResponse<Task> response = taskService.listTasks(
                Math.min(pageSize, 100),
                pageToken,
                status,
                assignedTo,
                tagList,
                sortOrder
        );
        
        return ResponseEntity.ok(response);
    }
    
    @GetMapping("/{id}")
    @Operation(summary = "Get a task", description = "Retrieve a specific task by ID")
    @ApiResponse(responseCode = "200", description = "Task retrieved successfully")
    @ApiResponse(responseCode = "404", description = "Task not found")
    public ResponseEntity<Task> getTask(@PathVariable String id) {
        return taskService.getTask(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
    
    @PostMapping
    @Operation(summary = "Create a task", description = "Create a new task")
    @ApiResponse(responseCode = "201", description = "Task created successfully")
    @ApiResponse(responseCode = "400", description = "Invalid request data")
    public ResponseEntity<Task> createTask(@Valid @RequestBody Task task) {
        Task created = taskService.createTask(task);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }
    
    @PutMapping("/{id}")
    @Operation(summary = "Update a task", description = "Update an existing task")
    @ApiResponse(responseCode = "200", description = "Task updated successfully")
    @ApiResponse(responseCode = "404", description = "Task not found")
    public ResponseEntity<Task> updateTask(
            @PathVariable String id,
            @Valid @RequestBody Task task) {
        return taskService.updateTask(id, task)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
    
    @PatchMapping("/{id}/status")
    @Operation(summary = "Update task status", description = "Update only the status of a task")
    @ApiResponse(responseCode = "200", description = "Status updated successfully")
    @ApiResponse(responseCode = "404", description = "Task not found")
    public ResponseEntity<Task> updateTaskStatus(
            @PathVariable String id,
            @RequestBody Map<String, String> statusUpdate) {
        
        try {
            TaskStatus status = TaskStatus.valueOf(statusUpdate.get("status").toUpperCase());
            return taskService.updateTaskStatus(id, status)
                    .map(ResponseEntity::ok)
                    .orElse(ResponseEntity.notFound().build());
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().build();
        }
    }
    
    @DeleteMapping("/{id}")
    @Operation(summary = "Delete a task", description = "Delete a task by ID")
    @ApiResponse(responseCode = "204", description = "Task deleted successfully")
    @ApiResponse(responseCode = "404", description = "Task not found")
    public ResponseEntity<Void> deleteTask(@PathVariable String id) {
        if (taskService.deleteTask(id)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.notFound().build();
    }
}