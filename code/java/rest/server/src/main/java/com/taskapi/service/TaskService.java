package com.taskapi.service;

import com.taskapi.model.*;
import org.springframework.stereotype.Service;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Service
public class TaskService {
    private final Map<String, Task> tasks = new ConcurrentHashMap<>();
    
    public TaskService() {
        initializeSampleData();
    }
    
    private void initializeSampleData() {
        createTask(Task.builder()
                .title("Complete project documentation")
                .description("Write comprehensive documentation for the REST API")
                .status(TaskStatus.IN_PROGRESS)
                .priority(TaskPriority.HIGH)
                .tags(Arrays.asList("documentation", "api"))
                .assignedTo("dev-team")
                .build());
        
        createTask(Task.builder()
                .title("Review pull requests")
                .description("Review and approve pending pull requests")
                .status(TaskStatus.PENDING)
                .priority(TaskPriority.MEDIUM)
                .tags(Arrays.asList("review", "code"))
                .assignedTo("senior-dev")
                .build());
        
        createTask(Task.builder()
                .title("Deploy to production")
                .description("Deploy the latest version to production environment")
                .status(TaskStatus.PENDING)
                .priority(TaskPriority.CRITICAL)
                .tags(Arrays.asList("deployment", "production"))
                .assignedTo("devops")
                .build());
    }
    
    public PageResponse<Task> listTasks(int pageSize, String pageToken, TaskStatus status,
                                         String assignedTo, List<String> tags, String sortOrder) {
        List<Task> filteredTasks = tasks.values().stream()
                .filter(task -> status == null || task.getStatus() == status)
                .filter(task -> assignedTo == null || assignedTo.equals(task.getAssignedTo()))
                .filter(task -> tags == null || tags.isEmpty() || 
                        (task.getTags() != null && task.getTags().containsAll(tags)))
                .collect(Collectors.toList());
        
        // Sort tasks
        if ("created_desc".equals(sortOrder)) {
            filteredTasks.sort((a, b) -> b.getCreatedAt().compareTo(a.getCreatedAt()));
        } else if ("updated_desc".equals(sortOrder)) {
            filteredTasks.sort((a, b) -> b.getUpdatedAt().compareTo(a.getUpdatedAt()));
        } else if ("priority_desc".equals(sortOrder)) {
            filteredTasks.sort((a, b) -> b.getPriority().compareTo(a.getPriority()));
        } else {
            filteredTasks.sort(Comparator.comparing(Task::getCreatedAt));
        }
        
        // Pagination
        int startIndex = 0;
        if (pageToken != null && !pageToken.isEmpty()) {
            try {
                startIndex = Integer.parseInt(pageToken);
            } catch (NumberFormatException e) {
                startIndex = 0;
            }
        }
        
        int endIndex = Math.min(startIndex + pageSize, filteredTasks.size());
        List<Task> paginatedTasks = filteredTasks.subList(startIndex, endIndex);
        
        String nextToken = null;
        if (endIndex < filteredTasks.size()) {
            nextToken = String.valueOf(endIndex);
        }
        
        return PageResponse.<Task>builder()
                .items(paginatedTasks)
                .pageSize(pageSize)
                .nextPageToken(nextToken)
                .totalCount(filteredTasks.size())
                .build();
    }
    
    public Optional<Task> getTask(String id) {
        return Optional.ofNullable(tasks.get(id));
    }
    
    public Task createTask(Task task) {
        if (task.getId() == null) {
            task.setId(UUID.randomUUID().toString());
        }
        task.setCreatedAt(LocalDateTime.now());
        task.setUpdatedAt(LocalDateTime.now());
        
        if (task.getStatus() == null) {
            task.setStatus(TaskStatus.PENDING);
        }
        if (task.getPriority() == null) {
            task.setPriority(TaskPriority.MEDIUM);
        }
        
        tasks.put(task.getId(), task);
        return task;
    }
    
    public Optional<Task> updateTask(String id, Task updates) {
        Task existingTask = tasks.get(id);
        if (existingTask == null) {
            return Optional.empty();
        }
        
        if (updates.getTitle() != null) {
            existingTask.setTitle(updates.getTitle());
        }
        if (updates.getDescription() != null) {
            existingTask.setDescription(updates.getDescription());
        }
        if (updates.getStatus() != null) {
            existingTask.setStatus(updates.getStatus());
        }
        if (updates.getPriority() != null) {
            existingTask.setPriority(updates.getPriority());
        }
        if (updates.getTags() != null) {
            existingTask.setTags(updates.getTags());
        }
        if (updates.getAssignedTo() != null) {
            existingTask.setAssignedTo(updates.getAssignedTo());
        }
        if (updates.getDueDate() != null) {
            existingTask.setDueDate(updates.getDueDate());
        }
        
        existingTask.setUpdatedAt(LocalDateTime.now());
        existingTask.setUpdatedBy(updates.getUpdatedBy());
        
        return Optional.of(existingTask);
    }
    
    public Optional<Task> updateTaskStatus(String id, TaskStatus status) {
        Task task = tasks.get(id);
        if (task == null) {
            return Optional.empty();
        }
        
        task.setStatus(status);
        task.setUpdatedAt(LocalDateTime.now());
        
        return Optional.of(task);
    }
    
    public boolean deleteTask(String id) {
        return tasks.remove(id) != null;
    }
}