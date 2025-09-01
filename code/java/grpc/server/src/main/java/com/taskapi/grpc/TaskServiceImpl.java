package com.taskapi.grpc;

import com.google.protobuf.Empty;
import com.google.protobuf.Timestamp;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;
// Import generated classes (will be available after proto compilation)
// import com.taskapi.grpc.generated.*;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@Slf4j
public class TaskServiceImpl /* extends TaskServiceGrpc.TaskServiceImplBase */ {
    
    private final Map<String, Object> tasks = new ConcurrentHashMap<>();
    
    public TaskServiceImpl() {
        initializeSampleData();
    }
    
    private void initializeSampleData() {
        // Sample tasks will be created after proto generation
        log.info("Sample data initialized");
    }
    
    /*
    @Override
    public void listTasks(ListTasksRequest request, StreamObserver<Task> responseObserver) {
        try {
            // Filter tasks based on request parameters
            List<Task> filteredTasks = filterTasks(request);
            
            // Sort tasks
            List<Task> sortedTasks = sortTasks(filteredTasks, request.getSortOrder());
            
            // Apply pagination
            int pageSize = request.getPageSize() > 0 ? request.getPageSize() : 20;
            pageSize = Math.min(pageSize, 100);
            
            int startIndex = 0;
            if (!request.getPageToken().isEmpty()) {
                try {
                    startIndex = Integer.parseInt(request.getPageToken());
                } catch (NumberFormatException e) {
                    startIndex = 0;
                }
            }
            
            // Stream tasks
            int endIndex = Math.min(startIndex + pageSize, sortedTasks.size());
            for (int i = startIndex; i < endIndex; i++) {
                responseObserver.onNext(sortedTasks.get(i));
            }
            
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Error listing tasks", e);
            responseObserver.onError(Status.INTERNAL
                    .withDescription("Error listing tasks")
                    .asException());
        }
    }
    
    @Override
    public void getTask(GetTaskRequest request, StreamObserver<Task> responseObserver) {
        Task task = tasks.get(request.getId());
        
        if (task == null) {
            responseObserver.onError(Status.NOT_FOUND
                    .withDescription("Task with ID " + request.getId() + " not found")
                    .asException());
        } else {
            responseObserver.onNext(task);
            responseObserver.onCompleted();
        }
    }
    
    @Override
    public void createTask(CreateTaskRequest request, StreamObserver<Task> responseObserver) {
        try {
            Task.Builder taskBuilder = request.getTask().toBuilder();
            
            // Generate ID and set timestamps
            taskBuilder.setId(UUID.randomUUID().toString());
            
            Timestamp now = Timestamp.newBuilder()
                    .setSeconds(Instant.now().getEpochSecond())
                    .build();
            taskBuilder.setCreatedAt(now);
            taskBuilder.setUpdatedAt(now);
            
            // Set defaults
            if (taskBuilder.getCreatedBy().isEmpty()) {
                taskBuilder.setCreatedBy("system");
            }
            
            // Validate
            if (taskBuilder.getTitle().isEmpty()) {
                responseObserver.onError(Status.INVALID_ARGUMENT
                        .withDescription("Title is required")
                        .asException());
                return;
            }
            
            if (taskBuilder.getTitle().length() > 200) {
                responseObserver.onError(Status.INVALID_ARGUMENT
                        .withDescription("Title must be 200 characters or less")
                        .asException());
                return;
            }
            
            Task task = taskBuilder.build();
            tasks.put(task.getId(), task);
            
            responseObserver.onNext(task);
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Error creating task", e);
            responseObserver.onError(Status.INTERNAL
                    .withDescription("Error creating task")
                    .asException());
        }
    }
    
    @Override
    public void updateTask(UpdateTaskRequest request, StreamObserver<Task> responseObserver) {
        String taskId = request.getTask().getId();
        
        if (taskId.isEmpty()) {
            responseObserver.onError(Status.INVALID_ARGUMENT
                    .withDescription("Task ID is required")
                    .asException());
            return;
        }
        
        Task existingTask = tasks.get(taskId);
        
        if (existingTask == null) {
            responseObserver.onError(Status.NOT_FOUND
                    .withDescription("Task with ID " + taskId + " not found")
                    .asException());
            return;
        }
        
        try {
            Task.Builder updatedBuilder = existingTask.toBuilder();
            
            // Apply updates based on update_mask
            for (String field : request.getUpdateMaskList()) {
                switch (field) {
                    case "title":
                        updatedBuilder.setTitle(request.getTask().getTitle());
                        break;
                    case "description":
                        updatedBuilder.setDescription(request.getTask().getDescription());
                        break;
                    case "status":
                        updatedBuilder.setStatus(request.getTask().getStatus());
                        break;
                    case "priority":
                        updatedBuilder.setPriority(request.getTask().getPriority());
                        break;
                    case "tags":
                        updatedBuilder.clearTags();
                        updatedBuilder.addAllTags(request.getTask().getTagsList());
                        break;
                    case "assigned_to":
                        updatedBuilder.setAssignedTo(request.getTask().getAssignedTo());
                        break;
                    case "due_date":
                        updatedBuilder.setDueDate(request.getTask().getDueDate());
                        break;
                }
            }
            
            // Update timestamp
            Timestamp now = Timestamp.newBuilder()
                    .setSeconds(Instant.now().getEpochSecond())
                    .build();
            updatedBuilder.setUpdatedAt(now);
            
            Task updatedTask = updatedBuilder.build();
            tasks.put(taskId, updatedTask);
            
            responseObserver.onNext(updatedTask);
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Error updating task", e);
            responseObserver.onError(Status.INTERNAL
                    .withDescription("Error updating task")
                    .asException());
        }
    }
    
    @Override
    public void deleteTask(DeleteTaskRequest request, StreamObserver<Empty> responseObserver) {
        if (!tasks.containsKey(request.getId())) {
            responseObserver.onError(Status.NOT_FOUND
                    .withDescription("Task with ID " + request.getId() + " not found")
                    .asException());
        } else {
            tasks.remove(request.getId());
            responseObserver.onNext(Empty.getDefaultInstance());
            responseObserver.onCompleted();
        }
    }
    
    @Override
    public StreamObserver<WatchTasksRequest> watchTasks(StreamObserver<TaskEvent> responseObserver) {
        return new StreamObserver<WatchTasksRequest>() {
            @Override
            public void onNext(WatchTasksRequest request) {
                try {
                    if (request.getWatchAll()) {
                        // Return all tasks as events
                        for (Task task : tasks.values()) {
                            TaskEvent event = TaskEvent.newBuilder()
                                    .setEventType(TaskEvent.EventType.UPDATED)
                                    .setTask(task)
                                    .setTimestamp(Timestamp.newBuilder()
                                            .setSeconds(Instant.now().getEpochSecond())
                                            .build())
                                    .build();
                            responseObserver.onNext(event);
                        }
                    } else if (!request.getTaskIdsList().isEmpty()) {
                        // Return specific tasks
                        for (String taskId : request.getTaskIdsList()) {
                            Task task = tasks.get(taskId);
                            if (task != null) {
                                TaskEvent event = TaskEvent.newBuilder()
                                        .setEventType(TaskEvent.EventType.UPDATED)
                                        .setTask(task)
                                        .setTimestamp(Timestamp.newBuilder()
                                                .setSeconds(Instant.now().getEpochSecond())
                                                .build())
                                        .build();
                                responseObserver.onNext(event);
                            }
                        }
                    } else if (!request.getAssignedTo().isEmpty()) {
                        // Return tasks assigned to specific user
                        for (Task task : tasks.values()) {
                            if (request.getAssignedTo().equals(task.getAssignedTo())) {
                                TaskEvent event = TaskEvent.newBuilder()
                                        .setEventType(TaskEvent.EventType.UPDATED)
                                        .setTask(task)
                                        .setTimestamp(Timestamp.newBuilder()
                                                .setSeconds(Instant.now().getEpochSecond())
                                                .build())
                                        .build();
                                responseObserver.onNext(event);
                            }
                        }
                    }
                } catch (Exception e) {
                    log.error("Error in watch stream", e);
                    responseObserver.onError(Status.INTERNAL
                            .withDescription("Error processing watch request")
                            .asException());
                }
            }
            
            @Override
            public void onError(Throwable t) {
                log.error("Error in watch stream", t);
            }
            
            @Override
            public void onCompleted() {
                responseObserver.onCompleted();
            }
        };
    }
    
    private List<Task> filterTasks(ListTasksRequest request) {
        // Implementation after proto generation
        return new ArrayList<>(tasks.values());
    }
    
    private List<Task> sortTasks(List<Task> tasks, SortOrder sortOrder) {
        // Implementation after proto generation
        return tasks;
    }
    */
    
    // Note: This is a template implementation. 
    // Full implementation requires proto file compilation first.
    // Run: mvn compile to generate the proto classes
}