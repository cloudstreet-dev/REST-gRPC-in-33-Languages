package com.taskapi.grpc.client;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.StatusRuntimeException;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;
// Import generated classes (will be available after proto compilation)
// import com.taskapi.grpc.generated.*;

import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

@Slf4j
public class TaskGrpcClient {
    private final ManagedChannel channel;
    // private final TaskServiceGrpc.TaskServiceBlockingStub blockingStub;
    // private final TaskServiceGrpc.TaskServiceStub asyncStub;
    
    public TaskGrpcClient(String host, int port) {
        this(ManagedChannelBuilder.forAddress(host, port)
                .usePlaintext()
                .build());
    }
    
    public TaskGrpcClient(ManagedChannel channel) {
        this.channel = channel;
        // this.blockingStub = TaskServiceGrpc.newBlockingStub(channel);
        // this.asyncStub = TaskServiceGrpc.newStub(channel);
    }
    
    public void shutdown() throws InterruptedException {
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS);
    }
    
    /*
    public List<Task> listTasks(int pageSize, String pageToken, TaskStatus status,
                                 String assignedTo, List<String> tags, SortOrder sortOrder) {
        ListTasksRequest.Builder requestBuilder = ListTasksRequest.newBuilder()
                .setPageSize(pageSize);
        
        if (pageToken != null && !pageToken.isEmpty()) {
            requestBuilder.setPageToken(pageToken);
        }
        if (status != null) {
            requestBuilder.setStatus(status);
        }
        if (assignedTo != null && !assignedTo.isEmpty()) {
            requestBuilder.setAssignedTo(assignedTo);
        }
        if (tags != null && !tags.isEmpty()) {
            requestBuilder.addAllTags(tags);
        }
        if (sortOrder != null) {
            requestBuilder.setSortOrder(sortOrder);
        }
        
        List<Task> tasks = new ArrayList<>();
        try {
            Iterator<Task> response = blockingStub.listTasks(requestBuilder.build());
            while (response.hasNext()) {
                tasks.add(response.next());
            }
        } catch (StatusRuntimeException e) {
            log.error("RPC failed: {}", e.getStatus());
        }
        
        return tasks;
    }
    
    public Task getTask(String taskId) {
        GetTaskRequest request = GetTaskRequest.newBuilder()
                .setId(taskId)
                .build();
        
        try {
            return blockingStub.getTask(request);
        } catch (StatusRuntimeException e) {
            log.error("RPC failed: {}", e.getStatus());
            return null;
        }
    }
    
    public Task createTask(String title, String description, TaskPriority priority,
                           List<String> tags, String assignedTo) {
        Task.Builder taskBuilder = Task.newBuilder()
                .setTitle(title);
        
        if (description != null) {
            taskBuilder.setDescription(description);
        }
        if (priority != null) {
            taskBuilder.setPriority(priority);
        }
        if (tags != null) {
            taskBuilder.addAllTags(tags);
        }
        if (assignedTo != null) {
            taskBuilder.setAssignedTo(assignedTo);
        }
        
        CreateTaskRequest request = CreateTaskRequest.newBuilder()
                .setTask(taskBuilder.build())
                .build();
        
        try {
            return blockingStub.createTask(request);
        } catch (StatusRuntimeException e) {
            log.error("RPC failed: {}", e.getStatus());
            return null;
        }
    }
    
    public Task updateTask(String taskId, String title, String description,
                           TaskStatus status, TaskPriority priority,
                           List<String> tags, String assignedTo) {
        Task.Builder taskBuilder = Task.newBuilder()
                .setId(taskId);
        
        List<String> updateMask = new ArrayList<>();
        
        if (title != null) {
            taskBuilder.setTitle(title);
            updateMask.add("title");
        }
        if (description != null) {
            taskBuilder.setDescription(description);
            updateMask.add("description");
        }
        if (status != null) {
            taskBuilder.setStatus(status);
            updateMask.add("status");
        }
        if (priority != null) {
            taskBuilder.setPriority(priority);
            updateMask.add("priority");
        }
        if (tags != null) {
            taskBuilder.addAllTags(tags);
            updateMask.add("tags");
        }
        if (assignedTo != null) {
            taskBuilder.setAssignedTo(assignedTo);
            updateMask.add("assigned_to");
        }
        
        UpdateTaskRequest request = UpdateTaskRequest.newBuilder()
                .setTask(taskBuilder.build())
                .addAllUpdateMask(updateMask)
                .build();
        
        try {
            return blockingStub.updateTask(request);
        } catch (StatusRuntimeException e) {
            log.error("RPC failed: {}", e.getStatus());
            return null;
        }
    }
    
    public boolean deleteTask(String taskId) {
        DeleteTaskRequest request = DeleteTaskRequest.newBuilder()
                .setId(taskId)
                .build();
        
        try {
            blockingStub.deleteTask(request);
            return true;
        } catch (StatusRuntimeException e) {
            log.error("RPC failed: {}", e.getStatus());
            return false;
        }
    }
    
    public void watchTasks(boolean watchAll, List<String> taskIds, String assignedTo,
                           StreamObserver<TaskEvent> responseObserver) {
        StreamObserver<WatchTasksRequest> requestObserver = asyncStub.watchTasks(responseObserver);
        
        try {
            WatchTasksRequest.Builder requestBuilder = WatchTasksRequest.newBuilder()
                    .setWatchAll(watchAll);
            
            if (taskIds != null && !taskIds.isEmpty()) {
                requestBuilder.addAllTaskIds(taskIds);
            }
            if (assignedTo != null && !assignedTo.isEmpty()) {
                requestBuilder.setAssignedTo(assignedTo);
            }
            
            requestObserver.onNext(requestBuilder.build());
            
            // Keep the stream open for continuous updates
            // In a real application, you would manage this lifecycle appropriately
            
        } catch (Exception e) {
            log.error("Error in watch stream", e);
            requestObserver.onError(e);
        }
    }
    */
    
    public static void main(String[] args) throws InterruptedException {
        TaskGrpcClient client = new TaskGrpcClient("localhost", 50051);
        
        try {
            log.info("Note: This is a template. Run 'mvn compile' first to generate proto classes.");
            
            // Example usage (will work after proto generation):
            
            /*
            // Create a task
            log.info("Creating a new task...");
            Task newTask = client.createTask(
                    "Test Java gRPC Client",
                    "Testing the Java gRPC client implementation",
                    TaskPriority.HIGH,
                    Arrays.asList("test", "java", "grpc"),
                    "dev-team"
            );
            if (newTask != null) {
                log.info("Created task: {} - {}", newTask.getId(), newTask.getTitle());
                
                // Get the task
                log.info("\nRetrieving task {}...", newTask.getId());
                Task task = client.getTask(newTask.getId());
                if (task != null) {
                    log.info("Retrieved task: {} - Status: {}", task.getTitle(), task.getStatus());
                }
                
                // Update task status
                log.info("\nUpdating task status to IN_PROGRESS...");
                Task updated = client.updateTask(
                        newTask.getId(),
                        null,
                        null,
                        TaskStatus.IN_PROGRESS,
                        null,
                        null,
                        null
                );
                if (updated != null) {
                    log.info("Updated task status: {}", updated.getStatus());
                }
                
                // List all tasks
                log.info("\nListing all tasks...");
                List<Task> tasks = client.listTasks(10, null, null, null, null, SortOrder.CREATED_DESC);
                for (Task t : tasks) {
                    log.info("[{}] {} - {}", t.getStatus(), t.getTitle(), t.getId());
                }
                
                // Watch for task changes
                log.info("\nWatching for task changes...");
                CountDownLatch latch = new CountDownLatch(1);
                client.watchTasks(true, null, null, new StreamObserver<TaskEvent>() {
                    @Override
                    public void onNext(TaskEvent event) {
                        log.info("Event: {} - Task: {}", 
                                event.getEventType(), 
                                event.getTask().getTitle());
                    }
                    
                    @Override
                    public void onError(Throwable t) {
                        log.error("Watch error", t);
                        latch.countDown();
                    }
                    
                    @Override
                    public void onCompleted() {
                        log.info("Watch completed");
                        latch.countDown();
                    }
                });
                
                // Wait for a bit to receive events
                latch.await(5, TimeUnit.SECONDS);
                
                // Delete the task
                log.info("\nDeleting task {}...", newTask.getId());
                boolean deleted = client.deleteTask(newTask.getId());
                log.info("Task deleted: {}", deleted);
            }
            */
            
        } finally {
            client.shutdown();
        }
    }
}