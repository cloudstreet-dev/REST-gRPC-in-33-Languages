package com.taskapi.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;

@Slf4j
public class TaskApiClient {
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final String baseUrl;
    private static final MediaType JSON = MediaType.get("application/json; charset=utf-8");
    
    public TaskApiClient(String baseUrl) {
        this.baseUrl = baseUrl.endsWith("/") ? baseUrl.substring(0, baseUrl.length() - 1) : baseUrl;
        
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(10, TimeUnit.SECONDS)
                .writeTimeout(10, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build();
        
        this.objectMapper = new ObjectMapper();
        this.objectMapper.registerModule(new JavaTimeModule());
        this.objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }
    
    public List<Map<String, Object>> listTasks(Integer pageSize, String pageToken, 
                                                String status, String assignedTo, 
                                                List<String> tags, String sortOrder) throws IOException {
        HttpUrl.Builder urlBuilder = HttpUrl.parse(baseUrl + "/api/v1/tasks").newBuilder();
        
        if (pageSize != null) {
            urlBuilder.addQueryParameter("pageSize", String.valueOf(pageSize));
        }
        if (pageToken != null) {
            urlBuilder.addQueryParameter("pageToken", pageToken);
        }
        if (status != null) {
            urlBuilder.addQueryParameter("status", status);
        }
        if (assignedTo != null) {
            urlBuilder.addQueryParameter("assignedTo", assignedTo);
        }
        if (tags != null && !tags.isEmpty()) {
            urlBuilder.addQueryParameter("tags", String.join(",", tags));
        }
        if (sortOrder != null) {
            urlBuilder.addQueryParameter("sortOrder", sortOrder);
        }
        
        Request request = new Request.Builder()
                .url(urlBuilder.build())
                .get()
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Unexpected response code: " + response);
            }
            
            Map<String, Object> result = objectMapper.readValue(response.body().string(), Map.class);
            return (List<Map<String, Object>>) result.get("items");
        }
    }
    
    public Map<String, Object> getTask(String taskId) throws IOException {
        Request request = new Request.Builder()
                .url(baseUrl + "/api/v1/tasks/" + taskId)
                .get()
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (response.code() == 404) {
                return null;
            }
            if (!response.isSuccessful()) {
                throw new IOException("Unexpected response code: " + response);
            }
            
            return objectMapper.readValue(response.body().string(), Map.class);
        }
    }
    
    public Map<String, Object> createTask(String title, String description, String priority,
                                           List<String> tags, String assignedTo) throws IOException {
        Map<String, Object> task = new HashMap<>();
        task.put("title", title);
        if (description != null) task.put("description", description);
        if (priority != null) task.put("priority", priority.toUpperCase());
        if (tags != null) task.put("tags", tags);
        if (assignedTo != null) task.put("assignedTo", assignedTo);
        
        String json = objectMapper.writeValueAsString(task);
        RequestBody body = RequestBody.create(json, JSON);
        
        Request request = new Request.Builder()
                .url(baseUrl + "/api/v1/tasks")
                .post(body)
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Failed to create task: " + response);
            }
            
            return objectMapper.readValue(response.body().string(), Map.class);
        }
    }
    
    public Map<String, Object> updateTask(String taskId, Map<String, Object> updates) throws IOException {
        String json = objectMapper.writeValueAsString(updates);
        RequestBody body = RequestBody.create(json, JSON);
        
        Request request = new Request.Builder()
                .url(baseUrl + "/api/v1/tasks/" + taskId)
                .put(body)
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (response.code() == 404) {
                return null;
            }
            if (!response.isSuccessful()) {
                throw new IOException("Failed to update task: " + response);
            }
            
            return objectMapper.readValue(response.body().string(), Map.class);
        }
    }
    
    public Map<String, Object> updateTaskStatus(String taskId, String status) throws IOException {
        Map<String, String> statusUpdate = new HashMap<>();
        statusUpdate.put("status", status.toUpperCase());
        
        String json = objectMapper.writeValueAsString(statusUpdate);
        RequestBody body = RequestBody.create(json, JSON);
        
        Request request = new Request.Builder()
                .url(baseUrl + "/api/v1/tasks/" + taskId + "/status")
                .patch(body)
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (response.code() == 404) {
                return null;
            }
            if (!response.isSuccessful()) {
                throw new IOException("Failed to update task status: " + response);
            }
            
            return objectMapper.readValue(response.body().string(), Map.class);
        }
    }
    
    public boolean deleteTask(String taskId) throws IOException {
        Request request = new Request.Builder()
                .url(baseUrl + "/api/v1/tasks/" + taskId)
                .delete()
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            return response.code() == 204;
        }
    }
    
    public void close() {
        httpClient.dispatcher().executorService().shutdown();
        httpClient.connectionPool().evictAll();
    }
    
    public static void main(String[] args) {
        TaskApiClient client = new TaskApiClient("http://localhost:8080");
        
        try {
            // Create a task
            log.info("Creating a new task...");
            Map<String, Object> newTask = client.createTask(
                    "Test Java REST Client",
                    "Testing the Java REST client implementation",
                    "HIGH",
                    Arrays.asList("test", "java", "rest"),
                    "dev-team"
            );
            log.info("Created task: {}", newTask);
            
            String taskId = (String) newTask.get("id");
            
            // Get the task
            log.info("\nRetrieving task {}...", taskId);
            Map<String, Object> task = client.getTask(taskId);
            log.info("Retrieved task: {}", task);
            
            // Update task status
            log.info("\nUpdating task status to IN_PROGRESS...");
            Map<String, Object> updated = client.updateTaskStatus(taskId, "IN_PROGRESS");
            log.info("Updated task: {}", updated);
            
            // List all tasks
            log.info("\nListing all tasks...");
            List<Map<String, Object>> tasks = client.listTasks(10, null, null, null, null, "created_desc");
            for (Map<String, Object> t : tasks) {
                log.info("[{}] {} - {}", t.get("status"), t.get("title"), t.get("id"));
            }
            
            // Delete the task
            log.info("\nDeleting task {}...", taskId);
            boolean deleted = client.deleteTask(taskId);
            log.info("Task deleted: {}", deleted);
            
        } catch (IOException e) {
            log.error("Error communicating with API", e);
        } finally {
            client.close();
        }
    }
}