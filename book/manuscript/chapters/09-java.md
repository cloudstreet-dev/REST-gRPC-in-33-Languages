# Chapter 9: Java - Enterprise Strength with Spring Boot and gRPC

## Introduction

Java, introduced by Sun Microsystems in 1995, has become one of the most widely adopted programming languages in enterprise software development. Its "write once, run anywhere" philosophy, powered by the Java Virtual Machine (JVM), has made it a cornerstone of large-scale, distributed systems. With the evolution to modern frameworks like Spring Boot and native gRPC support, Java continues to excel in building robust, scalable APIs.

In this chapter, we'll implement our Task Management API using Spring Boot for REST and native gRPC libraries, showcasing Java's strengths in enterprise API development.

## Why Java for APIs?

Java offers several compelling advantages for API development:

1. **Mature Ecosystem**: Extensive libraries, frameworks, and tools refined over decades
2. **Enterprise Ready**: Built-in support for transactions, security, and scalability
3. **Strong Type System**: Compile-time type checking prevents runtime errors
4. **JVM Performance**: Just-In-Time compilation and sophisticated garbage collection
5. **Spring Boot**: Convention-over-configuration framework for rapid development
6. **Native gRPC Support**: First-class support for Protocol Buffers and gRPC
7. **Excellent Tooling**: IDEs like IntelliJ IDEA and Eclipse provide powerful development experiences

## Setting Up the Development Environment

### Installing Java

For this project, we'll use Java 17 (LTS):

```bash
# macOS with Homebrew
brew install openjdk@17

# Ubuntu/Debian
sudo apt update
sudo apt install openjdk-17-jdk

# Windows with Chocolatey
choco install openjdk17

# Verify installation
java -version
javac -version
```

### Installing Maven

Maven is our build tool of choice:

```bash
# macOS with Homebrew
brew install maven

# Ubuntu/Debian
sudo apt install maven

# Windows with Chocolatey
choco install maven

# Verify installation
mvn -version
```

### Project Structure

```
code/java/
├── rest/
│   ├── server/
│   │   ├── pom.xml
│   │   └── src/main/java/com/taskapi/
│   │       ├── TaskApiApplication.java
│   │       ├── controller/
│   │       │   └── TaskController.java
│   │       ├── model/
│   │       │   ├── Task.java
│   │       │   ├── TaskStatus.java
│   │       │   ├── TaskPriority.java
│   │       │   └── PageResponse.java
│   │       ├── service/
│   │       │   └── TaskService.java
│   │       └── config/
│   │           └── OpenApiConfig.java
│   └── client/
│       ├── pom.xml
│       └── src/main/java/com/taskapi/client/
│           └── TaskApiClient.java
└── grpc/
    ├── server/
    │   ├── pom.xml
    │   └── src/main/java/com/taskapi/grpc/
    │       ├── TaskGrpcServer.java
    │       └── TaskServiceImpl.java
    └── client/
        ├── pom.xml
        └── src/main/java/com/taskapi/grpc/client/
            └── TaskGrpcClient.java
```

## Implementing the REST API with Spring Boot

### Maven Configuration

Spring Boot simplifies dependency management through its starter packages:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
        <relativePath/>
    </parent>
    
    <groupId>com.taskapi</groupId>
    <artifactId>task-rest-server</artifactId>
    <version>1.0.0</version>
    
    <properties>
        <java.version>17</java.version>
    </properties>
    
    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-validation</artifactId>
        </dependency>
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>
        <dependency>
            <groupId>org.springdoc</groupId>
            <artifactId>springdoc-openapi-starter-webmvc-ui</artifactId>
            <version>2.3.0</version>
        </dependency>
    </dependencies>
</project>
```

### Domain Models

Java's strong typing with Lombok annotations for boilerplate reduction:

```java
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Task {
    private String id;
    
    @NotBlank(message = "Title is required")
    @Size(max = 200, message = "Title must be 200 characters or less")
    private String title;
    
    private String description;
    
    @Builder.Default
    private TaskStatus status = TaskStatus.PENDING;
    
    @Builder.Default
    private TaskPriority priority = TaskPriority.MEDIUM;
    
    private List<String> tags;
    private String assignedTo;
    private LocalDateTime dueDate;
    
    @Builder.Default
    private LocalDateTime createdAt = LocalDateTime.now();
    
    @Builder.Default
    private LocalDateTime updatedAt = LocalDateTime.now();
    
    private String createdBy;
    private String updatedBy;
}
```

### Service Layer

Business logic encapsulated in service classes:

```java
@Service
public class TaskService {
    private final Map<String, Task> tasks = new ConcurrentHashMap<>();
    
    public PageResponse<Task> listTasks(int pageSize, String pageToken, 
                                         TaskStatus status, String assignedTo,
                                         List<String> tags, String sortOrder) {
        List<Task> filteredTasks = tasks.values().stream()
                .filter(task -> status == null || task.getStatus() == status)
                .filter(task -> assignedTo == null || 
                        assignedTo.equals(task.getAssignedTo()))
                .filter(task -> tags == null || tags.isEmpty() || 
                        (task.getTags() != null && 
                         task.getTags().containsAll(tags)))
                .collect(Collectors.toList());
        
        // Apply sorting
        if ("priority_desc".equals(sortOrder)) {
            filteredTasks.sort((a, b) -> 
                b.getPriority().compareTo(a.getPriority()));
        }
        
        // Pagination logic
        int startIndex = parsePageToken(pageToken);
        int endIndex = Math.min(startIndex + pageSize, filteredTasks.size());
        
        return PageResponse.<Task>builder()
                .items(filteredTasks.subList(startIndex, endIndex))
                .pageSize(pageSize)
                .nextPageToken(generateNextToken(endIndex, filteredTasks.size()))
                .totalCount(filteredTasks.size())
                .build();
    }
    
    public Task createTask(Task task) {
        if (task.getId() == null) {
            task.setId(UUID.randomUUID().toString());
        }
        task.setCreatedAt(LocalDateTime.now());
        task.setUpdatedAt(LocalDateTime.now());
        
        tasks.put(task.getId(), task);
        return task;
    }
}
```

### REST Controller

Spring's declarative approach with annotations:

```java
@RestController
@RequestMapping("/api/v1/tasks")
@RequiredArgsConstructor
@Tag(name = "Tasks", description = "Task management endpoints")
@CrossOrigin(origins = "*")
public class TaskController {
    
    private final TaskService taskService;
    
    @GetMapping
    @Operation(summary = "List all tasks", 
               description = "Retrieve a paginated list of tasks with optional filters")
    public ResponseEntity<PageResponse<Task>> listTasks(
            @RequestParam(defaultValue = "20") int pageSize,
            @RequestParam(required = false) String pageToken,
            @RequestParam(required = false) TaskStatus status,
            @RequestParam(required = false) String assignedTo,
            @RequestParam(required = false) String tags,
            @RequestParam(required = false) String sortOrder) {
        
        List<String> tagList = tags != null ? 
            Arrays.asList(tags.split(",")) : null;
        
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
    
    @PostMapping
    @Operation(summary = "Create a task", 
               description = "Create a new task")
    @ApiResponse(responseCode = "201", description = "Task created successfully")
    @ApiResponse(responseCode = "400", description = "Invalid request data")
    public ResponseEntity<Task> createTask(@Valid @RequestBody Task task) {
        Task created = taskService.createTask(task);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }
    
    @PutMapping("/{id}")
    public ResponseEntity<Task> updateTask(
            @PathVariable String id,
            @Valid @RequestBody Task task) {
        return taskService.updateTask(id, task)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
    
    @PatchMapping("/{id}/status")
    public ResponseEntity<Task> updateTaskStatus(
            @PathVariable String id,
            @RequestBody Map<String, String> statusUpdate) {
        
        try {
            TaskStatus status = TaskStatus.valueOf(
                statusUpdate.get("status").toUpperCase());
            return taskService.updateTaskStatus(id, status)
                    .map(ResponseEntity::ok)
                    .orElse(ResponseEntity.notFound().build());
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().build();
        }
    }
    
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteTask(@PathVariable String id) {
        if (taskService.deleteTask(id)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.notFound().build();
    }
}
```

### Configuration

Spring Boot's application configuration:

```yaml
server:
  port: 8080
  error:
    include-message: always
    include-binding-errors: always

spring:
  application:
    name: task-api-server
  jackson:
    default-property-inclusion: non_null
    serialization:
      write-dates-as-timestamps: false

springdoc:
  api-docs:
    path: /api-docs
  swagger-ui:
    path: /swagger-ui
    operations-sorter: method
    tags-sorter: alpha

management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics
```

### Running the REST Server

```bash
cd code/java/rest/server
mvn spring-boot:run

# Or build and run JAR
mvn clean package
java -jar target/task-rest-server-1.0.0.jar

# Access Swagger UI
open http://localhost:8080/swagger-ui
```

## Implementing the REST Client

### OkHttp-based Client

Modern HTTP client with fluent API:

```java
@Slf4j
public class TaskApiClient {
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final String baseUrl;
    
    public TaskApiClient(String baseUrl) {
        this.baseUrl = baseUrl;
        
        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(10, TimeUnit.SECONDS)
                .writeTimeout(10, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build();
        
        this.objectMapper = new ObjectMapper();
        this.objectMapper.registerModule(new JavaTimeModule());
        this.objectMapper.disable(
            SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }
    
    public List<Map<String, Object>> listTasks(
            Integer pageSize, String pageToken, 
            String status, String assignedTo, 
            List<String> tags, String sortOrder) throws IOException {
        
        HttpUrl.Builder urlBuilder = HttpUrl.parse(
            baseUrl + "/api/v1/tasks").newBuilder();
        
        if (pageSize != null) {
            urlBuilder.addQueryParameter("pageSize", 
                String.valueOf(pageSize));
        }
        // Add other parameters...
        
        Request request = new Request.Builder()
                .url(urlBuilder.build())
                .get()
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Unexpected response: " + response);
            }
            
            Map<String, Object> result = objectMapper.readValue(
                response.body().string(), Map.class);
            return (List<Map<String, Object>>) result.get("items");
        }
    }
    
    public Map<String, Object> createTask(
            String title, String description, String priority,
            List<String> tags, String assignedTo) throws IOException {
        
        Map<String, Object> task = new HashMap<>();
        task.put("title", title);
        if (description != null) task.put("description", description);
        if (priority != null) task.put("priority", priority.toUpperCase());
        if (tags != null) task.put("tags", tags);
        if (assignedTo != null) task.put("assignedTo", assignedTo);
        
        String json = objectMapper.writeValueAsString(task);
        RequestBody body = RequestBody.create(json, 
            MediaType.get("application/json; charset=utf-8"));
        
        Request request = new Request.Builder()
                .url(baseUrl + "/api/v1/tasks")
                .post(body)
                .build();
        
        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Failed to create task: " + response);
            }
            
            return objectMapper.readValue(
                response.body().string(), Map.class);
        }
    }
}
```

## Implementing gRPC Services

### Maven Configuration for gRPC

Protocol Buffer compilation and gRPC code generation:

```xml
<dependencies>
    <dependency>
        <groupId>io.grpc</groupId>
        <artifactId>grpc-netty-shaded</artifactId>
        <version>${grpc.version}</version>
    </dependency>
    <dependency>
        <groupId>io.grpc</groupId>
        <artifactId>grpc-protobuf</artifactId>
        <version>${grpc.version}</version>
    </dependency>
    <dependency>
        <groupId>io.grpc</groupId>
        <artifactId>grpc-stub</artifactId>
        <version>${grpc.version}</version>
    </dependency>
</dependencies>

<build>
    <plugins>
        <plugin>
            <groupId>org.xolstice.maven.plugins</groupId>
            <artifactId>protobuf-maven-plugin</artifactId>
            <version>0.6.1</version>
            <configuration>
                <protocArtifact>
                    com.google.protobuf:protoc:${protoc.version}:exe:${os.detected.classifier}
                </protocArtifact>
                <pluginId>grpc-java</pluginId>
                <pluginArtifact>
                    io.grpc:protoc-gen-grpc-java:${grpc.version}:exe:${os.detected.classifier}
                </pluginArtifact>
                <protoSourceRoot>${project.basedir}/../../shared/protos</protoSourceRoot>
            </configuration>
            <executions>
                <execution>
                    <goals>
                        <goal>compile</goal>
                        <goal>compile-custom</goal>
                    </goals>
                </execution>
            </executions>
        </plugin>
    </plugins>
</build>
```

### gRPC Service Implementation

```java
@Slf4j
public class TaskServiceImpl extends TaskServiceGrpc.TaskServiceImplBase {
    
    private final Map<String, Task> tasks = new ConcurrentHashMap<>();
    
    @Override
    public void listTasks(ListTasksRequest request, 
                          StreamObserver<Task> responseObserver) {
        try {
            List<Task> filteredTasks = filterTasks(request);
            List<Task> sortedTasks = sortTasks(
                filteredTasks, request.getSortOrder());
            
            // Apply pagination
            int pageSize = request.getPageSize() > 0 ? 
                request.getPageSize() : 20;
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
            int endIndex = Math.min(startIndex + pageSize, 
                sortedTasks.size());
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
    public void createTask(CreateTaskRequest request, 
                           StreamObserver<Task> responseObserver) {
        try {
            Task.Builder taskBuilder = request.getTask().toBuilder();
            
            // Generate ID and set timestamps
            taskBuilder.setId(UUID.randomUUID().toString());
            
            Timestamp now = Timestamp.newBuilder()
                    .setSeconds(Instant.now().getEpochSecond())
                    .build();
            taskBuilder.setCreatedAt(now);
            taskBuilder.setUpdatedAt(now);
            
            // Validate
            if (taskBuilder.getTitle().isEmpty()) {
                responseObserver.onError(Status.INVALID_ARGUMENT
                        .withDescription("Title is required")
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
    public StreamObserver<WatchTasksRequest> watchTasks(
            StreamObserver<TaskEvent> responseObserver) {
        
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
                                            .setSeconds(Instant.now()
                                                .getEpochSecond())
                                            .build())
                                    .build();
                            responseObserver.onNext(event);
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
}
```

### gRPC Server

```java
@Slf4j
public class TaskGrpcServer {
    private Server server;
    private final int port;
    
    public TaskGrpcServer(int port) {
        this.port = port;
    }
    
    public void start() throws IOException {
        server = ServerBuilder.forPort(port)
                .addService(new TaskServiceImpl())
                .addService(ProtoReflectionService.newInstance())
                .build()
                .start();
        
        log.info("Java gRPC server started on port {}", port);
        
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            log.info("Shutting down gRPC server...");
            try {
                TaskGrpcServer.this.stop();
            } catch (InterruptedException e) {
                log.error("Error during shutdown", e);
            }
            log.info("Server shut down");
        }));
    }
    
    public static void main(String[] args) 
            throws IOException, InterruptedException {
        TaskGrpcServer server = new TaskGrpcServer(50051);
        server.start();
        server.blockUntilShutdown();
    }
}
```

### gRPC Client

```java
@Slf4j
public class TaskGrpcClient {
    private final ManagedChannel channel;
    private final TaskServiceGrpc.TaskServiceBlockingStub blockingStub;
    private final TaskServiceGrpc.TaskServiceStub asyncStub;
    
    public TaskGrpcClient(String host, int port) {
        this(ManagedChannelBuilder.forAddress(host, port)
                .usePlaintext()
                .build());
    }
    
    public TaskGrpcClient(ManagedChannel channel) {
        this.channel = channel;
        this.blockingStub = TaskServiceGrpc.newBlockingStub(channel);
        this.asyncStub = TaskServiceGrpc.newStub(channel);
    }
    
    public List<Task> listTasks(int pageSize, String pageToken, 
                                 TaskStatus status, String assignedTo, 
                                 List<String> tags, SortOrder sortOrder) {
        ListTasksRequest.Builder requestBuilder = 
            ListTasksRequest.newBuilder()
                .setPageSize(pageSize);
        
        if (pageToken != null && !pageToken.isEmpty()) {
            requestBuilder.setPageToken(pageToken);
        }
        if (status != null) {
            requestBuilder.setStatus(status);
        }
        
        List<Task> tasks = new ArrayList<>();
        try {
            Iterator<Task> response = blockingStub.listTasks(
                requestBuilder.build());
            while (response.hasNext()) {
                tasks.add(response.next());
            }
        } catch (StatusRuntimeException e) {
            log.error("RPC failed: {}", e.getStatus());
        }
        
        return tasks;
    }
    
    public void watchTasks(boolean watchAll, List<String> taskIds, 
                           String assignedTo,
                           StreamObserver<TaskEvent> responseObserver) {
        StreamObserver<WatchTasksRequest> requestObserver = 
            asyncStub.watchTasks(responseObserver);
        
        try {
            WatchTasksRequest.Builder requestBuilder = 
                WatchTasksRequest.newBuilder()
                    .setWatchAll(watchAll);
            
            if (taskIds != null && !taskIds.isEmpty()) {
                requestBuilder.addAllTaskIds(taskIds);
            }
            
            requestObserver.onNext(requestBuilder.build());
            
        } catch (Exception e) {
            log.error("Error in watch stream", e);
            requestObserver.onError(e);
        }
    }
}
```

## Advanced Features

### Exception Handling

Global exception handler with Spring:

```java
@RestControllerAdvice
public class GlobalExceptionHandler {
    
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, Object>> handleValidationExceptions(
            MethodArgumentNotValidException ex) {
        
        Map<String, Object> errors = new HashMap<>();
        errors.put("status", "error");
        errors.put("message", "Validation failed");
        
        Map<String, String> fieldErrors = new HashMap<>();
        ex.getBindingResult().getAllErrors().forEach((error) -> {
            String fieldName = ((FieldError) error).getField();
            String errorMessage = error.getDefaultMessage();
            fieldErrors.put(fieldName, errorMessage);
        });
        errors.put("errors", fieldErrors);
        
        return ResponseEntity.badRequest().body(errors);
    }
    
    @ExceptionHandler(TaskNotFoundException.class)
    public ResponseEntity<Map<String, Object>> handleTaskNotFound(
            TaskNotFoundException ex) {
        
        Map<String, Object> error = new HashMap<>();
        error.put("status", "error");
        error.put("message", ex.getMessage());
        
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(error);
    }
}
```

### Security Configuration

Spring Security integration:

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {
    
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) 
            throws Exception {
        http
            .cors().and()
            .csrf().disable()
            .authorizeHttpRequests(authz -> authz
                .requestMatchers("/api/v1/tasks/**").permitAll()
                .requestMatchers("/swagger-ui/**", "/api-docs/**").permitAll()
                .requestMatchers("/actuator/health").permitAll()
                .anyRequest().authenticated()
            )
            .sessionManagement()
                .sessionCreationPolicy(SessionCreationPolicy.STATELESS);
        
        return http.build();
    }
    
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration = new CorsConfiguration();
        configuration.setAllowedOrigins(Arrays.asList("*"));
        configuration.setAllowedMethods(Arrays.asList(
            "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"));
        configuration.setAllowedHeaders(Arrays.asList("*"));
        
        UrlBasedCorsConfigurationSource source = 
            new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }
}
```

### Database Integration

JPA repository pattern:

```java
@Entity
@Table(name = "tasks")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TaskEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String id;
    
    @Column(nullable = false, length = 200)
    private String title;
    
    @Column(columnDefinition = "TEXT")
    private String description;
    
    @Enumerated(EnumType.STRING)
    private TaskStatus status;
    
    @Enumerated(EnumType.STRING)
    private TaskPriority priority;
    
    @ElementCollection
    private List<String> tags;
    
    @Column(name = "assigned_to")
    private String assignedTo;
    
    @Column(name = "due_date")
    private LocalDateTime dueDate;
    
    @CreationTimestamp
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;
    
    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
}

@Repository
public interface TaskRepository extends JpaRepository<TaskEntity, String> {
    
    Page<TaskEntity> findByStatus(TaskStatus status, Pageable pageable);
    
    Page<TaskEntity> findByAssignedTo(String assignedTo, Pageable pageable);
    
    @Query("SELECT t FROM TaskEntity t WHERE :tag MEMBER OF t.tags")
    Page<TaskEntity> findByTag(@Param("tag") String tag, Pageable pageable);
}
```

### Caching

Spring Cache abstraction:

```java
@Service
@CacheConfig(cacheNames = "tasks")
public class TaskService {
    
    @Cacheable(key = "#id")
    public Optional<Task> getTask(String id) {
        return Optional.ofNullable(tasks.get(id));
    }
    
    @CachePut(key = "#result.id")
    public Task createTask(Task task) {
        // Create task logic
        return task;
    }
    
    @CacheEvict(key = "#id")
    public boolean deleteTask(String id) {
        return tasks.remove(id) != null;
    }
    
    @CacheEvict(allEntries = true)
    public void clearCache() {
        // Cache will be cleared
    }
}
```

## Testing

### Unit Testing with JUnit 5

```java
@ExtendWith(MockitoExtension.class)
class TaskServiceTest {
    
    @InjectMocks
    private TaskService taskService;
    
    @Mock
    private TaskRepository taskRepository;
    
    @Test
    void testCreateTask() {
        // Given
        Task task = Task.builder()
                .title("Test Task")
                .description("Test Description")
                .priority(TaskPriority.HIGH)
                .build();
        
        // When
        Task created = taskService.createTask(task);
        
        // Then
        assertNotNull(created.getId());
        assertEquals("Test Task", created.getTitle());
        assertEquals(TaskStatus.PENDING, created.getStatus());
        assertNotNull(created.getCreatedAt());
    }
    
    @Test
    void testListTasksWithFilters() {
        // Given
        taskService.createTask(Task.builder()
                .title("Task 1")
                .status(TaskStatus.IN_PROGRESS)
                .assignedTo("user1")
                .build());
        
        taskService.createTask(Task.builder()
                .title("Task 2")
                .status(TaskStatus.PENDING)
                .assignedTo("user2")
                .build());
        
        // When
        PageResponse<Task> response = taskService.listTasks(
                10, null, TaskStatus.IN_PROGRESS, null, null, null);
        
        // Then
        assertEquals(1, response.getItems().size());
        assertEquals("Task 1", response.getItems().get(0).getTitle());
    }
}
```

### Integration Testing

```java
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
class TaskControllerIntegrationTest {
    
    @Autowired
    private MockMvc mockMvc;
    
    @Autowired
    private ObjectMapper objectMapper;
    
    @Test
    void testCreateAndRetrieveTask() throws Exception {
        // Create task
        Task task = Task.builder()
                .title("Integration Test Task")
                .description("Testing the full flow")
                .priority(TaskPriority.MEDIUM)
                .tags(Arrays.asList("test", "integration"))
                .build();
        
        MvcResult createResult = mockMvc.perform(post("/api/v1/tasks")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(task)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.title").value("Integration Test Task"))
                .andExpect(jsonPath("$.id").isNotEmpty())
                .andReturn();
        
        String taskId = JsonPath.read(
            createResult.getResponse().getContentAsString(), "$.id");
        
        // Retrieve task
        mockMvc.perform(get("/api/v1/tasks/{id}", taskId))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.id").value(taskId))
                .andExpect(jsonPath("$.title").value("Integration Test Task"));
    }
    
    @Test
    void testListTasksWithPagination() throws Exception {
        // Create multiple tasks
        for (int i = 1; i <= 25; i++) {
            Task task = Task.builder()
                    .title("Task " + i)
                    .priority(TaskPriority.MEDIUM)
                    .build();
            
            mockMvc.perform(post("/api/v1/tasks")
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(objectMapper.writeValueAsString(task)))
                    .andExpect(status().isCreated());
        }
        
        // Test pagination
        mockMvc.perform(get("/api/v1/tasks")
                .param("pageSize", "10"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items", hasSize(10)))
                .andExpect(jsonPath("$.nextPageToken").isNotEmpty());
    }
}
```

## Performance Optimization

### Connection Pooling

```java
@Configuration
public class HttpClientConfig {
    
    @Bean
    public OkHttpClient okHttpClient() {
        ConnectionPool connectionPool = new ConnectionPool(
                50, // Max idle connections
                5, TimeUnit.MINUTES // Keep-alive duration
        );
        
        return new OkHttpClient.Builder()
                .connectionPool(connectionPool)
                .connectTimeout(10, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(10, TimeUnit.SECONDS)
                .retryOnConnectionFailure(true)
                .build();
    }
}
```

### Async Processing

```java
@Service
public class AsyncTaskService {
    
    @Async
    public CompletableFuture<Task> processTaskAsync(Task task) {
        // Simulate long-running operation
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        
        // Process task
        task.setStatus(TaskStatus.IN_PROGRESS);
        
        return CompletableFuture.completedFuture(task);
    }
    
    @Async
    public CompletableFuture<List<Task>> batchProcessTasks(
            List<Task> tasks) {
        
        List<CompletableFuture<Task>> futures = tasks.stream()
                .map(this::processTaskAsync)
                .collect(Collectors.toList());
        
        return CompletableFuture.allOf(
                futures.toArray(new CompletableFuture[0]))
                .thenApply(v -> futures.stream()
                        .map(CompletableFuture::join)
                        .collect(Collectors.toList()));
    }
}
```

## Deployment

### Building for Production

```bash
# Build with Maven
cd code/java/rest/server
mvn clean package

# Run tests
mvn test

# Build Docker image
docker build -t task-api-java .

# Run with Docker
docker run -p 8080:8080 -p 50051:50051 task-api-java
```

### Dockerfile

```dockerfile
# Multi-stage build
FROM maven:3.8-openjdk-17 AS build
WORKDIR /app
COPY pom.xml .
RUN mvn dependency:go-offline
COPY src ./src
RUN mvn clean package -DskipTests

# Runtime stage
FROM openjdk:17-jdk-slim
WORKDIR /app
COPY --from=build /app/target/*.jar app.jar

# Add health check
HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
    CMD curl -f http://localhost:8080/actuator/health || exit 1

EXPOSE 8080 50051
ENTRYPOINT ["java", "-jar", "app.jar"]
```

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: task-api-java
spec:
  replicas: 3
  selector:
    matchLabels:
      app: task-api-java
  template:
    metadata:
      labels:
        app: task-api-java
    spec:
      containers:
      - name: task-api
        image: task-api-java:latest
        ports:
        - containerPort: 8080
          name: http
        - containerPort: 50051
          name: grpc
        env:
        - name: SPRING_PROFILES_ACTIVE
          value: "production"
        - name: JAVA_OPTS
          value: "-Xmx512m -Xms256m"
        resources:
          requests:
            memory: "512Mi"
            cpu: "250m"
          limits:
            memory: "1Gi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /actuator/health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /actuator/health/readiness
            port: 8080
          initialDelaySeconds: 20
          periodSeconds: 5
```

## Best Practices

### 1. Use Dependency Injection

```java
@Component
@RequiredArgsConstructor
public class TaskValidator {
    private final TaskRepository repository;
    
    public void validate(Task task) {
        if (task.getTitle() == null || task.getTitle().trim().isEmpty()) {
            throw new ValidationException("Title is required");
        }
        
        if (task.getDueDate() != null && 
            task.getDueDate().isBefore(LocalDateTime.now())) {
            throw new ValidationException("Due date must be in the future");
        }
    }
}
```

### 2. Use Proper Logging

```java
@Slf4j
@Service
public class TaskService {
    
    public Task createTask(Task task) {
        log.debug("Creating task with title: {}", task.getTitle());
        
        try {
            Task created = processTask(task);
            log.info("Task created successfully with ID: {}", created.getId());
            return created;
        } catch (Exception e) {
            log.error("Failed to create task", e);
            throw new TaskCreationException("Task creation failed", e);
        }
    }
}
```

### 3. Implement Circuit Breakers

```java
@Component
public class ExternalServiceClient {
    private final CircuitBreaker circuitBreaker;
    
    public ExternalServiceClient() {
        this.circuitBreaker = CircuitBreaker.ofDefaults("external-service");
        circuitBreaker.getEventPublisher()
            .onStateTransition(event -> 
                log.info("Circuit breaker state transition: {}", event));
    }
    
    public Optional<String> callExternalService(String input) {
        return Try.ofSupplier(
            CircuitBreaker.decorateSupplier(circuitBreaker, 
                () -> makeExternalCall(input)))
            .recover(throwable -> {
                log.error("External service call failed", throwable);
                return null;
            })
            .toJavaOptional();
    }
}
```

## Conclusion

Java with Spring Boot provides a mature, enterprise-ready platform for building REST APIs, while native gRPC support offers excellent performance for service-to-service communication. The combination of strong typing, extensive tooling, and a vast ecosystem makes Java an excellent choice for building scalable, maintainable APIs.

Key takeaways:
- Spring Boot's convention-over-configuration accelerates development
- Annotations provide clean, declarative programming model
- Strong type system catches errors at compile time
- Excellent IDE support enhances developer productivity
- JVM performance and garbage collection handle high loads
- Mature ecosystem provides solutions for most requirements
- Native gRPC support enables efficient microservices communication

Java continues to evolve with features like records, pattern matching, and virtual threads, ensuring it remains relevant for modern API development while maintaining its enterprise strengths.