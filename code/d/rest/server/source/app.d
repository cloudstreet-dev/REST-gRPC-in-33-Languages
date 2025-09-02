import vibe.vibe;
import std.uuid;
import std.datetime;
import std.algorithm;
import std.array;
import std.conv;
import std.string;
import core.sync.mutex;

// Task status enum
enum TaskStatus : string {
    pending = "pending",
    inProgress = "in_progress",
    completed = "completed",
    cancelled = "cancelled"
}

// Task priority enum
enum TaskPriority : string {
    low = "low",
    medium = "medium",
    high = "high",
    urgent = "urgent"
}

// Task model
struct Task {
    string id;
    string title;
    string description;
    TaskStatus status = TaskStatus.pending;
    TaskPriority priority = TaskPriority.medium;
    string[] tags;
    string assignedTo;
    SysTime createdAt;
    SysTime updatedAt;
    
    // Constructor for new tasks
    this(string title) {
        this.id = randomUUID().toString();
        this.title = title;
        this.createdAt = Clock.currTime(UTC());
        this.updatedAt = this.createdAt;
    }
    
    // Convert to JSON-compatible dictionary
    Json toJson() const {
        Json json = Json.emptyObject;
        json["id"] = id;
        json["title"] = title;
        json["description"] = description;
        json["status"] = cast(string)status;
        json["priority"] = cast(string)priority;
        json["tags"] = Json.emptyArray;
        foreach (tag; tags) {
            json["tags"] ~= Json(tag);
        }
        json["assigned_to"] = assignedTo;
        json["created_at"] = createdAt.toISOExtString();
        json["updated_at"] = updatedAt.toISOExtString();
        return json;
    }
    
    // Update from JSON
    void updateFromJson(Json json) {
        if ("title" in json && json["title"].type == Json.Type.string_)
            title = json["title"].get!string;
        if ("description" in json && json["description"].type == Json.Type.string_)
            description = json["description"].get!string;
        if ("status" in json && json["status"].type == Json.Type.string_)
            status = json["status"].get!string.to!TaskStatus;
        if ("priority" in json && json["priority"].type == Json.Type.string_)
            priority = json["priority"].get!string.to!TaskPriority;
        if ("tags" in json && json["tags"].type == Json.Type.array) {
            tags = [];
            foreach (tag; json["tags"]) {
                if (tag.type == Json.Type.string_)
                    tags ~= tag.get!string;
            }
        }
        if ("assigned_to" in json && json["assigned_to"].type == Json.Type.string_)
            assignedTo = json["assigned_to"].get!string;
        
        updatedAt = Clock.currTime(UTC());
    }
}

// Task Repository with thread-safe operations
class TaskRepository {
    private Task[] tasks;
    private Mutex mutex;
    
    this() {
        mutex = new Mutex();
        loadSampleData();
    }
    
    private void loadSampleData() {
        // Sample task 1
        auto task1 = Task("Implement D REST API");
        task1.description = "Build a REST API server using Vibe.d framework";
        task1.status = TaskStatus.inProgress;
        task1.priority = TaskPriority.high;
        task1.tags = ["d", "rest", "api"];
        task1.assignedTo = "systems-team";
        tasks ~= task1;
        
        // Sample task 2
        auto task2 = Task("Add gRPC support");
        task2.description = "Implement gRPC server and client for D";
        task2.status = TaskStatus.pending;
        task2.priority = TaskPriority.medium;
        task2.tags = ["d", "grpc", "protobuf"];
        task2.assignedTo = "backend-team";
        tasks ~= task2;
        
        // Sample task 3
        auto task3 = Task("Write unit tests");
        task3.description = "Add comprehensive test coverage using D's built-in unittest";
        task3.status = TaskStatus.pending;
        task3.priority = TaskPriority.high;
        task3.tags = ["testing", "quality"];
        task3.assignedTo = "qa-team";
        tasks ~= task3;
    }
    
    Task[] listTasks(string status = null, string assignedTo = null, string[] tags = null,
                     int pageSize = 20, string pageToken = null, 
                     string sortBy = "created_at", string sortOrder = "desc") {
        synchronized (mutex) {
            Task[] filtered = tasks.dup;
            
            // Filter by status
            if (status !is null && status.length > 0) {
                filtered = filtered.filter!(t => t.status == status.to!TaskStatus).array;
            }
            
            // Filter by assignedTo
            if (assignedTo !is null && assignedTo.length > 0) {
                filtered = filtered.filter!(t => t.assignedTo == assignedTo).array;
            }
            
            // Filter by tags
            if (tags !is null && tags.length > 0) {
                filtered = filtered.filter!(t => {
                    foreach (tag; tags) {
                        if (!t.tags.canFind(tag))
                            return false;
                    }
                    return true;
                }).array;
            }
            
            // Sort
            if (sortBy == "title") {
                if (sortOrder == "asc")
                    filtered.sort!((a, b) => a.title < b.title);
                else
                    filtered.sort!((a, b) => a.title > b.title);
            } else if (sortBy == "updated_at") {
                if (sortOrder == "asc")
                    filtered.sort!((a, b) => a.updatedAt < b.updatedAt);
                else
                    filtered.sort!((a, b) => a.updatedAt > b.updatedAt);
            } else { // created_at
                if (sortOrder == "asc")
                    filtered.sort!((a, b) => a.createdAt < b.createdAt);
                else
                    filtered.sort!((a, b) => a.createdAt > b.createdAt);
            }
            
            // Pagination
            int offset = pageToken ? pageToken.to!int : 0;
            int actualPageSize = min(pageSize, 100);
            
            if (offset < filtered.length) {
                int end = min(offset + actualPageSize, cast(int)filtered.length);
                return filtered[offset .. end];
            }
            
            return [];
        }
    }
    
    Task* getTask(string id) {
        synchronized (mutex) {
            foreach (ref task; tasks) {
                if (task.id == id)
                    return &task;
            }
            return null;
        }
    }
    
    Task createTask(Json json) {
        synchronized (mutex) {
            if ("title" !in json || json["title"].type != Json.Type.string_)
                throw new Exception("Title is required");
            
            auto task = Task(json["title"].get!string);
            task.updateFromJson(json);
            tasks ~= task;
            return task;
        }
    }
    
    Task* updateTask(string id, Json json) {
        synchronized (mutex) {
            foreach (ref task; tasks) {
                if (task.id == id) {
                    task.updateFromJson(json);
                    return &task;
                }
            }
            return null;
        }
    }
    
    Task* updateTaskStatus(string id, string status) {
        synchronized (mutex) {
            foreach (ref task; tasks) {
                if (task.id == id) {
                    task.status = status.to!TaskStatus;
                    task.updatedAt = Clock.currTime(UTC());
                    return &task;
                }
            }
            return null;
        }
    }
    
    bool deleteTask(string id) {
        synchronized (mutex) {
            auto oldLength = tasks.length;
            tasks = tasks.filter!(t => t.id != id).array;
            return tasks.length < oldLength;
        }
    }
    
    size_t count() {
        synchronized (mutex) {
            return tasks.length;
        }
    }
}

// REST API Interface
@path("/api")
interface ITaskAPI {
    // GET /api/tasks
    @path("/tasks")
    Json getTasks(string status = null, string assigned_to = null, string tags = null,
                  int page_size = 20, string page_token = null, 
                  string sort_by = "created_at", string sort_order = "desc");
    
    // GET /api/tasks/:id
    @path("/tasks/:id")
    Json getTask(string _id);
    
    // POST /api/tasks
    @path("/tasks")
    @method(HTTPMethod.POST)
    Json createTask(Json task);
    
    // PUT /api/tasks/:id
    @path("/tasks/:id")
    @method(HTTPMethod.PUT)
    Json updateTask(string _id, Json updates);
    
    // PATCH /api/tasks/:id/status
    @path("/tasks/:id/status")
    @method(HTTPMethod.PATCH)
    Json updateTaskStatus(string _id, string status);
    
    // DELETE /api/tasks/:id
    @path("/tasks/:id")
    @method(HTTPMethod.DELETE)
    void deleteTask(string _id);
}

// REST API Implementation
class TaskAPI : ITaskAPI {
    private TaskRepository repository;
    
    this() {
        repository = new TaskRepository();
    }
    
    Json getTasks(string status = null, string assigned_to = null, string tags = null,
                  int page_size = 20, string page_token = null,
                  string sort_by = "created_at", string sort_order = "desc") {
        string[] tagArray = tags ? tags.split(",") : null;
        
        auto tasks = repository.listTasks(status, assigned_to, tagArray, 
                                         page_size, page_token, sort_by, sort_order);
        
        Json response = Json.emptyObject;
        response["tasks"] = Json.emptyArray;
        foreach (task; tasks) {
            response["tasks"] ~= task.toJson();
        }
        response["total_count"] = cast(int)tasks.length;
        response["page_size"] = page_size;
        
        if (tasks.length == page_size) {
            int nextOffset = (page_token ? page_token.to!int : 0) + page_size;
            response["next_page_token"] = nextOffset.to!string;
        }
        
        return response;
    }
    
    Json getTask(string _id) {
        auto task = repository.getTask(_id);
        if (task is null) {
            throw new HTTPStatusException(HTTPStatus.notFound, "Task not found");
        }
        return task.toJson();
    }
    
    Json createTask(Json taskJson) {
        try {
            auto task = repository.createTask(taskJson);
            return task.toJson();
        } catch (Exception e) {
            throw new HTTPStatusException(HTTPStatus.badRequest, e.msg);
        }
    }
    
    Json updateTask(string _id, Json updates) {
        auto task = repository.updateTask(_id, updates);
        if (task is null) {
            throw new HTTPStatusException(HTTPStatus.notFound, "Task not found");
        }
        return task.toJson();
    }
    
    Json updateTaskStatus(string _id, string status) {
        if (status is null || status.length == 0) {
            throw new HTTPStatusException(HTTPStatus.badRequest, "Status is required");
        }
        
        try {
            auto task = repository.updateTaskStatus(_id, status);
            if (task is null) {
                throw new HTTPStatusException(HTTPStatus.notFound, "Task not found");
            }
            return task.toJson();
        } catch (Exception e) {
            throw new HTTPStatusException(HTTPStatus.badRequest, "Invalid status value");
        }
    }
    
    void deleteTask(string _id) {
        if (!repository.deleteTask(_id)) {
            throw new HTTPStatusException(HTTPStatus.notFound, "Task not found");
        }
    }
}

// Health check endpoint
void getHealth(HTTPServerRequest req, HTTPServerResponse res) {
    static TaskRepository repo;
    if (repo is null) repo = new TaskRepository();
    
    Json health = Json.emptyObject;
    health["status"] = "healthy";
    health["service"] = "d-task-api";
    health["task_count"] = cast(int)repo.count();
    
    res.writeJsonBody(health);
}

shared static this() {
    writeln("╔════════════════════════════════════════════════╗");
    writeln("║          D Task Management REST API            ║");
    writeln("║            Built with Vibe.d                   ║");
    writeln("╚════════════════════════════════════════════════╝");
    writeln();
    
    auto router = new URLRouter;
    
    // Register REST API
    router.registerRestInterface(new TaskAPI());
    
    // Health check
    router.get("/health", &getHealth);
    
    // CORS support
    router.any("*", (HTTPServerRequest req, HTTPServerResponse res) {
        res.headers["Access-Control-Allow-Origin"] = "*";
        res.headers["Access-Control-Allow-Methods"] = "GET, POST, PUT, PATCH, DELETE, OPTIONS";
        res.headers["Access-Control-Allow-Headers"] = "Content-Type";
        
        if (req.method == HTTPMethod.OPTIONS) {
            res.statusCode = HTTPStatus.noContent;
            res.writeVoidBody();
        }
    });
    
    auto settings = new HTTPServerSettings;
    settings.port = 8080;
    settings.bindAddresses = ["0.0.0.0"];
    
    // Check for port argument
    import std.process : environment;
    auto portStr = environment.get("PORT");
    if (portStr) {
        try {
            settings.port = portStr.to!ushort;
        } catch (Exception e) {
            writeln("Invalid port, using default 8080");
        }
    }
    
    listenHTTP(settings, router);
    
    writeln("[INFO] D Task REST Server started on port ", settings.port);
    writeln("[INFO] Visit http://localhost:", settings.port, "/api/tasks");
    writeln();
    writeln("Available endpoints:");
    writeln("  GET    /api/tasks          - List all tasks");
    writeln("  GET    /api/tasks/{id}     - Get a specific task");
    writeln("  POST   /api/tasks          - Create a new task");
    writeln("  PUT    /api/tasks/{id}     - Update a task");
    writeln("  PATCH  /api/tasks/{id}/status - Update task status");
    writeln("  DELETE /api/tasks/{id}     - Delete a task");
    writeln("  GET    /health             - Health check");
    writeln();
    writeln("Sample requests:");
    writeln("  curl http://localhost:", settings.port, "/api/tasks");
    writeln("  curl -X POST http://localhost:", settings.port, "/api/tasks \\");
    writeln("    -H \"Content-Type: application/json\" \\");
    writeln("    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'");
    writeln();
    writeln("[INFO] Press Ctrl+C to stop the server");
}