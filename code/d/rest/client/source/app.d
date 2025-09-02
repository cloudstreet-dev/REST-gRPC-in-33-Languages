import vibe.data.json;
import vibe.http.client;
import vibe.stream.operations;
import std.stdio;
import std.string;
import std.conv;
import std.process : environment;
import std.getopt;
import std.algorithm;
import std.array;
import core.time;

class TaskAPIClient {
    private string baseUrl;
    
    this(string baseUrl = "http://localhost:8080") {
        this.baseUrl = baseUrl;
    }
    
    Json request(HTTPMethod method, string path, Json requestBody = Json.undefined) {
        string url = baseUrl ~ path;
        Json responseJson;
        
        requestHTTP(url,
            (scope req) {
                req.method = method;
                if (requestBody != Json.undefined) {
                    req.headers["Content-Type"] = "application/json";
                    req.writeJsonBody(requestBody);
                }
            },
            (scope res) {
                if (res.statusCode >= 200 && res.statusCode < 300) {
                    if (res.statusCode != 204) {
                        responseJson = res.readJson();
                    } else {
                        responseJson = Json.emptyObject;
                        responseJson["success"] = true;
                    }
                } else {
                    string error = format("HTTP %d: %s", res.statusCode, res.statusPhrase);
                    try {
                        auto errorJson = res.readJson();
                        if ("error" in errorJson) {
                            error = errorJson["error"].get!string;
                        }
                    } catch (Exception e) {
                        // Use default error message
                    }
                    throw new Exception(error);
                }
            }
        );
        
        return responseJson;
    }
    
    Json listTasks(string status = null, string assignedTo = null, string[] tags = null) {
        string path = "/api/tasks";
        string[] params;
        
        if (status) params ~= "status=" ~ status;
        if (assignedTo) params ~= "assigned_to=" ~ assignedTo;
        if (tags && tags.length > 0) params ~= "tags=" ~ tags.join(",");
        
        if (params.length > 0) {
            path ~= "?" ~ params.join("&");
        }
        
        return request(HTTPMethod.GET, path);
    }
    
    Json getTask(string id) {
        return request(HTTPMethod.GET, "/api/tasks/" ~ id);
    }
    
    Json createTask(Json task) {
        return request(HTTPMethod.POST, "/api/tasks", task);
    }
    
    Json updateTask(string id, Json updates) {
        return request(HTTPMethod.PUT, "/api/tasks/" ~ id, updates);
    }
    
    Json updateTaskStatus(string id, string status) {
        return request(HTTPMethod.PATCH, "/api/tasks/" ~ id ~ "/status?status=" ~ status);
    }
    
    Json deleteTask(string id) {
        return request(HTTPMethod.DELETE, "/api/tasks/" ~ id);
    }
    
    Json health() {
        return request(HTTPMethod.GET, "/health");
    }
}

void printTask(Json task) {
    writeln("╔═══════════════════════════════════════════════════════╗");
    writefln("║ Task ID: %-44s ║", task["id"].get!string);
    writefln("║ Title: %-46s ║", task["title"].get!string);
    
    if ("description" in task && task["description"].type == Json.Type.string_) {
        string desc = task["description"].get!string;
        if (desc.length > 44) desc = desc[0..44];
        writefln("║ Description: %-40s ║", desc);
    }
    
    writefln("║ Status: %-45s ║", task["status"].get!string);
    writefln("║ Priority: %-43s ║", task["priority"].get!string);
    
    if ("tags" in task && task["tags"].type == Json.Type.array && task["tags"].length > 0) {
        string[] tags;
        foreach (tag; task["tags"]) {
            tags ~= tag.get!string;
        }
        string tagStr = tags.join(", ");
        if (tagStr.length > 42) tagStr = tagStr[0..42];
        writefln("║ Tags: %-47s ║", tagStr);
    }
    
    if ("assigned_to" in task && task["assigned_to"].type == Json.Type.string_) {
        writefln("║ Assigned To: %-40s ║", task["assigned_to"].get!string);
    }
    
    writefln("║ Created: %-44s ║", task["created_at"].get!string);
    writefln("║ Updated: %-44s ║", task["updated_at"].get!string);
    writeln("╚═══════════════════════════════════════════════════════╝");
}

void printHelp() {
    writeln("╔════════════════════════════════════════════════╗");
    writeln("║         D Task Management CLI                  ║");
    writeln("╚════════════════════════════════════════════════╝");
    writeln();
    writeln("Usage: task-rest-client <command> [options]");
    writeln();
    writeln("Commands:");
    writeln("  list                    List all tasks");
    writeln("  get <task-id>          Get a specific task");
    writeln("  create <json>          Create a new task");
    writeln("  update <task-id> <json> Update a task");
    writeln("  delete <task-id>       Delete a task");
    writeln("  health                 Check server health");
    writeln("  demo                   Run a demonstration");
    writeln("  help                   Show this help");
    writeln();
    writeln("Options:");
    writeln("  --url <url>            API base URL (default: http://localhost:8080)");
    writeln();
    writeln("Environment:");
    writeln("  TASK_API_URL           Base URL for the Task API");
    writeln();
    writeln("Examples:");
    writeln("  task-rest-client list");
    writeln("  task-rest-client get 123e4567-e89b-12d3-a456-426614174000");
    writeln("  task-rest-client create '{\"title\":\"New Task\",\"priority\":\"high\"}'");
    writeln("  task-rest-client delete 123e4567-e89b-12d3-a456-426614174000");
}

void runDemo(TaskAPIClient client) {
    writeln("╔════════════════════════════════════════════════╗");
    writeln("║          Task Management API Demo              ║");
    writeln("╚════════════════════════════════════════════════╝");
    writeln();
    
    try {
        // Step 1: List tasks
        writeln("1. Listing all tasks...");
        auto response = client.listTasks();
        writeln("   Found ", response["tasks"].length, " tasks");
        foreach (task; response["tasks"]) {
            writeln("   - ", task["id"].get!string, ": ", task["title"].get!string);
        }
        writeln();
        
        // Step 2: Create a task
        writeln("2. Creating a new task...");
        Json newTask = Json.emptyObject;
        newTask["title"] = "Demo Task from D Client";
        newTask["description"] = "This task was created using the D REST client";
        newTask["priority"] = "high";
        newTask["tags"] = Json.emptyArray;
        newTask["tags"] ~= Json("demo");
        newTask["tags"] ~= Json("d");
        newTask["tags"] ~= Json("api-test");
        newTask["assigned_to"] = "demo-user";
        
        auto created = client.createTask(newTask);
        writeln("   Created task: ", created["id"].get!string);
        printTask(created);
        writeln();
        
        string taskId = created["id"].get!string;
        
        // Step 3: Get the created task
        writeln("3. Retrieving the created task...");
        auto retrieved = client.getTask(taskId);
        writeln("   Retrieved task:");
        printTask(retrieved);
        writeln();
        
        // Step 4: Update the task
        writeln("4. Updating the task...");
        Json updates = Json.emptyObject;
        updates["title"] = "Updated Demo Task";
        updates["description"] = "This task has been updated via the API";
        updates["status"] = "in_progress";
        updates["priority"] = "urgent";
        
        auto updated = client.updateTask(taskId, updates);
        writeln("   Updated task:");
        printTask(updated);
        writeln();
        
        // Step 5: Update task status
        writeln("5. Updating task status to completed...");
        auto statusUpdated = client.updateTaskStatus(taskId, "completed");
        writeln("   Task status updated:");
        printTask(statusUpdated);
        writeln();
        
        // Step 6: List completed tasks
        writeln("6. Listing completed tasks...");
        auto completed = client.listTasks("completed");
        writeln("   Found ", completed["tasks"].length, " completed tasks");
        foreach (task; completed["tasks"]) {
            writeln("   - ", task["id"].get!string, ": ", task["title"].get!string,
                   " (Status: ", task["status"].get!string, ")");
        }
        writeln();
        
        // Step 7: Delete the task
        writeln("7. Deleting the demo task...");
        client.deleteTask(taskId);
        writeln("   Task deleted successfully");
        writeln();
        
        // Step 8: Verify deletion
        writeln("8. Verifying task deletion...");
        try {
            client.getTask(taskId);
            writeln("   Warning: Task still exists!");
        } catch (Exception e) {
            writeln("   Task not found (as expected): ", e.msg);
        }
        
    } catch (Exception e) {
        writeln("Error: ", e.msg);
    }
    
    writeln();
    writeln("╔════════════════════════════════════════════════╗");
    writeln("║              Demo Complete!                    ║");
    writeln("╚════════════════════════════════════════════════╝");
}

void main(string[] args) {
    string baseUrl = environment.get("TASK_API_URL", "http://localhost:8080");
    string command;
    
    auto helpInfo = getopt(args,
        "url", "API base URL", &baseUrl
    );
    
    if (helpInfo.helpWanted || args.length < 2) {
        printHelp();
        return;
    }
    
    command = args[1];
    auto client = new TaskAPIClient(baseUrl);
    
    try {
        switch (command) {
            case "help":
                printHelp();
                break;
                
            case "demo":
                runDemo(client);
                break;
                
            case "health":
                auto health = client.health();
                writeln("Server health:");
                writeln("  Status: ", health["status"].get!string);
                writeln("  Service: ", health["service"].get!string);
                writeln("  Task count: ", health["task_count"].get!int);
                break;
                
            case "list":
                auto tasks = client.listTasks();
                writeln("Found ", tasks["tasks"].length, " tasks:");
                foreach (task; tasks["tasks"]) {
                    printTask(task);
                    writeln();
                }
                break;
                
            case "get":
                if (args.length < 3) {
                    writeln("Error: Task ID required");
                    return;
                }
                auto task = client.getTask(args[2]);
                printTask(task);
                break;
                
            case "create":
                if (args.length < 3) {
                    writeln("Error: JSON data required");
                    return;
                }
                auto newTask = parseJsonString(args[2]);
                auto created = client.createTask(newTask);
                writeln("Task created successfully:");
                printTask(created);
                break;
                
            case "update":
                if (args.length < 4) {
                    writeln("Error: Task ID and JSON data required");
                    return;
                }
                auto updates = parseJsonString(args[3]);
                auto updated = client.updateTask(args[2], updates);
                writeln("Task updated successfully:");
                printTask(updated);
                break;
                
            case "delete":
                if (args.length < 3) {
                    writeln("Error: Task ID required");
                    return;
                }
                client.deleteTask(args[2]);
                writeln("Task ", args[2], " deleted successfully");
                break;
                
            default:
                writeln("Unknown command: ", command);
                printHelp();
                break;
        }
    } catch (Exception e) {
        writeln("Error: ", e.msg);
    }
}