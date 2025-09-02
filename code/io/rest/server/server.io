#!/usr/bin/env io

// Task Management REST API Server in Io

// Task prototype
Task := Object clone do(
    id ::= nil
    title ::= nil
    description ::= nil
    status ::= "pending"
    priority ::= "medium"
    tags ::= list()
    assignedTo ::= nil
    createdAt ::= nil
    updatedAt ::= nil
    
    // Convert to JSON-compatible Map
    asMap := method(
        Map clone atPut("id", id) \
            atPut("title", title) \
            atPut("description", description) \
            atPut("status", status) \
            atPut("priority", priority) \
            atPut("tags", tags) \
            atPut("assigned_to", assignedTo) \
            atPut("created_at", createdAt) \
            atPut("updated_at", updatedAt)
    )
    
    // Create from map
    fromMap := method(map,
        self setId(map at("id"))
        self setTitle(map at("title"))
        self setDescription(map at("description") ifNil(""))
        self setStatus(map at("status") ifNil("pending"))
        self setPriority(map at("priority") ifNil("medium"))
        self setTags(map at("tags") ifNil(list()))
        self setAssignedTo(map at("assigned_to"))
        self setCreatedAt(map at("created_at"))
        self setUpdatedAt(map at("updated_at"))
        self
    )
)

// Task Store - manages all tasks
TaskStore := Object clone do(
    tasks ::= Map clone
    counter ::= 0
    
    init := method(
        setTasks(Map clone)
        setCounter(0)
        self
    )
    
    // List all tasks with optional filters
    listTasks := method(statusFilter, assignedFilter,
        result := list()
        tasks foreach(id, task,
            shouldInclude := true
            
            if(statusFilter != nil and task status != statusFilter,
                shouldInclude = false
            )
            
            if(assignedFilter != nil and task assignedTo != assignedFilter,
                shouldInclude = false
            )
            
            if(shouldInclude, result append(task))
        )
        result
    )
    
    // Get a specific task
    getTask := method(taskId,
        tasks at(taskId)
    )
    
    // Create a new task
    createTask := method(title, description, priority, tags, assignedTo,
        counter = counter + 1
        taskId := "task_" .. counter
        now := Date now asNumber
        
        task := Task clone
        task setId(taskId)
        task setTitle(title)
        task setDescription(description ifNil(""))
        task setStatus("pending")
        task setPriority(priority ifNil("medium"))
        task setTags(tags ifNil(list()))
        task setAssignedTo(assignedTo)
        task setCreatedAt(now)
        task setUpdatedAt(now)
        
        tasks atPut(taskId, task)
        task
    )
    
    // Update a task
    updateTask := method(taskId, updates,
        task := tasks at(taskId)
        if(task == nil, return nil)
        
        if(updates hasKey("title"), task setTitle(updates at("title")))
        if(updates hasKey("description"), task setDescription(updates at("description")))
        if(updates hasKey("status"), task setStatus(updates at("status")))
        if(updates hasKey("priority"), task setPriority(updates at("priority")))
        if(updates hasKey("tags"), task setTags(updates at("tags")))
        if(updates hasKey("assigned_to"), task setAssignedTo(updates at("assigned_to")))
        
        task setUpdatedAt(Date now asNumber)
        task
    )
    
    // Update task status
    updateTaskStatus := method(taskId, newStatus,
        task := tasks at(taskId)
        if(task == nil, return nil)
        
        task setStatus(newStatus)
        task setUpdatedAt(Date now asNumber)
        task
    )
    
    // Delete a task
    deleteTask := method(taskId,
        if(tasks hasKey(taskId),
            tasks removeAt(taskId)
            true,
            false
        )
    )
)

// JSON utilities
JSON := Object clone do(
    // Simple JSON encoding
    encode := method(obj,
        if(obj type == "Number", return obj asString)
        if(obj type == "nil", return "null")
        if(obj type == "true" or obj type == "false", return obj asString)
        if(obj type == "Sequence", return "\"" .. obj .. "\"")
        
        if(obj type == "List",
            items := obj map(item, encode(item))
            return "[" .. items join(", ") .. "]"
        )
        
        if(obj type == "Map",
            pairs := list()
            obj foreach(key, value,
                pairs append("\"" .. key .. "\": " .. encode(value))
            )
            return "{" .. pairs join(", ") .. "}"
        )
        
        "{}"
    )
    
    // Parse JSON (simplified)
    decode := method(jsonString,
        // This is a simplified decoder for demonstration
        // In production, use a proper JSON library
        jsonString
    )
)

// HTTP Response builder
HttpResponse := Object clone do(
    status ::= 200
    headers ::= Map clone
    body ::= ""
    
    init := method(
        setHeaders(Map clone atPut("Content-Type", "application/json"))
        self
    )
    
    setJsonBody := method(data,
        setBody(JSON encode(data))
        self
    )
    
    send := method(socket,
        statusText := if(status == 200, "OK",
            if(status == 201, "Created",
            if(status == 204, "No Content",
            if(status == 400, "Bad Request",
            if(status == 404, "Not Found",
            if(status == 405, "Method Not Allowed",
            "Unknown"))))))
        
        response := "HTTP/1.1 " .. status .. " " .. statusText .. "\r\n"
        headers foreach(key, value,
            response = response .. key .. ": " .. value .. "\r\n"
        )
        response = response .. "Content-Length: " .. body size .. "\r\n"
        response = response .. "\r\n"
        response = response .. body
        
        socket write(response)
    )
)

// Request parser
HttpRequest := Object clone do(
    method ::= nil
    path ::= nil
    headers ::= Map clone
    body ::= nil
    params ::= Map clone
    
    parseFromSocket := method(socket,
        // Read request line
        requestLine := socket readLine
        if(requestLine == nil, return nil)
        
        parts := requestLine split(" ")
        if(parts size < 3, return nil)
        
        setMethod(parts at(0))
        
        // Parse path and query params
        fullPath := parts at(1)
        if(fullPath containsSeq("?"),
            pathParts := fullPath split("?")
            setPath(pathParts at(0))
            parseQueryParams(pathParts at(1)),
            setPath(fullPath)
        )
        
        // Read headers
        loop(
            line := socket readLine
            if(line == nil or line size == 0, break)
            
            colonIndex := line findSeq(":")
            if(colonIndex != nil,
                key := line slice(0, colonIndex)
                value := line slice(colonIndex + 2) # Skip ": "
                headers atPut(key, value)
            )
        )
        
        // Read body if present
        contentLength := headers at("Content-Length")
        if(contentLength != nil,
            bodySize := contentLength asNumber
            if(bodySize > 0,
                setBody(socket read(bodySize))
            )
        )
        
        self
    )
    
    parseQueryParams := method(queryString,
        queryString split("&") foreach(param,
            keyValue := param split("=")
            if(keyValue size == 2,
                params atPut(keyValue at(0), keyValue at(1))
            )
        )
    )
    
    parseJsonBody := method(
        if(body != nil,
            // Simplified JSON parsing
            // In production, use proper JSON parser
            map := Map clone
            
            // Extract key-value pairs from JSON
            bodyStr := body asString
            if(bodyStr containsSeq("\"title\""),
                titleMatch := bodyStr findRegex("\"title\"\\s*:\\s*\"([^\"]+)\"")
                if(titleMatch, map atPut("title", titleMatch at(1)))
            )
            if(bodyStr containsSeq("\"description\""),
                descMatch := bodyStr findRegex("\"description\"\\s*:\\s*\"([^\"]+)\"")
                if(descMatch, map atPut("description", descMatch at(1)))
            )
            if(bodyStr containsSeq("\"priority\""),
                prioMatch := bodyStr findRegex("\"priority\"\\s*:\\s*\"([^\"]+)\"")
                if(prioMatch, map atPut("priority", prioMatch at(1)))
            )
            if(bodyStr containsSeq("\"status\""),
                statusMatch := bodyStr findRegex("\"status\"\\s*:\\s*\"([^\"]+)\"")
                if(statusMatch, map atPut("status", statusMatch at(1)))
            )
            if(bodyStr containsSeq("\"assigned_to\""),
                assignedMatch := bodyStr findRegex("\"assigned_to\"\\s*:\\s*\"([^\"]+)\"")
                if(assignedMatch, map atPut("assigned_to", assignedMatch at(1)))
            )
            
            return map,
            
            Map clone
        )
    )
)

// REST API Server
RestServer := Object clone do(
    port ::= 8080
    store ::= nil
    serverSocket ::= nil
    
    init := method(
        setStore(TaskStore clone init)
        seedData
        self
    )
    
    // Add some initial data
    seedData := method(
        store createTask("Learn Io language", "Master prototype-based programming", "high", list("io", "learning"), "developer")
        store createTask("Build REST API", "Create REST API with sockets", "medium", list("io", "api", "web"), "backend-team")
    )
    
    // Start the server
    start := method(
        writeln("Starting Io Task Server on port ", port, "...")
        
        serverSocket := Socket clone
        serverSocket setHost("127.0.0.1")
        serverSocket setPort(port)
        serverSocket serverOpen
        
        writeln("Server listening on port ", port)
        
        loop(
            clientSocket := serverSocket serverAccept
            if(clientSocket != nil,
                handleRequest(clientSocket)
                clientSocket close
            )
        )
    )
    
    // Handle incoming request
    handleRequest := method(socket,
        request := HttpRequest clone parseFromSocket(socket)
        if(request == nil, return)
        
        writeln(request method, " ", request path)
        
        response := routeRequest(request)
        response send(socket)
    )
    
    // Route request to appropriate handler
    routeRequest := method(request,
        path := request path
        method := request method
        
        // Health check
        if(path == "/health",
            return handleHealth()
        )
        
        // Task routes
        if(path == "/api/tasks",
            if(method == "GET", return handleListTasks(request))
            if(method == "POST", return handleCreateTask(request))
            return methodNotAllowed()
        )
        
        // Task by ID routes
        if(path beginsWithSeq("/api/tasks/"),
            taskId := path slice(11)
            
            // Status update
            if(taskId endsWithSeq("/status"),
                taskId = taskId slice(0, taskId size - 7)
                if(method == "PATCH", return handleUpdateStatus(request, taskId))
                return methodNotAllowed()
            )
            
            if(method == "GET", return handleGetTask(taskId))
            if(method == "PUT", return handleUpdateTask(request, taskId))
            if(method == "DELETE", return handleDeleteTask(taskId))
            return methodNotAllowed()
        )
        
        notFound()
    )
    
    // Handler methods
    handleHealth := method(
        HttpResponse clone setStatus(200) setJsonBody(
            Map clone atPut("status", "healthy") \
                     atPut("service", "task-server") \
                     atPut("version", "1.0.0")
        )
    )
    
    handleListTasks := method(request,
        statusFilter := request params at("status")
        assignedFilter := request params at("assigned_to")
        
        tasks := store listTasks(statusFilter, assignedFilter)
        taskMaps := tasks map(task, task asMap)
        
        HttpResponse clone setStatus(200) setJsonBody(
            Map clone atPut("tasks", taskMaps) \
                     atPut("count", tasks size)
        )
    )
    
    handleGetTask := method(taskId,
        task := store getTask(taskId)
        if(task == nil,
            return notFound("Task not found")
        )
        
        HttpResponse clone setStatus(200) setJsonBody(task asMap)
    )
    
    handleCreateTask := method(request,
        data := request parseJsonBody
        
        task := store createTask(
            data at("title"),
            data at("description"),
            data at("priority"),
            data at("tags"),
            data at("assigned_to")
        )
        
        HttpResponse clone setStatus(201) setJsonBody(task asMap)
    )
    
    handleUpdateTask := method(request, taskId,
        data := request parseJsonBody
        
        task := store updateTask(taskId, data)
        if(task == nil,
            return notFound("Task not found")
        )
        
        HttpResponse clone setStatus(200) setJsonBody(task asMap)
    )
    
    handleUpdateStatus := method(request, taskId,
        data := request parseJsonBody
        newStatus := data at("status")
        
        task := store updateTaskStatus(taskId, newStatus)
        if(task == nil,
            return notFound("Task not found")
        )
        
        HttpResponse clone setStatus(200) setJsonBody(task asMap)
    )
    
    handleDeleteTask := method(taskId,
        success := store deleteTask(taskId)
        if(success,
            HttpResponse clone setStatus(204) setBody(""),
            notFound("Task not found")
        )
    )
    
    // Error responses
    notFound := method(message,
        HttpResponse clone setStatus(404) setJsonBody(
            Map clone atPut("error", message ifNil("Not found")) \
                     atPut("code", 404)
        )
    )
    
    methodNotAllowed := method(
        HttpResponse clone setStatus(405) setJsonBody(
            Map clone atPut("error", "Method not allowed") \
                     atPut("code", 405)
        )
    )
)

// Main entry point
server := RestServer clone init
server start