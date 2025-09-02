#!/usr/bin/env io

// Task Management REST API Client in Io

// HTTP Client for REST API
HttpClient := Object clone do(
    baseUrl ::= nil
    host ::= nil
    port ::= nil
    
    init := method(url,
        setBaseUrl(url)
        
        // Parse URL (simplified)
        if(url beginsWithSeq("http://"),
            urlPart := url slice(7)
            parts := urlPart split(":")
            setHost(parts at(0))
            setPort(if(parts size > 1, parts at(1) asNumber, 80))
        ,
            setHost("localhost")
            setPort(8080)
        )
        self
    )
    
    // Make HTTP request
    request := method(method, path, body,
        socket := Socket clone
        socket setHost(host)
        socket setPort(port)
        socket connect
        
        // Build request
        request := method .. " " .. path .. " HTTP/1.1\r\n"
        request = request .. "Host: " .. host .. "\r\n"
        request = request .. "User-Agent: Io-Client/1.0\r\n"
        
        if(body != nil,
            request = request .. "Content-Type: application/json\r\n"
            request = request .. "Content-Length: " .. body size .. "\r\n"
        )
        
        request = request .. "Connection: close\r\n"
        request = request .. "\r\n"
        
        if(body != nil, request = request .. body)
        
        socket write(request)
        
        // Read response
        response := ""
        loop(
            data := socket read(1024)
            if(data == nil or data size == 0, break)
            response = response .. data
        )
        
        socket close
        
        // Parse response (simplified)
        parseResponse(response)
    )
    
    // Parse HTTP response
    parseResponse := method(response,
        lines := response split("\r\n")
        
        // Get status code
        statusLine := lines at(0)
        statusParts := statusLine split(" ")
        statusCode := if(statusParts size > 1, statusParts at(1) asNumber, 0)
        
        // Find body start
        bodyStart := response findSeq("\r\n\r\n")
        body := if(bodyStart != nil, response slice(bodyStart + 4), "")
        
        Map clone atPut("status", statusCode) atPut("body", body)
    )
    
    // REST methods
    get := method(path,
        request("GET", path, nil)
    )
    
    post := method(path, body,
        request("POST", path, body)
    )
    
    put := method(path, body,
        request("PUT", path, body)
    )
    
    patch := method(path, body,
        request("PATCH", path, body)
    )
    
    delete := method(path,
        request("DELETE", path, nil)
    )
)

// Task Client
TaskClient := Object clone do(
    client ::= nil
    
    init := method(baseUrl,
        setClient(HttpClient clone init(baseUrl))
        self
    )
    
    // List all tasks
    listTasks := method(status, assignedTo,
        path := "/api/tasks"
        params := list()
        
        if(status != nil, params append("status=" .. status))
        if(assignedTo != nil, params append("assigned_to=" .. assignedTo))
        
        if(params size > 0,
            path = path .. "?" .. params join("&")
        )
        
        response := client get(path)
        
        if(response at("status") == 200,
            writeln("Tasks retrieved successfully")
            parseTaskList(response at("body")),
            writeln("Failed to list tasks: ", response at("status"))
            nil
        )
    )
    
    // Get a specific task
    getTask := method(taskId,
        path := "/api/tasks/" .. taskId
        response := client get(path)
        
        if(response at("status") == 200,
            writeln("Task retrieved successfully")
            parseTask(response at("body")),
            if(response at("status") == 404,
                writeln("Task not found"),
                writeln("Failed to get task: ", response at("status"))
            )
            nil
        )
    )
    
    // Create a new task
    createTask := method(title, description, priority, tags, assignedTo,
        body := buildJsonBody(
            Map clone atPut("title", title) \
                     atPut("description", description) \
                     atPut("priority", priority) \
                     atPut("assigned_to", assignedTo)
        )
        
        response := client post("/api/tasks", body)
        
        if(response at("status") == 201,
            writeln("Task created successfully")
            parseTask(response at("body")),
            writeln("Failed to create task: ", response at("status"))
            nil
        )
    )
    
    // Update a task
    updateTask := method(taskId, updates,
        body := buildJsonBody(updates)
        path := "/api/tasks/" .. taskId
        
        response := client put(path, body)
        
        if(response at("status") == 200,
            writeln("Task updated successfully")
            parseTask(response at("body")),
            writeln("Failed to update task: ", response at("status"))
            nil
        )
    )
    
    // Update task status
    updateTaskStatus := method(taskId, newStatus,
        body := "{\"status\": \"" .. newStatus .. "\"}"
        path := "/api/tasks/" .. taskId .. "/status"
        
        response := client patch(path, body)
        
        if(response at("status") == 200,
            writeln("Status updated successfully")
            parseTask(response at("body")),
            writeln("Failed to update status: ", response at("status"))
            nil
        )
    )
    
    // Delete a task
    deleteTask := method(taskId,
        path := "/api/tasks/" .. taskId
        response := client delete(path)
        
        if(response at("status") == 204,
            writeln("Task deleted successfully")
            true,
            writeln("Failed to delete task: ", response at("status"))
            false
        )
    )
    
    // Helper methods
    buildJsonBody := method(data,
        pairs := list()
        data foreach(key, value,
            if(value != nil,
                pairs append("\"" .. key .. "\": \"" .. value .. "\"")
            )
        )
        "{" .. pairs join(", ") .. "}"
    )
    
    parseTask := method(jsonBody,
        // Simplified parsing - in production use proper JSON parser
        map := Map clone
        
        // Extract ID
        idMatch := jsonBody findRegex("\"id\"\\s*:\\s*\"([^\"]+)\"")
        if(idMatch, map atPut("id", idMatch at(1)))
        
        // Extract title
        titleMatch := jsonBody findRegex("\"title\"\\s*:\\s*\"([^\"]+)\"")
        if(titleMatch, map atPut("title", titleMatch at(1)))
        
        // Extract status
        statusMatch := jsonBody findRegex("\"status\"\\s*:\\s*\"([^\"]+)\"")
        if(statusMatch, map atPut("status", statusMatch at(1)))
        
        // Extract priority
        priorityMatch := jsonBody findRegex("\"priority\"\\s*:\\s*\"([^\"]+)\"")
        if(priorityMatch, map atPut("priority", priorityMatch at(1)))
        
        map
    )
    
    parseTaskList := method(jsonBody,
        // Extract task count
        countMatch := jsonBody findRegex("\"count\"\\s*:\\s*(\\d+)")
        count := if(countMatch, countMatch at(1) asNumber, 0)
        
        writeln("Found ", count, " tasks")
        
        // For demonstration, just return the count
        count
    )
)

// Demo runner
Demo := Object clone do(
    run := method(
        printBanner
        
        // Wait for server
        wait(1)
        
        client := TaskClient clone init("http://localhost:8080")
        
        writeln("\n1. Listing all tasks...")
        client listTasks(nil, nil)
        
        writeln("\n2. Creating a new task...")
        task := client createTask(
            "Master Io's prototypes",
            "Learn prototype-based OOP and message passing",
            "high",
            nil,
            "io-team"
        )
        
        if(task != nil,
            taskId := task at("id")
            writeln("   Created task with ID: ", taskId)
            writeln("   Title: ", task at("title"))
            writeln("   Priority: ", task at("priority"))
            
            writeln("\n3. Getting task details...")
            taskDetail := client getTask(taskId)
            if(taskDetail != nil,
                writeln("   Status: ", taskDetail at("status"))
            )
            
            writeln("\n4. Updating task status to 'in-progress'...")
            updated := client updateTaskStatus(taskId, "in-progress")
            if(updated != nil,
                writeln("   New status: ", updated at("status"))
            )
            
            writeln("\n5. Updating task details...")
            updates := Map clone atPut("title", "Master Io language and actors") \
                                atPut("priority", "urgent")
            updated = client updateTask(taskId, updates)
            if(updated != nil,
                writeln("   Updated title: ", updated at("title"))
                writeln("   Updated priority: ", updated at("priority"))
            )
            
            writeln("\n6. Filtering tasks by status...")
            client listTasks("in-progress", nil)
            
            writeln("\n7. Deleting the task...")
            client deleteTask(taskId)
            
            writeln("\n8. Verifying deletion...")
            client getTask(taskId)
        )
        
        writeln("\n✅ Demo completed!")
    )
    
    printBanner := method(
        writeln("╔════════════════════════════════════════════════╗")
        writeln("║         Io Task Management REST Client         ║")
        writeln("║            Testing API Operations              ║")
        writeln("╚════════════════════════════════════════════════╝")
    )
)

// Run the demo
Demo run