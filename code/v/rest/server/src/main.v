module main

import vweb
import json
import time
import rand

// Task status enum
enum TaskStatus {
    pending
    in_progress
    completed
    cancelled
}

// Task priority enum  
enum TaskPriority {
    low = 1
    medium = 2
    high = 3
    urgent = 4
}

// Task struct
struct Task {
mut:
    id          string
    title       string
    description string
    status      TaskStatus
    priority    TaskPriority
    tags        []string
    assigned_to string
    created_at  i64
    updated_at  i64
}

// Request structs
struct CreateTaskRequest {
    title       string
    description string
    priority    string
    tags        []string
    assigned_to string
}

struct UpdateTaskRequest {
    title       string
    description string
    status      string
    priority    string
    tags        []string
    assigned_to string
}

struct UpdateStatusRequest {
    status string
}

// Response structs
struct TaskResponse {
    id          string   `json:"id"`
    title       string   `json:"title"`
    description string   `json:"description"`
    status      string   `json:"status"`
    priority    string   `json:"priority"`
    tags        []string `json:"tags"`
    assigned_to string   `json:"assigned_to"`
    created_at  f64      `json:"created_at"`
    updated_at  f64      `json:"updated_at"`
}

struct ListTasksResponse {
    tasks []TaskResponse `json:"tasks"`
    count int            `json:"count"`
}

struct ErrorResponse {
    error string `json:"error"`
    code  int    `json:"code"`
}

// App struct with vweb Context embedded
struct App {
    vweb.Context
mut:
    tasks   map[string]Task
    counter int
}

// Initialize the app
fn main() {
    mut app := &App{
        tasks: map[string]Task{}
        counter: 0
    }
    
    // Add some initial tasks
    app.seed_tasks()
    
    println('Starting V Task Server on port 8080...')
    vweb.run(app, 8080)
}

// Seed initial tasks
fn (mut app App) seed_tasks() {
    app.create_task_internal('Learn V language', 'Master the V programming language', .high, ['v', 'learning'], 'developer')
    app.create_task_internal('Build REST API', 'Create a REST API with vweb', .medium, ['v', 'api', 'web'], 'backend-team')
}

// Health check endpoint
pub fn (mut app App) health() vweb.Result {
    return app.json({
        'status': 'healthy'
        'service': 'task-server'
        'version': '1.0.0'
    })
}

// List all tasks - GET /api/tasks
['/api/tasks'; get]
pub fn (mut app App) list_tasks() vweb.Result {
    status_filter := app.query['status'] or { '' }
    assigned_filter := app.query['assigned_to'] or { '' }
    
    mut filtered_tasks := []TaskResponse{}
    
    for _, task in app.tasks {
        // Apply filters
        if status_filter != '' && task.status.str() != status_filter {
            continue
        }
        if assigned_filter != '' && task.assigned_to != assigned_filter {
            continue
        }
        
        filtered_tasks << task_to_response(task)
    }
    
    response := ListTasksResponse{
        tasks: filtered_tasks
        count: filtered_tasks.len
    }
    
    return app.json(response)
}

// Get a specific task - GET /api/tasks/:id
['/api/tasks/:id'; get]
pub fn (mut app App) get_task(id string) vweb.Result {
    if task := app.tasks[id] {
        return app.json(task_to_response(task))
    }
    
    app.set_status(404, '')
    return app.json(ErrorResponse{
        error: 'Task not found'
        code: 404
    })
}

// Create a new task - POST /api/tasks
['/api/tasks'; post]
pub fn (mut app App) create_task() vweb.Result {
    body := app.req.data
    request := json.decode(CreateTaskRequest, body) or {
        app.set_status(400, '')
        return app.json(ErrorResponse{
            error: 'Invalid request body'
            code: 400
        })
    }
    
    priority := parse_priority(request.priority)
    task := app.create_task_internal(
        request.title,
        request.description,
        priority,
        request.tags,
        request.assigned_to
    )
    
    app.set_status(201, '')
    return app.json(task_to_response(task))
}

// Update a task - PUT /api/tasks/:id
['/api/tasks/:id'; put]
pub fn (mut app App) update_task(id string) vweb.Result {
    mut task := app.tasks[id] or {
        app.set_status(404, '')
        return app.json(ErrorResponse{
            error: 'Task not found'
            code: 404
        })
    }
    
    body := app.req.data
    request := json.decode(UpdateTaskRequest, body) or {
        app.set_status(400, '')
        return app.json(ErrorResponse{
            error: 'Invalid request body'
            code: 400
        })
    }
    
    // Update fields
    if request.title != '' {
        task.title = request.title
    }
    if request.description != '' {
        task.description = request.description
    }
    if request.status != '' {
        task.status = parse_status(request.status)
    }
    if request.priority != '' {
        task.priority = parse_priority(request.priority)
    }
    if request.tags.len > 0 {
        task.tags = request.tags
    }
    if request.assigned_to != '' {
        task.assigned_to = request.assigned_to
    }
    
    task.updated_at = time.now().unix_milli()
    app.tasks[id] = task
    
    return app.json(task_to_response(task))
}

// Update task status - PATCH /api/tasks/:id/status
['/api/tasks/:id/status'; patch]
pub fn (mut app App) update_task_status(id string) vweb.Result {
    mut task := app.tasks[id] or {
        app.set_status(404, '')
        return app.json(ErrorResponse{
            error: 'Task not found'
            code: 404
        })
    }
    
    body := app.req.data
    request := json.decode(UpdateStatusRequest, body) or {
        app.set_status(400, '')
        return app.json(ErrorResponse{
            error: 'Invalid request body'
            code: 400
        })
    }
    
    task.status = parse_status(request.status)
    task.updated_at = time.now().unix_milli()
    app.tasks[id] = task
    
    return app.json(task_to_response(task))
}

// Delete a task - DELETE /api/tasks/:id
['/api/tasks/:id'; delete]
pub fn (mut app App) delete_task(id string) vweb.Result {
    if _ := app.tasks[id] {
        app.tasks.delete(id)
        app.set_status(204, '')
        return app.ok('')
    }
    
    app.set_status(404, '')
    return app.json(ErrorResponse{
        error: 'Task not found'
        code: 404
    })
}

// Internal helper functions

fn (mut app App) create_task_internal(title string, description string, priority TaskPriority, tags []string, assigned_to string) Task {
    app.counter++
    task_id := 'task_${app.counter}'
    now := time.now().unix_milli()
    
    task := Task{
        id: task_id
        title: title
        description: description
        status: .pending
        priority: priority
        tags: tags
        assigned_to: assigned_to
        created_at: now
        updated_at: now
    }
    
    app.tasks[task_id] = task
    return task
}

fn task_to_response(task Task) TaskResponse {
    return TaskResponse{
        id: task.id
        title: task.title
        description: task.description
        status: task.status.str()
        priority: priority_to_string(task.priority)
        tags: task.tags
        assigned_to: task.assigned_to
        created_at: f64(task.created_at) / 1000.0
        updated_at: f64(task.updated_at) / 1000.0
    }
}

fn parse_status(status string) TaskStatus {
    return match status.to_lower() {
        'pending' { .pending }
        'in-progress', 'in_progress' { .in_progress }
        'completed' { .completed }
        'cancelled' { .cancelled }
        else { .pending }
    }
}

fn parse_priority(priority string) TaskPriority {
    return match priority.to_lower() {
        'low' { .low }
        'medium' { .medium }
        'high' { .high }
        'urgent' { .urgent }
        else { .medium }
    }
}

fn priority_to_string(priority TaskPriority) string {
    return match priority {
        .low { 'low' }
        .medium { 'medium' }
        .high { 'high' }
        .urgent { 'urgent' }
    }
}

fn (status TaskStatus) str() string {
    return match status {
        .pending { 'pending' }
        .in_progress { 'in-progress' }
        .completed { 'completed' }
        .cancelled { 'cancelled' }
    }
}