module main

import net.http
import json
import time

struct Task {
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
    tasks []Task `json:"tasks"`
    count int    `json:"count"`
}

struct CreateTaskRequest {
    title       string   `json:"title"`
    description string   `json:"description"`
    priority    string   `json:"priority"`
    tags        []string `json:"tags"`
    assigned_to string   `json:"assigned_to"`
}

struct UpdateTaskRequest {
    title       string   `json:"title"`
    description string   `json:"description"`
    status      string   `json:"status"`
    priority    string   `json:"priority"`
    tags        []string `json:"tags"`
    assigned_to string   `json:"assigned_to"`
}

struct UpdateStatusRequest {
    status string `json:"status"`
}

struct TaskClient {
    base_url string
}

fn new_client(base_url string) TaskClient {
    return TaskClient{
        base_url: base_url
    }
}

fn (c TaskClient) list_tasks(status string, assigned_to string) !ListTasksResponse {
    mut url := '${c.base_url}/api/tasks'
    mut params := []string{}
    
    if status != '' {
        params << 'status=${status}'
    }
    if assigned_to != '' {
        params << 'assigned_to=${assigned_to}'
    }
    
    if params.len > 0 {
        url += '?' + params.join('&')
    }
    
    resp := http.get(url)!
    if resp.status_code != 200 {
        return error('Failed to list tasks: ${resp.status_code}')
    }
    
    return json.decode(ListTasksResponse, resp.body)!
}

fn (c TaskClient) get_task(id string) !Task {
    url := '${c.base_url}/api/tasks/${id}'
    resp := http.get(url)!
    
    if resp.status_code == 404 {
        return error('Task not found')
    }
    if resp.status_code != 200 {
        return error('Failed to get task: ${resp.status_code}')
    }
    
    return json.decode(Task, resp.body)!
}

fn (c TaskClient) create_task(request CreateTaskRequest) !Task {
    url := '${c.base_url}/api/tasks'
    body := json.encode(request)
    
    resp := http.post_json(url, body)!
    if resp.status_code != 201 {
        return error('Failed to create task: ${resp.status_code}')
    }
    
    return json.decode(Task, resp.body)!
}

fn (c TaskClient) update_task(id string, request UpdateTaskRequest) !Task {
    url := '${c.base_url}/api/tasks/${id}'
    body := json.encode(request)
    
    mut req := http.Request{
        url: url
        method: .put
        data: body
        headers: {
            'Content-Type': 'application/json'
        }
    }
    
    resp := req.do()!
    if resp.status_code == 404 {
        return error('Task not found')
    }
    if resp.status_code != 200 {
        return error('Failed to update task: ${resp.status_code}')
    }
    
    return json.decode(Task, resp.body)!
}

fn (c TaskClient) update_task_status(id string, status string) !Task {
    url := '${c.base_url}/api/tasks/${id}/status'
    body := json.encode(UpdateStatusRequest{status: status})
    
    mut req := http.Request{
        url: url
        method: .patch
        data: body
        headers: {
            'Content-Type': 'application/json'
        }
    }
    
    resp := req.do()!
    if resp.status_code == 404 {
        return error('Task not found')
    }
    if resp.status_code != 200 {
        return error('Failed to update status: ${resp.status_code}')
    }
    
    return json.decode(Task, resp.body)!
}

fn (c TaskClient) delete_task(id string) ! {
    url := '${c.base_url}/api/tasks/${id}'
    
    mut req := http.Request{
        url: url
        method: .delete
    }
    
    resp := req.do()!
    if resp.status_code == 404 {
        return error('Task not found')
    }
    if resp.status_code != 204 {
        return error('Failed to delete task: ${resp.status_code}')
    }
}

fn print_banner() {
    println('╔════════════════════════════════════════════════╗')
    println('║         V Task Management REST Client          ║')
    println('║            Testing API Operations              ║')
    println('╚════════════════════════════════════════════════╝')
    println('')
}

fn run_demo(client TaskClient) {
    // 1. List all tasks
    println('1. Listing all tasks...')
    list_resp := client.list_tasks('', '') or {
        println('   Error: ${err}')
        return
    }
    println('   Found ${list_resp.count} tasks')
    for task in list_resp.tasks {
        println('   - [${task.id}] ${task.title} (${task.status})')
    }
    
    // 2. Create a new task
    println('\n2. Creating a new task...')
    create_req := CreateTaskRequest{
        title: 'Learn V language patterns'
        description: 'Master V\'s unique features and idioms'
        priority: 'high'
        tags: ['v', 'learning', 'patterns']
        assigned_to: 'v-team'
    }
    
    new_task := client.create_task(create_req) or {
        println('   Error: ${err}')
        return
    }
    println('   Created task: ${new_task.title}')
    println('   ID: ${new_task.id}')
    println('   Priority: ${new_task.priority}')
    println('   Tags: ${new_task.tags.join(', ')}')
    
    task_id := new_task.id
    
    // 3. Get task details
    println('\n3. Getting task details...')
    task_detail := client.get_task(task_id) or {
        println('   Error: ${err}')
        return
    }
    println('   Title: ${task_detail.title}')
    println('   Description: ${task_detail.description}')
    println('   Status: ${task_detail.status}')
    println('   Assigned to: ${task_detail.assigned_to}')
    
    // 4. Update task status
    println('\n4. Updating task status to \'in-progress\'...')
    status_update := client.update_task_status(task_id, 'in-progress') or {
        println('   Error: ${err}')
        return
    }
    println('   Updated status to: ${status_update.status}')
    
    // 5. Update task details
    println('\n5. Updating task details...')
    update_req := UpdateTaskRequest{
        title: 'Master V language and vweb'
        description: ''
        status: ''
        priority: 'urgent'
        tags: []string{}
        assigned_to: ''
    }
    
    task_update := client.update_task(task_id, update_req) or {
        println('   Error: ${err}')
        return
    }
    println('   Updated title: ${task_update.title}')
    println('   Updated priority: ${task_update.priority}')
    
    // 6. Filter tasks by status
    println('\n6. Filtering tasks by status...')
    filtered := client.list_tasks('in-progress', '') or {
        println('   Error: ${err}')
        return
    }
    println('   Found ${filtered.count} in-progress tasks')
    for task in filtered.tasks {
        println('   - ${task.title}')
    }
    
    // 7. Delete the task
    println('\n7. Deleting the task...')
    client.delete_task(task_id) or {
        println('   Error: ${err}')
        return
    }
    println('   Task deleted successfully')
    
    // 8. Verify deletion
    println('\n8. Verifying deletion...')
    client.get_task(task_id) or {
        if err.msg().contains('not found') {
            println('   Task not found (as expected)')
        } else {
            println('   Error: ${err}')
        }
        return
    }
}

fn main() {
    print_banner()
    
    // Wait a moment for server to be ready
    time.sleep(1 * time.second)
    
    client := new_client('http://localhost:8080')
    run_demo(client)
    
    println('\n✅ Demo completed successfully!')
}