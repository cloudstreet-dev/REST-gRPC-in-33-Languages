import puppy
import json
import strformat
import options
import os
import strutils

type
  TaskClient* = object
    baseUrl: string
  
  Task* = object
    id*: string
    title*: string
    description*: Option[string]
    status*: string
    priority*: string
    tags*: seq[string]
    assignedTo*: Option[string]
    createdAt*: float
    updatedAt*: float

proc newTaskClient*(baseUrl: string = "http://localhost:8080"): TaskClient =
  TaskClient(baseUrl: baseUrl)

proc listTasks*(client: TaskClient, status: string = "", assignedTo: string = ""): seq[Task] =
  var url = client.baseUrl & "/api/tasks"
  var params: seq[string] = @[]
  
  if status != "":
    params.add("status=" & status)
  if assignedTo != "":
    params.add("assigned_to=" & assignedTo)
  
  if params.len > 0:
    url = url & "?" & params.join("&")
  
  let response = fetch(url)
  if response.code == 200:
    let data = parseJson(response.body)
    result = @[]
    for taskJson in data["tasks"]:
      var task = Task(
        id: taskJson["id"].getStr(),
        title: taskJson["title"].getStr(),
        status: taskJson["status"].getStr(),
        priority: taskJson["priority"].getStr(),
        tags: taskJson["tags"].getElems().mapIt(it.getStr()),
        createdAt: taskJson["created_at"].getFloat(),
        updatedAt: taskJson["updated_at"].getFloat()
      )
      
      if taskJson.hasKey("description"):
        task.description = some(taskJson["description"].getStr())
      else:
        task.description = none(string)
      
      if taskJson.hasKey("assigned_to"):
        task.assignedTo = some(taskJson["assigned_to"].getStr())
      else:
        task.assignedTo = none(string)
      
      result.add(task)
  else:
    result = @[]

proc getTask*(client: TaskClient, id: string): Option[Task] =
  let url = client.baseUrl & "/api/tasks/" & id
  let response = fetch(url)
  
  if response.code == 200:
    let taskJson = parseJson(response.body)
    var task = Task(
      id: taskJson["id"].getStr(),
      title: taskJson["title"].getStr(),
      status: taskJson["status"].getStr(),
      priority: taskJson["priority"].getStr(),
      tags: taskJson["tags"].getElems().mapIt(it.getStr()),
      createdAt: taskJson["created_at"].getFloat(),
      updatedAt: taskJson["updated_at"].getFloat()
    )
    
    if taskJson.hasKey("description"):
      task.description = some(taskJson["description"].getStr())
    else:
      task.description = none(string)
    
    if taskJson.hasKey("assigned_to"):
      task.assignedTo = some(taskJson["assigned_to"].getStr())
    else:
      task.assignedTo = none(string)
    
    result = some(task)
  else:
    result = none(Task)

proc createTask*(client: TaskClient, title: string, description: string = "", 
                priority: string = "medium", tags: seq[string] = @[], 
                assignedTo: string = ""): Option[Task] =
  let url = client.baseUrl & "/api/tasks"
  var body = %* {
    "title": title,
    "priority": priority,
    "tags": tags
  }
  
  if description != "":
    body["description"] = %description
  
  if assignedTo != "":
    body["assigned_to"] = %assignedTo
  
  let response = fetch(
    url,
    verb = "post",
    headers = @[Header(key: "Content-Type", value: "application/json")],
    body = $body
  )
  
  if response.code == 201:
    let taskJson = parseJson(response.body)
    var task = Task(
      id: taskJson["id"].getStr(),
      title: taskJson["title"].getStr(),
      status: taskJson["status"].getStr(),
      priority: taskJson["priority"].getStr(),
      tags: taskJson["tags"].getElems().mapIt(it.getStr()),
      createdAt: taskJson["created_at"].getFloat(),
      updatedAt: taskJson["updated_at"].getFloat()
    )
    
    if taskJson.hasKey("description"):
      task.description = some(taskJson["description"].getStr())
    else:
      task.description = none(string)
    
    if taskJson.hasKey("assigned_to"):
      task.assignedTo = some(taskJson["assigned_to"].getStr())
    else:
      task.assignedTo = none(string)
    
    result = some(task)
  else:
    result = none(Task)

proc updateTask*(client: TaskClient, id: string, title: string = "", 
                description: string = "", status: string = "", 
                priority: string = "", tags: seq[string] = @[],
                assignedTo: string = ""): Option[Task] =
  let url = client.baseUrl & "/api/tasks/" & id
  var body = newJObject()
  
  if title != "":
    body["title"] = %title
  if description != "":
    body["description"] = %description
  if status != "":
    body["status"] = %status
  if priority != "":
    body["priority"] = %priority
  if tags.len > 0:
    body["tags"] = %tags
  if assignedTo != "":
    body["assigned_to"] = %assignedTo
  
  let response = fetch(
    url,
    verb = "put",
    headers = @[Header(key: "Content-Type", value: "application/json")],
    body = $body
  )
  
  if response.code == 200:
    let taskJson = parseJson(response.body)
    var task = Task(
      id: taskJson["id"].getStr(),
      title: taskJson["title"].getStr(),
      status: taskJson["status"].getStr(),
      priority: taskJson["priority"].getStr(),
      tags: taskJson["tags"].getElems().mapIt(it.getStr()),
      createdAt: taskJson["created_at"].getFloat(),
      updatedAt: taskJson["updated_at"].getFloat()
    )
    
    if taskJson.hasKey("description"):
      task.description = some(taskJson["description"].getStr())
    else:
      task.description = none(string)
    
    if taskJson.hasKey("assigned_to"):
      task.assignedTo = some(taskJson["assigned_to"].getStr())
    else:
      task.assignedTo = none(string)
    
    result = some(task)
  else:
    result = none(Task)

proc updateTaskStatus*(client: TaskClient, id: string, status: string): Option[Task] =
  let url = client.baseUrl & "/api/tasks/" & id & "/status"
  let body = %* {"status": status}
  
  let response = fetch(
    url,
    verb = "patch",
    headers = @[Header(key: "Content-Type", value: "application/json")],
    body = $body
  )
  
  if response.code == 200:
    let taskJson = parseJson(response.body)
    var task = Task(
      id: taskJson["id"].getStr(),
      title: taskJson["title"].getStr(),
      status: taskJson["status"].getStr(),
      priority: taskJson["priority"].getStr(),
      tags: taskJson["tags"].getElems().mapIt(it.getStr()),
      createdAt: taskJson["created_at"].getFloat(),
      updatedAt: taskJson["updated_at"].getFloat()
    )
    
    if taskJson.hasKey("description"):
      task.description = some(taskJson["description"].getStr())
    else:
      task.description = none(string)
    
    if taskJson.hasKey("assigned_to"):
      task.assignedTo = some(taskJson["assigned_to"].getStr())
    else:
      task.assignedTo = none(string)
    
    result = some(task)
  else:
    result = none(Task)

proc deleteTask*(client: TaskClient, id: string): bool =
  let url = client.baseUrl & "/api/tasks/" & id
  let response = fetch(url, verb = "delete")
  result = response.code == 204

proc printBanner() =
  echo "╔════════════════════════════════════════════════╗"
  echo "║        Nim Task Management REST Client         ║"
  echo "║            Testing API Operations              ║"
  echo "╚════════════════════════════════════════════════╝"
  echo ""

proc runDemo() =
  let client = newTaskClient()
  
  # 1. List all tasks
  echo "1. Listing all tasks..."
  let tasks = client.listTasks()
  echo fmt"   Found {tasks.len} tasks"
  for task in tasks:
    echo fmt"   - [{task.id}] {task.title} ({task.status})"
  
  # 2. Create a new task
  echo "\n2. Creating a new task..."
  let newTaskOpt = client.createTask(
    title = "Learn Nim metaprogramming",
    description = "Master macros and compile-time execution",
    priority = "high",
    tags = @["nim", "metaprogramming", "macros"],
    assignedTo = "nim-team"
  )
  
  if newTaskOpt.isSome:
    let task = newTaskOpt.get()
    echo fmt"   Created task: {task.title}"
    echo fmt"   ID: {task.id}"
    echo fmt"   Priority: {task.priority}"
    echo fmt"   Tags: {task.tags.join(\", \")}"
    
    # 3. Get task details
    echo "\n3. Getting task details..."
    let taskDetailOpt = client.getTask(task.id)
    if taskDetailOpt.isSome:
      let taskDetail = taskDetailOpt.get()
      echo fmt"   Title: {taskDetail.title}"
      if taskDetail.description.isSome:
        echo fmt"   Description: {taskDetail.description.get()}"
      echo fmt"   Status: {taskDetail.status}"
      if taskDetail.assignedTo.isSome:
        echo fmt"   Assigned to: {taskDetail.assignedTo.get()}"
    
    # 4. Update task status
    echo "\n4. Updating task status to 'in-progress'..."
    let statusUpdateOpt = client.updateTaskStatus(task.id, "in-progress")
    if statusUpdateOpt.isSome:
      echo fmt"   Updated status to: {statusUpdateOpt.get().status}"
    
    # 5. Update task details
    echo "\n5. Updating task details..."
    let updateOpt = client.updateTask(
      task.id,
      title = "Master Nim async and parallelism",
      priority = "urgent"
    )
    if updateOpt.isSome:
      let updated = updateOpt.get()
      echo fmt"   Updated title: {updated.title}"
      echo fmt"   Updated priority: {updated.priority}"
    
    # 6. Filter tasks by status
    echo "\n6. Filtering tasks by status..."
    let inProgressTasks = client.listTasks(status = "in-progress")
    echo fmt"   Found {inProgressTasks.len} in-progress tasks"
    for t in inProgressTasks:
      echo fmt"   - {t.title}"
    
    # 7. Delete the task
    echo "\n7. Deleting the task..."
    if client.deleteTask(task.id):
      echo "   Task deleted successfully"
    else:
      echo "   Error: Failed to delete task"
    
    # 8. Verify deletion
    echo "\n8. Verifying deletion..."
    let verifyOpt = client.getTask(task.id)
    if verifyOpt.isNone:
      echo "   Task not found (as expected)"
    else:
      echo "   Error: Task still exists"
  else:
    echo "   Error: Failed to create task"
  
  echo "\n✅ Demo completed successfully!"

when isMainModule:
  printBanner()
  sleep(1000)  # Give server time to be ready
  runDemo()