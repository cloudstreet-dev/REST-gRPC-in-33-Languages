import jester
import json
import strutils
import times
import tables
import locks
import uuid4
import options

type
  TaskStatus* = enum
    tsPending = "pending"
    tsInProgress = "in-progress"
    tsCompleted = "completed"
    tsCancelled = "cancelled"

  TaskPriority* = enum
    tpLow = "low"
    tpMedium = "medium"
    tpHigh = "high"
    tpUrgent = "urgent"

  Task* = object
    id*: string
    title*: string
    description*: Option[string]
    status*: TaskStatus
    priority*: TaskPriority
    tags*: seq[string]
    assignedTo*: Option[string]
    createdAt*: float
    updatedAt*: float

  CreateTaskRequest* = object
    title*: string
    description*: Option[string]
    priority*: Option[string]
    tags*: Option[seq[string]]
    assignedTo*: Option[string]

  UpdateTaskRequest* = object
    title*: Option[string]
    description*: Option[string]
    status*: Option[string]
    priority*: Option[string]
    tags*: Option[seq[string]]
    assignedTo*: Option[string]

  UpdateStatusRequest* = object
    status*: string

  TaskListResponse* = object
    tasks*: seq[Task]
    totalCount*: int

  ErrorResponse* = object
    error*: string

# Global task store with thread safety
var 
  taskStore = initTable[string, Task]()
  taskLock: Lock
  taskCounter = 0

initLock(taskLock)

proc generateTaskId(): string =
  withLock taskLock:
    inc taskCounter
    result = "task-" & $taskCounter

proc parseTaskStatus(s: string): Option[TaskStatus] =
  case s.toLowerAscii()
  of "pending": some(tsPending)
  of "in-progress", "in_progress": some(tsInProgress)
  of "completed": some(tsCompleted)
  of "cancelled": some(tsCancelled)
  else: none(TaskStatus)

proc parseTaskPriority(s: string): Option[TaskPriority] =
  case s.toLowerAscii()
  of "low": some(tpLow)
  of "medium": some(tpMedium)
  of "high": some(tpHigh)
  of "urgent": some(tpUrgent)
  else: none(TaskPriority)

proc taskToJson(task: Task): JsonNode =
  result = %* {
    "id": task.id,
    "title": task.title,
    "status": $task.status,
    "priority": $task.priority,
    "tags": task.tags,
    "created_at": task.createdAt,
    "updated_at": task.updatedAt
  }
  
  if task.description.isSome:
    result["description"] = %task.description.get()
  
  if task.assignedTo.isSome:
    result["assigned_to"] = %task.assignedTo.get()

proc createTask(req: CreateTaskRequest): Task =
  let now = epochTime()
  result = Task(
    id: generateTaskId(),
    title: req.title,
    description: req.description,
    status: tsPending,
    priority: if req.priority.isSome: 
                parseTaskPriority(req.priority.get()).get(tpMedium)
              else: 
                tpMedium,
    tags: if req.tags.isSome: req.tags.get() else: @[],
    assignedTo: req.assignedTo,
    createdAt: now,
    updatedAt: now
  )
  
  withLock taskLock:
    taskStore[result.id] = result

proc getTask(id: string): Option[Task] =
  withLock taskLock:
    if taskStore.hasKey(id):
      result = some(taskStore[id])
    else:
      result = none(Task)

proc updateTask(id: string, updates: UpdateTaskRequest): Option[Task] =
  withLock taskLock:
    if not taskStore.hasKey(id):
      return none(Task)
    
    var task = taskStore[id]
    
    if updates.title.isSome:
      task.title = updates.title.get()
    
    if updates.description.isSome:
      task.description = updates.description
    
    if updates.status.isSome:
      let status = parseTaskStatus(updates.status.get())
      if status.isSome:
        task.status = status.get()
    
    if updates.priority.isSome:
      let priority = parseTaskPriority(updates.priority.get())
      if priority.isSome:
        task.priority = priority.get()
    
    if updates.tags.isSome:
      task.tags = updates.tags.get()
    
    if updates.assignedTo.isSome:
      task.assignedTo = updates.assignedTo
    
    task.updatedAt = epochTime()
    taskStore[id] = task
    result = some(task)

proc updateTaskStatus(id: string, status: TaskStatus): Option[Task] =
  withLock taskLock:
    if not taskStore.hasKey(id):
      return none(Task)
    
    var task = taskStore[id]
    task.status = status
    task.updatedAt = epochTime()
    taskStore[id] = task
    result = some(task)

proc deleteTask(id: string): bool =
  withLock taskLock:
    if taskStore.hasKey(id):
      taskStore.del(id)
      result = true
    else:
      result = false

proc listTasks(statusFilter: Option[string] = none(string), 
               assignedToFilter: Option[string] = none(string)): seq[Task] =
  result = @[]
  
  withLock taskLock:
    for task in taskStore.values:
      var include = true
      
      if statusFilter.isSome:
        let status = parseTaskStatus(statusFilter.get())
        if status.isSome and task.status != status.get():
          include = false
      
      if assignedToFilter.isSome and include:
        if task.assignedTo.isNone or task.assignedTo.get() != assignedToFilter.get():
          include = false
      
      if include:
        result.add(task)
  
  # Sort by creation time (newest first)
  result.sort do (a, b: Task) -> int:
    if a.createdAt > b.createdAt: -1
    elif a.createdAt < b.createdAt: 1
    else: 0

# Initialize with sample data
proc initSampleData() =
  discard createTask(CreateTaskRequest(
    title: "Learn Nim",
    description: some("Master systems programming with Nim"),
    priority: some("high"),
    tags: some(@["nim", "learning", "systems"]),
    assignedTo: some("developer")
  ))
  
  discard createTask(CreateTaskRequest(
    title: "Build REST API",
    description: some("Create REST API with Jester framework"),
    priority: some("urgent"),
    tags: some(@["nim", "api", "rest", "jester"]),
    assignedTo: some("backend-team")
  ))
  
  discard createTask(CreateTaskRequest(
    title: "Write Tests",
    description: some("Add unit tests with unittest2"),
    priority: some("medium"),
    tags: some(@["testing", "quality"]),
    assignedTo: none(string)
  ))

# Initialize sample data on startup
initSampleData()

# Routes
routes:
  # Health check
  get "/health":
    resp %* {
      "status": "healthy",
      "service": "task-api",
      "version": "1.0.0",
      "timestamp": epochTime()
    }
  
  # List tasks
  get "/api/tasks":
    let statusFilter = 
      if request.params.hasKey("status"):
        some(request.params["status"])
      else:
        none(string)
    
    let assignedToFilter = 
      if request.params.hasKey("assigned_to"):
        some(request.params["assigned_to"])
      else:
        none(string)
    
    let tasks = listTasks(statusFilter, assignedToFilter)
    let response = TaskListResponse(
      tasks: tasks,
      totalCount: tasks.len
    )
    
    var jsonTasks = newJArray()
    for task in tasks:
      jsonTasks.add(taskToJson(task))
    
    resp %* {
      "tasks": jsonTasks,
      "total_count": tasks.len
    }
  
  # Get task by ID
  get "/api/tasks/@id":
    let taskOpt = getTask(@"id")
    if taskOpt.isSome:
      resp taskToJson(taskOpt.get())
    else:
      resp Http404, %* {"error": "Task not found"}
  
  # Create task
  post "/api/tasks":
    try:
      let body = parseJson(request.body)
      
      if not body.hasKey("title"):
        resp Http400, %* {"error": "Missing required field: title"}
        return
      
      let req = CreateTaskRequest(
        title: body["title"].getStr(),
        description: if body.hasKey("description"): 
                       some(body["description"].getStr()) 
                     else: 
                       none(string),
        priority: if body.hasKey("priority"): 
                    some(body["priority"].getStr()) 
                  else: 
                    none(string),
        tags: if body.hasKey("tags"):
                some(body["tags"].getElems().mapIt(it.getStr()))
              else:
                none(seq[string]),
        assignedTo: if body.hasKey("assigned_to"):
                      some(body["assigned_to"].getStr())
                    else:
                      none(string)
      )
      
      let task = createTask(req)
      resp Http201, taskToJson(task)
    except:
      resp Http400, %* {"error": "Invalid JSON"}
  
  # Update task
  put "/api/tasks/@id":
    try:
      let body = parseJson(request.body)
      let updates = UpdateTaskRequest(
        title: if body.hasKey("title"): 
                 some(body["title"].getStr()) 
               else: 
                 none(string),
        description: if body.hasKey("description"): 
                       some(body["description"].getStr()) 
                     else: 
                       none(string),
        status: if body.hasKey("status"): 
                  some(body["status"].getStr()) 
                else: 
                  none(string),
        priority: if body.hasKey("priority"): 
                    some(body["priority"].getStr()) 
                  else: 
                    none(string),
        tags: if body.hasKey("tags"):
                some(body["tags"].getElems().mapIt(it.getStr()))
              else:
                none(seq[string]),
        assignedTo: if body.hasKey("assigned_to"):
                      some(body["assigned_to"].getStr())
                    else:
                      none(string)
      )
      
      let taskOpt = updateTask(@"id", updates)
      if taskOpt.isSome:
        resp taskToJson(taskOpt.get())
      else:
        resp Http404, %* {"error": "Task not found"}
    except:
      resp Http400, %* {"error": "Invalid JSON"}
  
  # Update task status
  patch "/api/tasks/@id/status":
    try:
      let body = parseJson(request.body)
      
      if not body.hasKey("status"):
        resp Http400, %* {"error": "Missing required field: status"}
        return
      
      let statusStr = body["status"].getStr()
      let statusOpt = parseTaskStatus(statusStr)
      
      if statusOpt.isNone:
        resp Http400, %* {"error": "Invalid status value"}
        return
      
      let taskOpt = updateTaskStatus(@"id", statusOpt.get())
      if taskOpt.isSome:
        resp taskToJson(taskOpt.get())
      else:
        resp Http404, %* {"error": "Task not found"}
    except:
      resp Http400, %* {"error": "Invalid JSON"}
  
  # Delete task
  delete "/api/tasks/@id":
    if deleteTask(@"id"):
      resp Http204, ""
    else:
      resp Http404, %* {"error": "Task not found"}

when isMainModule:
  echo "🚀 Nim Task REST API Server"
  echo "📍 Listening on http://localhost:8080"
  echo "🔍 Health check: http://localhost:8080/health\n"