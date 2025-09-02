# Chapter 16: Scala - Functional Meets Object-Oriented

Scala represents a unique fusion of functional and object-oriented programming paradigms, running on the JVM with seamless Java interoperability. Created by Martin Odersky in 2003, Scala has become a powerhouse for building scalable, distributed systems and data processing pipelines. With its expressive type system, pattern matching, and actor-based concurrency model, Scala excels at building robust APIs that can handle massive scale.

This chapter explores how Scala's unique features—from its powerful type system to its functional programming capabilities—make it an excellent choice for building both REST APIs with Akka HTTP and gRPC services with ScalaPB.

## Table of Contents

- [Scala Language Overview](#scala-language-overview)
- [Setting Up the Development Environment](#setting-up-the-development-environment)
- [REST API Implementation with Akka HTTP](#rest-api-implementation-with-akka-http)
- [gRPC Implementation with ScalaPB](#grpc-implementation-with-scalapb)
- [Building and Running the Services](#building-and-running-the-services)
- [Testing the Implementations](#testing-the-implementations)
- [Scala-Specific Features and Patterns](#scala-specific-features-and-patterns)
- [Performance Considerations](#performance-considerations)
- [Deployment Strategies](#deployment-strategies)
- [Conclusion](#conclusion)

## Scala Language Overview

Scala combines the best of both functional and object-oriented programming paradigms, offering developers a powerful and expressive language for building scalable applications.

### Key Language Features

**Type System Excellence**
- Static typing with powerful type inference
- Higher-kinded types for advanced abstractions
- Implicit parameters and conversions
- Path-dependent types and type members

**Functional Programming**
- First-class functions and higher-order functions
- Immutable data structures by default
- Pattern matching with case classes
- For-comprehensions for monadic operations

**Object-Oriented Programming**
- Traits for flexible composition
- Singleton objects instead of static members
- Uniform access principle
- Self-type annotations for dependency injection

**Concurrency and Distribution**
- Actor model with Akka for message-passing concurrency
- Futures and Promises for asynchronous programming
- Parallel collections for data parallelism
- Reactive Streams for backpressure handling

### Scala in API Development

Scala excels in API development through:
- **Scalability**: Actor model enables massive concurrency
- **Type Safety**: Catch errors at compile time
- **Expressiveness**: Concise syntax reduces boilerplate
- **Performance**: JVM optimization and efficient collections
- **Ecosystem**: Rich libraries for web, data, and distributed systems

## Setting Up the Development Environment

### Prerequisites

Before building Scala APIs, ensure you have:

**Required Tools**
- JDK 11 or later (OpenJDK or Oracle JDK)
- SBT (Scala Build Tool) 1.9.0 or later
- Scala 2.13.12 (or 3.3+ for Scala 3)

**IDE Options**
- IntelliJ IDEA with Scala plugin (recommended)
- VS Code with Metals extension
- Emacs with ENSIME or Metals

### Installation Commands

**macOS (using Homebrew)**
```bash
brew install openjdk@11
brew install sbt
brew install scala
```

**Ubuntu/Debian**
```bash
sudo apt update
sudo apt install openjdk-11-jdk
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt update
sudo apt install sbt
```

**Windows (using Chocolatey)**
```powershell
choco install openjdk11
choco install sbt
choco install scala
```

### Project Structure

Our Scala implementation follows SBT's standard directory layout:

```
scala/
├── rest/
│   ├── server/
│   │   ├── build.sbt              # Build configuration
│   │   ├── project/
│   │   │   └── plugins.sbt        # SBT plugins
│   │   └── src/
│   │       └── main/
│   │           ├── scala/
│   │           │   └── com/example/taskapi/
│   │           │       ├── Main.scala
│   │           │       ├── models/
│   │           │       │   └── Task.scala
│   │           │       ├── db/
│   │           │       │   └── TaskRepository.scala
│   │           │       └── routes/
│   │           │           └── TaskRoutes.scala
│   │           └── resources/
│   │               └── application.conf
│   └── client/
│       ├── build.sbt
│       └── src/main/scala/
├── grpc/
│   ├── server/
│   │   ├── build.sbt
│   │   └── src/main/scala/
│   └── client/
│       ├── build.sbt
│       └── src/main/scala/
└── README.md
```

## REST API Implementation with Akka HTTP

Akka HTTP provides a powerful, type-safe, and actor-based foundation for building REST APIs in Scala.

### Build Configuration

```scala
// build.sbt
name := "task-rest-server"
version := "1.0.0"
scalaVersion := "2.13.12"

val AkkaVersion = "2.8.5"
val AkkaHttpVersion = "10.5.3"
val CirceVersion = "0.14.6"
val SlickVersion = "3.4.1"

libraryDependencies ++= Seq(
  // Akka HTTP for REST API
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  
  // JSON handling with Circe
  "io.circe" %% "circe-core" % CirceVersion,
  "io.circe" %% "circe-generic" % CirceVersion,
  "io.circe" %% "circe-parser" % CirceVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.39.2",
  
  // Database with Slick
  "com.typesafe.slick" %% "slick" % SlickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % SlickVersion,
  "com.h2database" % "h2" % "2.2.224",
  
  // Testing
  "com.typesafe.akka" %% "akka-http-testkit" % AkkaHttpVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.17" % Test
)
```

### Data Models with Case Classes

Scala's case classes provide immutable data structures with built-in equality, hashing, and pattern matching:

```scala
// models/Task.scala
sealed trait TaskStatus
object TaskStatus {
  case object Pending extends TaskStatus
  case object InProgress extends TaskStatus
  case object Completed extends TaskStatus
  case object Cancelled extends TaskStatus
  
  def fromString(s: String): Option[TaskStatus] = s.toLowerCase match {
    case "pending" => Some(Pending)
    case "in_progress" => Some(InProgress)
    case "completed" => Some(Completed)
    case "cancelled" => Some(Cancelled)
    case _ => None
  }
}

sealed trait TaskPriority
object TaskPriority {
  case object Low extends TaskPriority
  case object Medium extends TaskPriority
  case object High extends TaskPriority
  case object Urgent extends TaskPriority
}

case class Task(
  id: String,
  title: String,
  description: Option[String] = None,
  status: TaskStatus = TaskStatus.Pending,
  priority: TaskPriority = TaskPriority.Medium,
  tags: List[String] = List.empty,
  assignedTo: Option[String] = None,
  createdAt: Instant = Instant.now(),
  updatedAt: Instant = Instant.now()
)

// JSON codecs using Circe
object Task {
  implicit val encoder: Encoder[Task] = deriveEncoder[Task]
  implicit val decoder: Decoder[Task] = deriveDecoder[Task]
}
```

Key Scala features demonstrated:
- **Sealed traits**: Exhaustive pattern matching
- **Case classes**: Immutable data with automatic methods
- **Companion objects**: Factory methods and implicits
- **Option type**: Null-safe optional values

### Database Layer with Slick

Slick provides functional-relational mapping for Scala:

```scala
// db/TaskRepository.scala
class TasksTable(tag: Tag) extends Table[Task](tag, "tasks") {
  def id: Rep[String] = column[String]("id", O.PrimaryKey)
  def title: Rep[String] = column[String]("title")
  def description: Rep[Option[String]] = column[Option[String]]("description")
  def status: Rep[String] = column[String]("status")
  def priority: Rep[String] = column[String]("priority")
  def tags: Rep[String] = column[String]("tags")
  def assignedTo: Rep[Option[String]] = column[Option[String]]("assigned_to")
  def createdAt: Rep[Instant] = column[Instant]("created_at")
  def updatedAt: Rep[Instant] = column[Instant]("updated_at")
  
  // Custom type mappers for enums
  implicit val statusMapper = MappedColumnType.base[TaskStatus, String](
    TaskStatus.toString,
    s => TaskStatus.fromString(s).getOrElse(TaskStatus.Pending)
  )
  
  def * : ProvenShape[Task] = (
    id, title, description, statusTyped, priorityTyped, 
    tagsTyped, assignedTo, createdAt, updatedAt
  ).mapTo[Task]
}

class TaskRepository(db: Database)(implicit ec: ExecutionContext) {
  val tasks = TableQuery[TasksTable]
  
  def list(
    status: Option[TaskStatus] = None,
    assignedTo: Option[String] = None,
    tags: Option[List[String]] = None,
    limit: Int = 20,
    offset: Int = 0
  ): Future[List[Task]] = {
    val query = tasks
      .filterOpt(status)((t, s) => t.statusTyped === s)
      .filterOpt(assignedTo)((t, a) => t.assignedTo === a)
    
    db.run(query.drop(offset).take(limit).result).map(_.toList)
  }
  
  def create(task: Task): Future[Task] = {
    db.run(tasks += task).map(_ => task)
  }
  
  def update(id: String, request: UpdateTaskRequest): Future[Option[Task]] = {
    val updateAction = for {
      existing <- tasks.filter(_.id === id).result.headOption
      result <- existing match {
        case Some(task) =>
          val updated = task.copy(
            title = request.title.getOrElse(task.title),
            status = request.status.getOrElse(task.status),
            updatedAt = Instant.now()
          )
          tasks.filter(_.id === id).update(updated).map(_ => Some(updated))
        case None =>
          DBIO.successful(None)
      }
    } yield result
    
    db.run(updateAction.transactionally)
  }
}
```

### Route Definition with Akka HTTP

Akka HTTP provides a powerful DSL for defining routes:

```scala
// routes/TaskRoutes.scala
class TaskRoutes(repository: TaskRepository)(implicit ec: ExecutionContext) {
  
  val routes: Route = pathPrefix("api" / "tasks") {
    concat(
      // GET /api/tasks - List tasks
      pathEndOrSingleSlash {
        get {
          parameters(
            Symbol("status").as[String].optional,
            Symbol("assigned_to").as[String].optional,
            Symbol("page_size").as[Int].withDefault(20)
          ) { (statusStr, assignedTo, pageSize) =>
            val status = statusStr.flatMap(TaskStatus.fromString)
            
            val futureResult = for {
              tasks <- repository.list(status, assignedTo, limit = pageSize)
              totalCount <- repository.count(status, assignedTo)
            } yield ListTasksResponse(tasks, totalCount, pageSize)
            
            onComplete(futureResult) {
              case Success(response) => complete(response)
              case Failure(ex) => 
                complete(StatusCodes.InternalServerError, 
                  ErrorResponse("internal_error", ex.getMessage, 500))
            }
          }
        }
      },
      
      // POST /api/tasks - Create task
      pathEndOrSingleSlash {
        post {
          entity(as[CreateTaskRequest]) { request =>
            val task = Task.create(request)
            onComplete(repository.create(task)) {
              case Success(createdTask) => 
                complete(StatusCodes.Created, createdTask)
              case Failure(ex) => 
                complete(StatusCodes.InternalServerError,
                  ErrorResponse("internal_error", ex.getMessage, 500))
            }
          }
        }
      },
      
      // Routes with ID parameter
      pathPrefix(Segment) { taskId =>
        concat(
          // GET /api/tasks/{id}
          pathEndOrSingleSlash {
            get {
              onComplete(repository.getById(taskId)) {
                case Success(Some(task)) => complete(task)
                case Success(None) => 
                  complete(StatusCodes.NotFound,
                    ErrorResponse("not_found", s"Task $taskId not found", 404))
                case Failure(ex) => 
                  complete(StatusCodes.InternalServerError,
                    ErrorResponse("internal_error", ex.getMessage, 500))
              }
            }
          },
          
          // PUT /api/tasks/{id}
          pathEndOrSingleSlash {
            put {
              entity(as[UpdateTaskRequest]) { request =>
                onComplete(repository.update(taskId, request)) {
                  case Success(Some(task)) => complete(task)
                  case Success(None) => 
                    complete(StatusCodes.NotFound,
                      ErrorResponse("not_found", s"Task $taskId not found", 404))
                  case Failure(ex) => 
                    complete(StatusCodes.InternalServerError,
                      ErrorResponse("internal_error", ex.getMessage, 500))
                }
              }
            }
          }
        )
      }
    )
  }
}
```

### Main Application with Actor System

```scala
// Main.scala
object Main extends App with LazyLogging {
  
  // Create actor system
  implicit val system: ActorSystem[Nothing] = 
    ActorSystem(Behaviors.empty, "task-api-system")
  implicit val ec: ExecutionContext = system.executionContext
  
  // Initialize database and repository
  val repository = DatabaseConfig.createRepository()
  repository.initialize()
  
  // Create routes
  val taskRoutes = new TaskRoutes(repository)
  
  // Start HTTP server
  val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(taskRoutes.routes)
  
  bindingFuture.onComplete {
    case Success(binding) =>
      val address = binding.localAddress
      logger.info(s"Server online at http://${address.getHostString}:${address.getPort}/")
    case Failure(exception) =>
      logger.error(s"Failed to bind", exception)
      system.terminate()
  }
}
```

## gRPC Implementation with ScalaPB

ScalaPB provides Protocol Buffer and gRPC support for Scala with excellent type safety and functional programming support.

### Build Configuration for gRPC

```scala
// build.sbt
name := "task-grpc-server"
scalaVersion := "2.13.12"

// Enable ScalaPB compilation
Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)

// Proto file location
Compile / PB.protoSources := Seq(
  baseDirectory.value / ".." / ".." / ".." / "shared" / "protos"
)

libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
  "io.grpc" % "grpc-netty-shaded" % grpcVersion,
  "io.grpc" % "grpc-services" % grpcVersion
)

// project/plugins.sbt
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.7")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.14"
```

### gRPC Service Implementation

ScalaPB generates Scala code from Protocol Buffers with excellent type safety:

```scala
// TaskServiceImpl.scala
class TaskServiceImpl(implicit ec: ExecutionContext) extends TaskServiceGrpc.TaskService {
  
  // Thread-safe in-memory storage
  private val tasks = TrieMap[String, Task]()
  
  override def listTasks(request: ListTasksRequest): Future[ListTasksResponse] = Future {
    val allTasks = tasks.values.toSeq
    
    // Apply filters using functional operations
    val filtered = allTasks.filter { task =>
      val statusMatch = request.status.forall(_ == task.status)
      val assignedMatch = request.assignedTo.forall(_ == task.assignedTo)
      val tagsMatch = request.tags.isEmpty || request.tags.forall(task.tags.contains)
      
      statusMatch && assignedMatch && tagsMatch
    }
    
    // Sort and paginate
    val sorted = request.sortBy match {
      case "title" => filtered.sortBy(_.title)
      case _ => filtered.sortBy(_.id)
    }
    
    val pageSize = if (request.pageSize > 0) request.pageSize else 20
    val offset = request.pageToken.map(_.toInt).getOrElse(0)
    val paginated = sorted.slice(offset, offset + pageSize)
    
    ListTasksResponse(
      tasks = paginated,
      totalCount = filtered.length,
      nextPageToken = if (offset + pageSize < filtered.length) 
        Some((offset + pageSize).toString) else None
    )
  }
  
  override def getTask(request: GetTaskRequest): Future[Task] = Future {
    tasks.get(request.id) match {
      case Some(task) => task
      case None => 
        throw Status.NOT_FOUND
          .withDescription(s"Task ${request.id} not found")
          .asException()
    }
  }
  
  override def createTask(request: CreateTaskRequest): Future[Task] = Future {
    require(request.title.nonEmpty, "Title is required")
    
    val task = Task(
      id = UUID.randomUUID().toString,
      title = request.title,
      description = request.description,
      status = TaskStatus.PENDING,
      priority = request.priority.getOrElse(TaskPriority.MEDIUM),
      tags = request.tags,
      assignedTo = request.assignedTo,
      createdAt = Some(Timestamp(Instant.now().getEpochSecond)),
      updatedAt = Some(Timestamp(Instant.now().getEpochSecond))
    )
    
    tasks.put(task.id, task)
    task
  }
  
  // Server streaming for real-time updates
  override def watchTasks(
    request: WatchTasksRequest,
    responseObserver: StreamObserver[TaskUpdate]
  ): Unit = {
    // Send initial state
    val relevantTasks = tasks.values.filter { task =>
      request.taskIds.isEmpty || request.taskIds.contains(task.id)
    }
    
    relevantTasks.foreach { task =>
      responseObserver.onNext(TaskUpdate(
        updateType = UpdateType.CREATED,
        task = Some(task)
      ))
    }
    
    // In production, implement change detection
    responseObserver.onCompleted()
  }
}
```

### gRPC Server Setup

```scala
// Main.scala for gRPC server
object Main extends App with LazyLogging {
  
  implicit val ec: ExecutionContext = ExecutionContext.global
  
  // Create service implementation
  val taskService = new TaskServiceImpl()
  taskService.initialize()
  
  // Build and start server
  val server: Server = ServerBuilder
    .forPort(50051)
    .addService(taskService)
    .addService(ProtoReflectionService.newInstance()) // Enable reflection
    .build()
  
  server.start()
  logger.info(s"gRPC Server started on port 50051")
  
  // Add shutdown hook
  sys.addShutdownHook {
    logger.info("Shutting down gRPC server...")
    server.shutdown()
    server.awaitTermination()
  }
  
  server.awaitTermination()
}
```

### gRPC Client Implementation

```scala
// TaskGrpcClient.scala
class TaskGrpcClient(host: String, port: Int)(implicit ec: ExecutionContext) {
  
  private val channel: ManagedChannel = ManagedChannelBuilder
    .forAddress(host, port)
    .usePlaintext()
    .build()
  
  private val stub = TaskServiceGrpc.stub(channel)
  private val blockingStub = TaskServiceGrpc.blockingStub(channel)
  
  def listTasks(
    status: Option[TaskStatus] = None,
    assignedTo: Option[String] = None,
    tags: Seq[String] = Seq.empty
  ): Future[ListTasksResponse] = {
    val request = ListTasksRequest(
      status = status,
      assignedTo = assignedTo.getOrElse(""),
      tags = tags
    )
    
    stub.listTasks(request)
  }
  
  def createTask(
    title: String,
    description: String = "",
    priority: TaskPriority = TaskPriority.MEDIUM,
    tags: Seq[String] = Seq.empty
  ): Future[Task] = {
    val request = CreateTaskRequest(
      title = title,
      description = description,
      priority = Some(priority),
      tags = tags
    )
    
    stub.createTask(request)
  }
  
  // Watch for updates (server streaming)
  def watchTasks(taskIds: Seq[String] = Seq.empty): Unit = {
    val request = WatchTasksRequest(taskIds = taskIds)
    val responseIterator = blockingStub.watchTasks(request)
    
    while (responseIterator.hasNext) {
      val update = responseIterator.next()
      println(s"[${update.updateType}] Task ${update.task.map(_.id).getOrElse("?")}}")
    }
  }
  
  def shutdown(): Unit = channel.shutdown()
}
```

## Building and Running the Services

### Building with SBT

**REST Server**
```bash
cd code/scala/rest/server
sbt compile
sbt run
# Or create a fat JAR
sbt assembly
java -jar target/scala-2.13/task-rest-server.jar
```

**REST Client**
```bash
cd code/scala/rest/client
sbt "run demo"
# Or with specific commands
sbt "run list --status pending"
sbt "run create 'New Task' --priority high"
```

**gRPC Server**
```bash
cd code/scala/grpc/server
sbt compile  # This will also generate Protocol Buffer code
sbt run
```

**gRPC Client**
```bash
cd code/scala/grpc/client
sbt "run demo"
# Or specific operations
sbt "run list"
sbt "run create 'gRPC Task' --priority HIGH"
```

### Docker Deployment

```dockerfile
# Multi-stage build for Scala
FROM hseeberger/scala-sbt:11.0.15_1.7.1_2.13.8 AS builder

WORKDIR /app
COPY build.sbt .
COPY project project
RUN sbt update

COPY src src
RUN sbt assembly

# Production stage
FROM openjdk:11-jre-slim

WORKDIR /app
COPY --from=builder /app/target/scala-2.13/*-assembly-*.jar app.jar

EXPOSE 8080
CMD ["java", "-jar", "app.jar"]
```

## Testing the Implementations

### Unit Testing with ScalaTest

```scala
// TaskRoutesSpec.scala
class TaskRoutesSpec extends AnyWordSpec with Matchers with ScalatestRouteTest {
  
  val repository = mock[TaskRepository]
  val routes = new TaskRoutes(repository).routes
  
  "TaskRoutes" should {
    "return all tasks for GET /api/tasks" in {
      val tasks = List(
        Task("1", "Test Task", status = TaskStatus.Pending)
      )
      
      when(repository.list(any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(tasks))
      when(repository.count(any(), any(), any()))
        .thenReturn(Future.successful(1))
      
      Get("/api/tasks") ~> routes ~> check {
        status shouldBe StatusCodes.OK
        val response = responseAs[ListTasksResponse]
        response.tasks should have length 1
        response.totalCount shouldBe 1
      }
    }
    
    "create a new task for POST /api/tasks" in {
      val request = CreateTaskRequest("New Task", priority = Some(TaskPriority.High))
      val task = Task.create(request)
      
      when(repository.create(any())).thenReturn(Future.successful(task))
      
      Post("/api/tasks", request) ~> routes ~> check {
        status shouldBe StatusCodes.Created
        val created = responseAs[Task]
        created.title shouldBe "New Task"
        created.priority shouldBe TaskPriority.High
      }
    }
  }
}
```

### Integration Testing

```scala
// IntegrationSpec.scala
class IntegrationSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  
  val system = ActorSystem("test-system")
  implicit val ec = system.dispatcher
  
  val client = new TaskApiClient("http://localhost:8080")
  
  "Task API" should {
    "handle complete CRUD workflow" in {
      // Create
      val createResult = Await.result(
        client.createTask(CreateTaskRequest("Integration Test")),
        5.seconds
      )
      createResult should be a Right
      
      val taskId = createResult.right.get.id
      
      // Read
      val getResult = Await.result(client.getTask(taskId), 5.seconds)
      getResult should be a Right
      getResult.right.get.title shouldBe "Integration Test"
      
      // Update
      val updateResult = Await.result(
        client.updateTask(taskId, UpdateTaskRequest(status = Some(TaskStatus.Completed))),
        5.seconds
      )
      updateResult should be a Right
      updateResult.right.get.status shouldBe TaskStatus.Completed
      
      // Delete
      val deleteResult = Await.result(client.deleteTask(taskId), 5.seconds)
      deleteResult should be a Right
    }
  }
  
  override def afterAll(): Unit = {
    system.terminate()
  }
}
```

## Scala-Specific Features and Patterns

### Pattern Matching

```scala
def processTask(task: Task): String = task.status match {
  case TaskStatus.Pending => 
    s"Task ${task.id} is waiting to be started"
  case TaskStatus.InProgress => 
    s"Task ${task.id} is currently being worked on"
  case TaskStatus.Completed => 
    s"Task ${task.id} has been completed"
  case TaskStatus.Cancelled => 
    s"Task ${task.id} was cancelled"
}

// Pattern matching with guards
def priorityMessage(task: Task): String = task match {
  case Task(_, _, _, _, TaskPriority.Urgent, _, _, _, _) =>
    "This task needs immediate attention!"
  case t if t.priority == TaskPriority.High && t.status == TaskStatus.Pending =>
    "High priority task waiting to be started"
  case _ =>
    "Regular priority task"
}
```

### For Comprehensions

```scala
def processTaskUpdate(id: String, request: UpdateTaskRequest): Future[Either[String, Task]] = {
  for {
    existing <- repository.getById(id)
    validated <- Future.successful(validateUpdate(existing, request))
    updated <- validated match {
      case Right(task) => repository.update(task)
      case Left(error) => Future.successful(Left(error))
    }
    _ <- auditLog.record(s"Task $id updated")
  } yield updated
}
```

### Implicit Parameters and Type Classes

```scala
// Type class for JSON encoding
trait JsonEncoder[T] {
  def encode(value: T): Json
}

object JsonEncoder {
  implicit val taskEncoder: JsonEncoder[Task] = new JsonEncoder[Task] {
    def encode(task: Task): Json = Json.obj(
      "id" -> Json.fromString(task.id),
      "title" -> Json.fromString(task.title),
      "status" -> Json.fromString(TaskStatus.toString(task.status))
    )
  }
  
  implicit class JsonEncoderOps[T](val value: T) extends AnyVal {
    def toJson(implicit encoder: JsonEncoder[T]): Json = encoder.encode(value)
  }
}

// Usage
val task = Task("1", "Example")
val json = task.toJson  // Implicit resolution finds taskEncoder
```

### Higher-Order Functions

```scala
class TaskFilters {
  type TaskFilter = Task => Boolean
  
  def byStatus(status: TaskStatus): TaskFilter = 
    _.status == status
  
  def byPriority(priority: TaskPriority): TaskFilter = 
    _.priority == priority
  
  def byTags(tags: Set[String]): TaskFilter = 
    task => tags.subsetOf(task.tags.toSet)
  
  def combine(filters: TaskFilter*): TaskFilter = 
    task => filters.forall(_(task))
  
  // Usage
  val urgentInProgress = combine(
    byStatus(TaskStatus.InProgress),
    byPriority(TaskPriority.Urgent)
  )
  
  val filtered = tasks.filter(urgentInProgress)
}
```

### Futures and Async Operations

```scala
def parallelTaskOperations(taskIds: List[String]): Future[List[Task]] = {
  val futures = taskIds.map { id =>
    repository.getById(id).recover {
      case _: NoSuchElementException => 
        Task(id, "Not Found", status = TaskStatus.Cancelled)
    }
  }
  
  Future.sequence(futures)
}

// Using Future.traverse for transformation
def enrichTasks(tasks: List[Task]): Future[List[EnrichedTask]] = {
  Future.traverse(tasks) { task =>
    for {
      user <- userService.getUser(task.assignedTo)
      comments <- commentService.getComments(task.id)
    } yield EnrichedTask(task, user, comments)
  }
}
```

## Performance Considerations

### JVM Optimization

**JVM Tuning**
```bash
# Optimized JVM settings for Scala applications
java -server \
  -Xms2g -Xmx2g \
  -XX:+UseG1GC \
  -XX:MaxGCPauseMillis=200 \
  -XX:+UseStringDeduplication \
  -jar task-api-server.jar
```

### Collection Performance

```scala
// Use appropriate collection types
val taskMap = mutable.HashMap[String, Task]()  // Fast lookup
val taskList = ListBuffer[Task]()  // Fast append
val taskSet = mutable.HashSet[String]()  // Fast contains

// Prefer lazy evaluation for large datasets
def processLargeTasks(tasks: Iterator[Task]): Iterator[ProcessedTask] = {
  tasks
    .filter(_.status == TaskStatus.Pending)
    .map(processTask)
    .take(1000)  // Process only first 1000
}

// Use parallel collections for CPU-bound operations
val results = tasks.par.map { task =>
  expensiveComputation(task)
}.seq
```

### Database Connection Pooling

```scala
// application.conf
h2mem = {
  url = "jdbc:h2:mem:tasks"
  driver = org.h2.Driver
  connectionPool = "HikariCP"
  numThreads = 20
  maxConnections = 20
  minConnections = 5
  connectionTimeout = 5000
}

// Using connection pool in Slick
class DatabaseConfig {
  val db = Database.forConfig("h2mem")
  
  // Execute with proper resource management
  def withTransaction[T](action: DBIO[T]): Future[T] = {
    db.run(action.transactionally)
  }
}
```

### Akka HTTP Performance Tuning

```scala
// application.conf
akka.http {
  server {
    server-header = "Task API"
    idle-timeout = 60s
    request-timeout = 30s
    max-connections = 1024
    pipelining-limit = 16
  }
  
  client {
    connecting-timeout = 10s
    idle-timeout = 60s
  }
  
  host-connection-pool {
    max-connections = 100
    min-connections = 10
    max-open-requests = 256
    idle-timeout = 30s
  }
}
```

## Deployment Strategies

### Assembly Plugin for Fat JARs

```scala
// project/plugins.sbt
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.3")

// build.sbt
assembly / assemblyMergeStrategy := {
  case "reference.conf" => MergeStrategy.concat
  case "application.conf" => MergeStrategy.concat
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}

assembly / mainClass := Some("com.example.taskapi.Main")
assembly / assemblyJarName := "task-api.jar"
```

### Native Image with GraalVM

```bash
# Install GraalVM
sdk install java 22.3.0.r17-grl

# Build native image
sbt assembly
native-image -jar target/scala-2.13/task-api.jar \
  --no-fallback \
  --enable-http \
  --enable-https \
  -H:+ReportExceptionStackTraces
```

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: scala-task-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: task-api
  template:
    metadata:
      labels:
        app: task-api
    spec:
      containers:
      - name: task-api
        image: task-api:latest
        ports:
        - containerPort: 8080
        env:
        - name: JAVA_OPTS
          value: "-Xms1g -Xmx1g -XX:+UseG1GC"
        resources:
          requests:
            memory: "1.5Gi"
            cpu: "1"
          limits:
            memory: "2Gi"
            cpu: "2"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
```

## Conclusion

This chapter demonstrated Scala's exceptional capabilities for building both REST APIs and gRPC services. Key takeaways include:

### Language Strengths

**Functional Programming Excellence**
- Immutable data structures ensure thread safety
- Higher-order functions enable composable abstractions
- Pattern matching provides exhaustive case handling
- For-comprehensions simplify asynchronous operations

**Type System Power**
- Static typing catches errors at compile time
- Type inference reduces boilerplate
- Implicit parameters enable dependency injection
- Type classes provide ad-hoc polymorphism

**Concurrency and Scale**
- Actor model with Akka handles massive concurrency
- Futures and Promises for asynchronous programming
- Reactive Streams for backpressure handling
- Parallel collections for data parallelism

### Practical Applications

**Enterprise Systems**
- Microservices with excellent type safety
- Event-driven architectures with Akka
- Stream processing with Akka Streams
- Integration with existing Java infrastructure

**Data Engineering**
- Apache Spark for big data processing
- Kafka Streams for real-time data pipelines
- Distributed systems with Akka Cluster
- Machine learning with Spark MLlib

### Development Experience

**Pros**
- Expressive syntax reduces code verbosity
- Powerful abstractions for complex domains
- Excellent tooling with IntelliJ IDEA
- Strong ecosystem for web and data applications
- Seamless Java interoperability

**Cons**
- Steep learning curve for functional concepts
- Longer compilation times than Java
- Binary compatibility issues between versions
- Complex implicit resolution can be confusing

**Best Use Cases**
- High-throughput API services
- Distributed and concurrent systems
- Data processing pipelines
- Financial and trading systems
- Complex domain modeling

Scala's unique combination of functional and object-oriented programming, coupled with its powerful type system and excellent concurrency support, makes it an outstanding choice for building scalable, maintainable APIs. The ability to leverage both the vast Java ecosystem and Scala's advanced features provides developers with unparalleled flexibility in solving complex problems.

Whether building microservices with Akka HTTP, high-performance gRPC services with ScalaPB, or distributed systems with Akka Cluster, Scala provides the tools and abstractions necessary to build robust, scalable applications that can handle the demands of modern distributed systems.