package com.example.taskapi

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import com.example.taskapi.db.{DatabaseConfig, TaskRepository}
import com.example.taskapi.models._
import com.example.taskapi.routes.TaskRoutes
import com.typesafe.scalalogging.LazyLogging

import java.time.Instant
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Failure, Success}

object Main extends App with LazyLogging {
  
  println("""
    |╔═══════════════════════════════════════════╗
    |║     Scala Task Management REST API        ║
    |║         Built with Akka HTTP              ║
    |╚═══════════════════════════════════════════╝
    |""".stripMargin)
  
  // Create actor system
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "task-api-system")
  implicit val ec: ExecutionContext = system.executionContext
  
  // CORS settings
  val corsResponseHeaders = List(
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Credentials`(true),
    `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With"),
    `Access-Control-Allow-Methods`(GET, POST, PUT, PATCH, DELETE, OPTIONS)
  )
  
  // CORS directive
  def corsHandler: Directive0 = {
    extractRequest.flatMap { request =>
      respondWithHeaders(corsResponseHeaders) &
      (if (request.method == OPTIONS) {
        complete(200, "OK")
      } else {
        pass
      })
    }
  }
  
  // Initialize database and repository
  logger.info("Initializing database...")
  val repository = DatabaseConfig.createRepository()
  
  // Initialize database schema
  val initFuture = repository.initialize().map { _ =>
    logger.info("Database initialized successfully")
    
    // Add sample tasks
    val sampleTasks = List(
      Task(
        id = "1",
        title = "Implement Scala REST API",
        description = Some("Build a complete REST API using Akka HTTP"),
        status = TaskStatus.InProgress,
        priority = TaskPriority.High,
        tags = List("scala", "rest", "api"),
        assignedTo = Some("dev-team"),
        createdAt = Instant.now().minusSeconds(3600),
        updatedAt = Instant.now().minusSeconds(1800)
      ),
      Task(
        id = "2",
        title = "Add gRPC support",
        description = Some("Implement gRPC server using ScalaPB"),
        status = TaskStatus.Pending,
        priority = TaskPriority.Medium,
        tags = List("scala", "grpc", "protobuf"),
        assignedTo = Some("backend-team"),
        createdAt = Instant.now().minusSeconds(7200),
        updatedAt = Instant.now().minusSeconds(7200)
      ),
      Task(
        id = "3",
        title = "Write unit tests",
        description = Some("Add comprehensive test coverage"),
        status = TaskStatus.Pending,
        priority = TaskPriority.High,
        tags = List("testing", "quality"),
        assignedTo = Some("qa-team"),
        createdAt = Instant.now().minusSeconds(1800),
        updatedAt = Instant.now().minusSeconds(900)
      )
    )
    
    // Insert sample tasks
    Future.sequence(sampleTasks.map(repository.create)).map { _ =>
      logger.info(s"Added ${sampleTasks.size} sample tasks")
    }
  }
  
  Await.result(initFuture, 10.seconds)
  
  // Create routes
  val taskRoutes = new TaskRoutes(repository)
  val routes: Route = corsHandler {
    taskRoutes.allRoutes
  }
  
  // Start HTTP server
  val interface = "0.0.0.0"
  val port = 8080
  
  val bindingFuture = Http().newServerAt(interface, port).bind(routes)
  
  bindingFuture.onComplete {
    case Success(binding) =>
      val address = binding.localAddress
      println(s"""
        |Server online at http://${address.getHostString}:${address.getPort}/
        |
        |Available endpoints:
        |  GET    /api/tasks          - List all tasks
        |  GET    /api/tasks/{id}     - Get a specific task
        |  POST   /api/tasks          - Create a new task
        |  PUT    /api/tasks/{id}     - Update a task
        |  PATCH  /api/tasks/{id}/status - Update task status
        |  DELETE /api/tasks/{id}     - Delete a task
        |  GET    /health             - Health check
        |
        |Sample requests:
        |  curl http://localhost:8080/api/tasks
        |  curl -X POST http://localhost:8080/api/tasks \\
        |    -H "Content-Type: application/json" \\
        |    -d '{"title":"New Task","priority":"high"}'
        |
        |Press RETURN to stop...
        |""".stripMargin)
      
    case Failure(exception) =>
      logger.error(s"Failed to bind to $interface:$port", exception)
      system.terminate()
  }
  
  // Wait for user input to stop
  StdIn.readLine()
  
  // Shutdown
  bindingFuture
    .flatMap(_.unbind())
    .onComplete { _ =>
      logger.info("Server stopped")
      system.terminate()
    }
}