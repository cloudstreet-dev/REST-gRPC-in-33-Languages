package com.example.taskapi.grpc.server

import com.typesafe.scalalogging.LazyLogging
import io.grpc.{Server, ServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionService

import scala.concurrent.ExecutionContext

object Main extends App with LazyLogging {
  
  println("""
    |╔═══════════════════════════════════════════╗
    |║     Scala Task Management gRPC Server      ║
    |║         Built with ScalaPB                ║
    |╚═══════════════════════════════════════════╝
    |""".stripMargin)
  
  implicit val ec: ExecutionContext = ExecutionContext.global
  
  // Create service implementation
  val taskService = new TaskServiceImpl()
  taskService.initialize()
  logger.info("Task service initialized with sample data")
  
  // Build and start server
  val port = 50051
  val server: Server = ServerBuilder
    .forPort(port)
    .addService(taskService)
    .addService(ProtoReflectionService.newInstance()) // Enable reflection for grpcurl
    .build()
  
  server.start()
  
  logger.info(s"gRPC Server started on port $port")
  
  println(s"""
    |Server listening on port $port
    |
    |Available gRPC methods:
    |  - ListTasks       : List tasks with filtering and pagination
    |  - GetTask         : Get a specific task by ID
    |  - CreateTask      : Create a new task
    |  - UpdateTask      : Update an existing task
    |  - DeleteTask      : Delete a task
    |  - UpdateTaskStatus: Update only the status of a task
    |  - WatchTasks      : Stream task updates (server streaming)
    |
    |Test with grpcurl:
    |  grpcurl -plaintext localhost:50051 list
    |  grpcurl -plaintext localhost:50051 tasks.TaskService/ListTasks
    |  
    |  grpcurl -plaintext -d '{
    |    "title": "Test Task",
    |    "description": "Created via gRPC",
    |    "priority": "HIGH",
    |    "tags": ["test", "grpc"],
    |    "assigned_to": "test-user"
    |  }' localhost:50051 tasks.TaskService/CreateTask
    |
    |Press Ctrl+C to stop the server...
    |""".stripMargin)
  
  // Add shutdown hook
  sys.addShutdownHook {
    logger.info("Shutting down gRPC server...")
    server.shutdown()
    server.awaitTermination()
    logger.info("Server stopped")
  }
  
  // Keep server running
  server.awaitTermination()
}