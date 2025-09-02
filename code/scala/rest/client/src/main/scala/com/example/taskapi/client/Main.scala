package com.example.taskapi.client

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.example.taskapi.models._
import scopt.OParser

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success}

// Command line configuration
case class Config(
  command: String = "",
  baseUrl: String = "http://localhost:8080",
  taskId: String = "",
  title: String = "",
  description: String = "",
  status: String = "",
  priority: String = "medium",
  tags: List[String] = List.empty,
  assignedTo: String = ""
)

object Main extends App {
  
  println("""
    |╔═══════════════════════════════════════════╗
    |║     Scala Task Management REST Client      ║
    |╚═══════════════════════════════════════════╝
    |""".stripMargin)
  
  // Parse command line arguments
  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("task-client"),
      head("task-client", "1.0"),
      
      cmd("demo")
        .action((_, c) => c.copy(command = "demo"))
        .text("Run a comprehensive demo"),
      
      cmd("list")
        .action((_, c) => c.copy(command = "list"))
        .text("List all tasks")
        .children(
          opt[String]('s', "status")
            .action((x, c) => c.copy(status = x))
            .text("Filter by status"),
          opt[String]('a', "assigned")
            .action((x, c) => c.copy(assignedTo = x))
            .text("Filter by assigned to"),
          opt[Seq[String]]('t', "tags")
            .action((x, c) => c.copy(tags = x.toList))
            .text("Filter by tags (comma separated)")
        ),
      
      cmd("get")
        .action((_, c) => c.copy(command = "get"))
        .text("Get a specific task")
        .children(
          arg[String]("<id>")
            .action((x, c) => c.copy(taskId = x))
            .text("Task ID")
        ),
      
      cmd("create")
        .action((_, c) => c.copy(command = "create"))
        .text("Create a new task")
        .children(
          arg[String]("<title>")
            .action((x, c) => c.copy(title = x))
            .text("Task title"),
          opt[String]('d', "description")
            .action((x, c) => c.copy(description = x))
            .text("Task description"),
          opt[String]('p', "priority")
            .action((x, c) => c.copy(priority = x))
            .text("Task priority (low, medium, high, urgent)"),
          opt[Seq[String]]('t', "tags")
            .action((x, c) => c.copy(tags = x.toList))
            .text("Tags (comma separated)"),
          opt[String]('a', "assigned")
            .action((x, c) => c.copy(assignedTo = x))
            .text("Assigned to")
        ),
      
      cmd("update")
        .action((_, c) => c.copy(command = "update"))
        .text("Update a task")
        .children(
          arg[String]("<id>")
            .action((x, c) => c.copy(taskId = x))
            .text("Task ID"),
          opt[String]("title")
            .action((x, c) => c.copy(title = x))
            .text("New title"),
          opt[String]('d', "description")
            .action((x, c) => c.copy(description = x))
            .text("New description"),
          opt[String]('s', "status")
            .action((x, c) => c.copy(status = x))
            .text("New status"),
          opt[String]('p', "priority")
            .action((x, c) => c.copy(priority = x))
            .text("New priority")
        ),
      
      cmd("status")
        .action((_, c) => c.copy(command = "status"))
        .text("Update task status")
        .children(
          arg[String]("<id>")
            .action((x, c) => c.copy(taskId = x))
            .text("Task ID"),
          arg[String]("<status>")
            .action((x, c) => c.copy(status = x))
            .text("New status (pending, in_progress, completed, cancelled)")
        ),
      
      cmd("delete")
        .action((_, c) => c.copy(command = "delete"))
        .text("Delete a task")
        .children(
          arg[String]("<id>")
            .action((x, c) => c.copy(taskId = x))
            .text("Task ID")
        ),
      
      opt[String]('u', "url")
        .action((x, c) => c.copy(baseUrl = x))
        .text("Base URL (default: http://localhost:8080)"),
      
      help("help").text("Print this help text")
    )
  }
  
  // Parse arguments
  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      runCommand(config)
    case _ =>
      // Arguments are invalid, help message will be displayed
      System.exit(1)
  }
  
  def runCommand(config: Config): Unit = {
    // Create actor system
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "task-client-system")
    implicit val ec: ExecutionContext = system.executionContext
    
    val client = new TaskApiClient(config.baseUrl)
    
    val result = config.command match {
      case "demo" =>
        client.runDemo()
      
      case "list" =>
        val statusOpt = if (config.status.nonEmpty) Some(config.status) else None
        val assignedOpt = if (config.assignedTo.nonEmpty) Some(config.assignedTo) else None
        val tagsOpt = if (config.tags.nonEmpty) Some(config.tags) else None
        
        client.listTasks(statusOpt, assignedOpt, tagsOpt).map {
          case Right(response) =>
            println(s"\nFound ${response.totalCount} tasks:")
            response.tasks.foreach { task =>
              println(s"  ${task.id}: ${task.title}")
              println(s"    Status: ${TaskStatus.toString(task.status)}")
              println(s"    Priority: ${TaskPriority.toString(task.priority)}")
              if (task.tags.nonEmpty) {
                println(s"    Tags: ${task.tags.mkString(", ")}")
              }
              task.assignedTo.foreach { assigned =>
                println(s"    Assigned to: $assigned")
              }
              println()
            }
          case Left(error) =>
            println(s"Error: $error")
        }
      
      case "get" =>
        client.getTask(config.taskId).map {
          case Right(task) =>
            println(s"\nTask Details:")
            println(s"  ID: ${task.id}")
            println(s"  Title: ${task.title}")
            task.description.foreach { desc =>
              println(s"  Description: $desc")
            }
            println(s"  Status: ${TaskStatus.toString(task.status)}")
            println(s"  Priority: ${TaskPriority.toString(task.priority)}")
            if (task.tags.nonEmpty) {
              println(s"  Tags: ${task.tags.mkString(", ")}")
            }
            task.assignedTo.foreach { assigned =>
              println(s"  Assigned to: $assigned")
            }
            println(s"  Created: ${task.createdAt}")
            println(s"  Updated: ${task.updatedAt}")
          case Left(error) =>
            println(s"Error: $error")
        }
      
      case "create" =>
        val request = CreateTaskRequest(
          title = config.title,
          description = if (config.description.nonEmpty) Some(config.description) else None,
          priority = TaskPriority.fromString(config.priority),
          tags = if (config.tags.nonEmpty) Some(config.tags) else None,
          assignedTo = if (config.assignedTo.nonEmpty) Some(config.assignedTo) else None
        )
        
        client.createTask(request).map {
          case Right(task) =>
            println(s"\nTask created successfully!")
            println(s"  ID: ${task.id}")
            println(s"  Title: ${task.title}")
            println(s"  Status: ${TaskStatus.toString(task.status)}")
          case Left(error) =>
            println(s"Error: $error")
        }
      
      case "update" =>
        val request = UpdateTaskRequest(
          title = if (config.title.nonEmpty) Some(config.title) else None,
          description = if (config.description.nonEmpty) Some(config.description) else None,
          status = if (config.status.nonEmpty) TaskStatus.fromString(config.status) else None,
          priority = if (config.priority.nonEmpty && config.priority != "medium") 
            TaskPriority.fromString(config.priority) else None,
          tags = if (config.tags.nonEmpty) Some(config.tags) else None,
          assignedTo = if (config.assignedTo.nonEmpty) Some(config.assignedTo) else None
        )
        
        client.updateTask(config.taskId, request).map {
          case Right(task) =>
            println(s"\nTask updated successfully!")
            println(s"  ID: ${task.id}")
            println(s"  Title: ${task.title}")
            println(s"  Status: ${TaskStatus.toString(task.status)}")
          case Left(error) =>
            println(s"Error: $error")
        }
      
      case "status" =>
        client.updateTaskStatus(config.taskId, config.status).map {
          case Right(task) =>
            println(s"\nStatus updated successfully!")
            println(s"  ID: ${task.id}")
            println(s"  Title: ${task.title}")
            println(s"  New Status: ${TaskStatus.toString(task.status)}")
          case Left(error) =>
            println(s"Error: $error")
        }
      
      case "delete" =>
        client.deleteTask(config.taskId).map {
          case Right(_) =>
            println(s"\nTask ${config.taskId} deleted successfully!")
          case Left(error) =>
            println(s"Error: $error")
        }
      
      case _ =>
        println("Invalid command. Use --help for usage information.")
        system.terminate()
    }
    
    // Wait for result and shutdown
    try {
      Await.result(result, 30.seconds)
    } catch {
      case ex: Exception =>
        println(s"Error executing command: ${ex.getMessage}")
    } finally {
      system.terminate()
      Await.result(system.whenTerminated, 10.seconds)
    }
  }
}