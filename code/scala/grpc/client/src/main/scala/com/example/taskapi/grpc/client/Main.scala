package com.example.taskapi.grpc.client

import io.grpc.StatusRuntimeException
import scopt.OParser
import tasks.tasks._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

// Command line configuration
case class Config(
  command: String = "",
  host: String = "localhost",
  port: Int = 50051,
  taskId: String = "",
  title: String = "",
  description: String = "",
  status: String = "",
  priority: String = "MEDIUM",
  tags: List[String] = List.empty,
  assignedTo: String = ""
)

object Main extends App {
  
  println("""
    |╔═══════════════════════════════════════════╗
    |║     Scala Task Management gRPC Client      ║
    |╚═══════════════════════════════════════════╝
    |""".stripMargin)
  
  implicit val ec: ExecutionContext = ExecutionContext.global
  
  // Parse command line arguments
  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("task-grpc-client"),
      head("task-grpc-client", "1.0"),
      
      cmd("demo")
        .action((_, c) => c.copy(command = "demo"))
        .text("Run a comprehensive demo"),
      
      cmd("list")
        .action((_, c) => c.copy(command = "list"))
        .text("List all tasks")
        .children(
          opt[String]('s', "status")
            .action((x, c) => c.copy(status = x))
            .text("Filter by status (PENDING, IN_PROGRESS, COMPLETED, CANCELLED)"),
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
            .text("Task priority (LOW, MEDIUM, HIGH, URGENT)"),
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
            .text("New status (PENDING, IN_PROGRESS, COMPLETED, CANCELLED)")
        ),
      
      cmd("delete")
        .action((_, c) => c.copy(command = "delete"))
        .text("Delete a task")
        .children(
          arg[String]("<id>")
            .action((x, c) => c.copy(taskId = x))
            .text("Task ID")
        ),
      
      cmd("watch")
        .action((_, c) => c.copy(command = "watch"))
        .text("Watch for task updates (streaming)")
        .children(
          opt[Seq[String]]("ids")
            .action((x, c) => c.copy(tags = x.toList))
            .text("Task IDs to watch (comma separated, or empty for all)")
        ),
      
      opt[String]('h', "host")
        .action((x, c) => c.copy(host = x))
        .text("gRPC server host (default: localhost)"),
      
      opt[Int]('p', "port")
        .action((x, c) => c.copy(port = x))
        .text("gRPC server port (default: 50051)"),
      
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
    val client = new TaskGrpcClient(config.host, config.port)
    
    val result: Future[Unit] = config.command match {
      case "demo" =>
        client.runDemo()
      
      case "list" =>
        val status = parseStatus(config.status)
        val assignedTo = if (config.assignedTo.nonEmpty) Some(config.assignedTo) else None
        
        client.listTasks(status, assignedTo, config.tags).map { response =>
          println(s"\nFound ${response.totalCount} tasks:")
          response.tasks.foreach { task =>
            println(s"  ${task.id}: ${task.title}")
            println(s"    Status: ${formatStatus(task.status)}")
            println(s"    Priority: ${formatPriority(task.priority)}")
            if (task.tags.nonEmpty) {
              println(s"    Tags: ${task.tags.mkString(", ")}")
            }
            if (task.assignedTo.nonEmpty) {
              println(s"    Assigned to: ${task.assignedTo}")
            }
            println()
          }
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error: ${e.getStatus}")
        }
      
      case "get" =>
        client.getTask(config.taskId).map { task =>
          println(s"\nTask Details:")
          println(s"  ID: ${task.id}")
          println(s"  Title: ${task.title}")
          if (task.description.nonEmpty) {
            println(s"  Description: ${task.description}")
          }
          println(s"  Status: ${formatStatus(task.status)}")
          println(s"  Priority: ${formatPriority(task.priority)}")
          if (task.tags.nonEmpty) {
            println(s"  Tags: ${task.tags.mkString(", ")}")
          }
          if (task.assignedTo.nonEmpty) {
            println(s"  Assigned to: ${task.assignedTo}")
          }
          task.createdAt.foreach { ts =>
            println(s"  Created: ${new java.util.Date(ts.seconds * 1000)}")
          }
          task.updatedAt.foreach { ts =>
            println(s"  Updated: ${new java.util.Date(ts.seconds * 1000)}")
          }
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error: ${e.getStatus}")
        }
      
      case "create" =>
        val priority = parsePriority(config.priority).getOrElse(TaskPriority.MEDIUM)
        
        client.createTask(
          title = config.title,
          description = config.description,
          priority = priority,
          tags = config.tags,
          assignedTo = config.assignedTo
        ).map { task =>
          println(s"\nTask created successfully!")
          println(s"  ID: ${task.id}")
          println(s"  Title: ${task.title}")
          println(s"  Status: ${formatStatus(task.status)}")
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error: ${e.getStatus}")
        }
      
      case "update" =>
        client.updateTask(
          id = config.taskId,
          title = if (config.title.nonEmpty) Some(config.title) else None,
          description = if (config.description.nonEmpty) Some(config.description) else None,
          status = parseStatus(config.status),
          priority = parsePriority(config.priority),
          tags = if (config.tags.nonEmpty) Some(config.tags) else None,
          assignedTo = if (config.assignedTo.nonEmpty) Some(config.assignedTo) else None
        ).map { task =>
          println(s"\nTask updated successfully!")
          println(s"  ID: ${task.id}")
          println(s"  Title: ${task.title}")
          println(s"  Status: ${formatStatus(task.status)}")
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error: ${e.getStatus}")
        }
      
      case "status" =>
        parseStatus(config.status) match {
          case Some(status) =>
            client.updateTaskStatus(config.taskId, status).map { task =>
              println(s"\nStatus updated successfully!")
              println(s"  ID: ${task.id}")
              println(s"  Title: ${task.title}")
              println(s"  New Status: ${formatStatus(task.status)}")
            }.recover {
              case e: StatusRuntimeException =>
                println(s"Error: ${e.getStatus}")
            }
          case None =>
            println(s"Invalid status: ${config.status}")
            println("Valid values: PENDING, IN_PROGRESS, COMPLETED, CANCELLED")
            Future.successful(())
        }
      
      case "delete" =>
        client.deleteTask(config.taskId).map { response =>
          if (response.success) {
            println(s"\nTask ${config.taskId} deleted successfully!")
          } else {
            println(s"\nFailed to delete task ${config.taskId}")
          }
        }.recover {
          case e: StatusRuntimeException =>
            println(s"Error: ${e.getStatus}")
        }
      
      case "watch" =>
        Future {
          client.watchTasks(config.tags)
        }
      
      case _ =>
        println("Invalid command. Use --help for usage information.")
        Future.successful(())
    }
    
    // Wait for result
    try {
      Await.result(result, 30.seconds)
    } catch {
      case ex: Exception =>
        println(s"Error executing command: ${ex.getMessage}")
    } finally {
      client.shutdown()
    }
  }
  
  private def parseStatus(str: String): Option[TaskStatus] = str.toUpperCase match {
    case "PENDING" => Some(TaskStatus.PENDING)
    case "IN_PROGRESS" => Some(TaskStatus.IN_PROGRESS)
    case "COMPLETED" => Some(TaskStatus.COMPLETED)
    case "CANCELLED" => Some(TaskStatus.CANCELLED)
    case "" => None
    case _ => None
  }
  
  private def parsePriority(str: String): Option[TaskPriority] = str.toUpperCase match {
    case "LOW" => Some(TaskPriority.LOW)
    case "MEDIUM" => Some(TaskPriority.MEDIUM)
    case "HIGH" => Some(TaskPriority.HIGH)
    case "URGENT" => Some(TaskPriority.URGENT)
    case "" => None
    case _ => None
  }
  
  private def formatStatus(status: TaskStatus): String = status match {
    case TaskStatus.PENDING => "PENDING"
    case TaskStatus.IN_PROGRESS => "IN_PROGRESS"
    case TaskStatus.COMPLETED => "COMPLETED"
    case TaskStatus.CANCELLED => "CANCELLED"
    case _ => "UNKNOWN"
  }
  
  private def formatPriority(priority: TaskPriority): String = priority match {
    case TaskPriority.LOW => "LOW"
    case TaskPriority.MEDIUM => "MEDIUM"
    case TaskPriority.HIGH => "HIGH"
    case TaskPriority.URGENT => "URGENT"
    case _ => "UNKNOWN"
  }
}