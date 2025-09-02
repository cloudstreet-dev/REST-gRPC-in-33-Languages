package com.example.taskapi.db

import com.example.taskapi.models._
import slick.jdbc.H2Profile.api._
import slick.lifted.{ProvenShape, Tag}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

// Slick table definition
class TasksTable(tag: Tag) extends Table[Task](tag, "tasks") {
  def id: Rep[String] = column[String]("id", O.PrimaryKey)
  def title: Rep[String] = column[String]("title")
  def description: Rep[Option[String]] = column[Option[String]]("description")
  def status: Rep[String] = column[String]("status")
  def priority: Rep[String] = column[String]("priority")
  def tags: Rep[String] = column[String]("tags") // JSON stored as string
  def assignedTo: Rep[Option[String]] = column[Option[String]]("assigned_to")
  def createdAt: Rep[Instant] = column[Instant]("created_at")
  def updatedAt: Rep[Instant] = column[Instant]("updated_at")
  
  // Custom mappings for enums and lists
  implicit val statusMapper = MappedColumnType.base[TaskStatus, String](
    TaskStatus.toString,
    s => TaskStatus.fromString(s).getOrElse(TaskStatus.Pending)
  )
  
  implicit val priorityMapper = MappedColumnType.base[TaskPriority, String](
    TaskPriority.toString,
    s => TaskPriority.fromString(s).getOrElse(TaskPriority.Medium)
  )
  
  implicit val tagsMapper = MappedColumnType.base[List[String], String](
    tags => tags.mkString(","),
    str => if (str.isEmpty) List.empty else str.split(",").toList
  )
  
  def statusTyped: Rep[TaskStatus] = status.asColumnOf[TaskStatus]
  def priorityTyped: Rep[TaskPriority] = priority.asColumnOf[TaskPriority]
  def tagsTyped: Rep[List[String]] = tags.asColumnOf[List[String]]
  
  def * : ProvenShape[Task] = (
    id, title, description, statusTyped, priorityTyped, tagsTyped, assignedTo, createdAt, updatedAt
  ).mapTo[Task]
}

// Repository for database operations
class TaskRepository(db: Database)(implicit ec: ExecutionContext) {
  
  val tasks = TableQuery[TasksTable]
  
  // Initialize database schema
  def initialize(): Future[Unit] = {
    val setup = DBIO.seq(
      tasks.schema.createIfNotExists
    )
    db.run(setup)
  }
  
  // Create a new task
  def create(task: Task): Future[Task] = {
    db.run(tasks += task).map(_ => task)
  }
  
  // Get task by ID
  def getById(id: String): Future[Option[Task]] = {
    db.run(tasks.filter(_.id === id).result.headOption)
  }
  
  // List tasks with filters
  def list(
    status: Option[TaskStatus] = None,
    assignedTo: Option[String] = None,
    tags: Option[List[String]] = None,
    limit: Int = 20,
    offset: Int = 0,
    sortBy: String = "created_at",
    sortOrder: String = "desc"
  ): Future[List[Task]] = {
    
    var query = tasks.filterOpt(status) { (t, s) =>
      t.statusTyped === s
    }.filterOpt(assignedTo) { (t, a) =>
      t.assignedTo === a
    }
    
    // Filter by tags if provided
    tags.foreach { tagList =>
      tagList.foreach { tag =>
        query = query.filter(_.tags like s"%$tag%")
      }
    }
    
    // Apply sorting
    val sortedQuery = (sortBy, sortOrder) match {
      case ("title", "asc") => query.sortBy(_.title.asc)
      case ("title", "desc") => query.sortBy(_.title.desc)
      case ("updated_at", "asc") => query.sortBy(_.updatedAt.asc)
      case ("updated_at", "desc") => query.sortBy(_.updatedAt.desc)
      case ("created_at", "desc") => query.sortBy(_.createdAt.desc)
      case _ => query.sortBy(_.createdAt.asc) // default
    }
    
    db.run(sortedQuery.drop(offset).take(limit).result).map(_.toList)
  }
  
  // Count tasks with filters
  def count(
    status: Option[TaskStatus] = None,
    assignedTo: Option[String] = None,
    tags: Option[List[String]] = None
  ): Future[Int] = {
    var query = tasks.filterOpt(status) { (t, s) =>
      t.statusTyped === s
    }.filterOpt(assignedTo) { (t, a) =>
      t.assignedTo === a
    }
    
    tags.foreach { tagList =>
      tagList.foreach { tag =>
        query = query.filter(_.tags like s"%$tag%")
      }
    }
    
    db.run(query.length.result)
  }
  
  // Update a task
  def update(id: String, request: UpdateTaskRequest): Future[Option[Task]] = {
    val updateAction = for {
      existing <- tasks.filter(_.id === id).result.headOption
      result <- existing match {
        case Some(task) =>
          val updated = task.copy(
            title = request.title.getOrElse(task.title),
            description = request.description.orElse(task.description),
            status = request.status.getOrElse(task.status),
            priority = request.priority.getOrElse(task.priority),
            tags = request.tags.getOrElse(task.tags),
            assignedTo = request.assignedTo.orElse(task.assignedTo),
            updatedAt = Instant.now()
          )
          tasks.filter(_.id === id).update(updated).map(_ => Some(updated))
        case None =>
          DBIO.successful(None)
      }
    } yield result
    
    db.run(updateAction.transactionally)
  }
  
  // Update task status
  def updateStatus(id: String, status: TaskStatus): Future[Option[Task]] = {
    update(id, UpdateTaskRequest(status = Some(status)))
  }
  
  // Delete a task
  def delete(id: String): Future[Boolean] = {
    db.run(tasks.filter(_.id === id).delete).map(_ > 0)
  }
  
  // Delete all tasks (for testing)
  def deleteAll(): Future[Unit] = {
    db.run(tasks.delete).map(_ => ())
  }
}

// Database configuration
object DatabaseConfig {
  def createDatabase(): Database = {
    Database.forConfig("h2mem")
  }
  
  def createRepository()(implicit ec: ExecutionContext): TaskRepository = {
    val db = createDatabase()
    new TaskRepository(db)
  }
}