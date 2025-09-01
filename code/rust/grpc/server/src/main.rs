use std::collections::HashMap;
use std::pin::Pin;
use std::sync::{Arc, RwLock};
use chrono::Utc;
use tokio::sync::mpsc;
use tokio_stream::{wrappers::ReceiverStream, Stream, StreamExt};
use tonic::{transport::Server, Request, Response, Status, Streaming};
use uuid::Uuid;

// Include the generated proto code
pub mod tasks {
    include!("tasks.rs");
}

use tasks::{
    task_service_server::{TaskService, TaskServiceServer},
    CreateTaskRequest, DeleteTaskRequest, GetTaskRequest, ListTasksRequest,
    Task, TaskEvent, UpdateTaskRequest, WatchTasksRequest, Empty,
};

#[derive(Debug, Default)]
pub struct TaskServiceImpl {
    tasks: Arc<RwLock<HashMap<String, Task>>>,
}

impl TaskServiceImpl {
    pub fn new() -> Self {
        let service = Self {
            tasks: Arc::new(RwLock::new(HashMap::new())),
        };
        
        // Initialize with sample data
        service.initialize_sample_data();
        service
    }
    
    fn initialize_sample_data(&self) {
        let mut tasks = self.tasks.write().unwrap();
        
        let task1 = Task {
            id: Uuid::new_v4().to_string(),
            title: "Complete project documentation".to_string(),
            description: "Write comprehensive documentation for the gRPC API".to_string(),
            status: tasks::TaskStatus::InProgress as i32,
            priority: tasks::TaskPriority::High as i32,
            tags: vec!["documentation".to_string(), "api".to_string()],
            assigned_to: "dev-team".to_string(),
            created_at: Some(prost_types::Timestamp {
                seconds: Utc::now().timestamp(),
                nanos: 0,
            }),
            updated_at: Some(prost_types::Timestamp {
                seconds: Utc::now().timestamp(),
                nanos: 0,
            }),
            ..Default::default()
        };
        tasks.insert(task1.id.clone(), task1);
        
        let task2 = Task {
            id: Uuid::new_v4().to_string(),
            title: "Review pull requests".to_string(),
            description: "Review and approve pending pull requests".to_string(),
            status: tasks::TaskStatus::Pending as i32,
            priority: tasks::TaskPriority::Medium as i32,
            tags: vec!["review".to_string(), "code".to_string()],
            assigned_to: "senior-dev".to_string(),
            created_at: Some(prost_types::Timestamp {
                seconds: Utc::now().timestamp(),
                nanos: 0,
            }),
            updated_at: Some(prost_types::Timestamp {
                seconds: Utc::now().timestamp(),
                nanos: 0,
            }),
            ..Default::default()
        };
        tasks.insert(task2.id.clone(), task2);
    }
}

#[tonic::async_trait]
impl TaskService for TaskServiceImpl {
    type ListTasksStream = Pin<Box<dyn Stream<Item = Result<Task, Status>> + Send>>;
    
    async fn list_tasks(
        &self,
        request: Request<ListTasksRequest>,
    ) -> Result<Response<Self::ListTasksStream>, Status> {
        let req = request.into_inner();
        let tasks = self.tasks.read().unwrap();
        
        let mut filtered_tasks: Vec<Task> = tasks
            .values()
            .filter(|task| {
                if req.status != 0 && task.status != req.status {
                    return false;
                }
                if !req.assigned_to.is_empty() && task.assigned_to != req.assigned_to {
                    return false;
                }
                if !req.tags.is_empty() {
                    if !req.tags.iter().all(|tag| task.tags.contains(tag)) {
                        return false;
                    }
                }
                true
            })
            .cloned()
            .collect();
        
        // Sort tasks
        match req.sort_order {
            1 => filtered_tasks.sort_by(|a, b| b.created_at.cmp(&a.created_at)),
            2 => filtered_tasks.sort_by(|a, b| b.updated_at.cmp(&a.updated_at)),
            3 => filtered_tasks.sort_by(|a, b| b.priority.cmp(&a.priority)),
            _ => {}
        }
        
        // Apply pagination
        let page_size = if req.page_size > 0 { req.page_size as usize } else { 20 };
        let page_size = page_size.min(100);
        
        let start_index = if !req.page_token.is_empty() {
            req.page_token.parse::<usize>().unwrap_or(0)
        } else {
            0
        };
        
        let end_index = (start_index + page_size).min(filtered_tasks.len());
        let paginated_tasks = filtered_tasks[start_index..end_index].to_vec();
        
        // Create stream
        let (tx, rx) = mpsc::channel(128);
        
        tokio::spawn(async move {
            for task in paginated_tasks {
                if tx.send(Ok(task)).await.is_err() {
                    break;
                }
            }
        });
        
        let output_stream = ReceiverStream::new(rx);
        Ok(Response::new(Box::pin(output_stream)))
    }
    
    async fn get_task(
        &self,
        request: Request<GetTaskRequest>,
    ) -> Result<Response<Task>, Status> {
        let task_id = request.into_inner().id;
        let tasks = self.tasks.read().unwrap();
        
        match tasks.get(&task_id) {
            Some(task) => Ok(Response::new(task.clone())),
            None => Err(Status::not_found(format!("Task with ID {} not found", task_id))),
        }
    }
    
    async fn create_task(
        &self,
        request: Request<CreateTaskRequest>,
    ) -> Result<Response<Task>, Status> {
        let req = request.into_inner();
        let mut task = req.task.ok_or_else(|| Status::invalid_argument("Task data is required"))?;
        
        // Validate
        if task.title.is_empty() {
            return Err(Status::invalid_argument("Title is required"));
        }
        if task.title.len() > 200 {
            return Err(Status::invalid_argument("Title must be 200 characters or less"));
        }
        
        // Generate ID and timestamps
        task.id = Uuid::new_v4().to_string();
        let now = prost_types::Timestamp {
            seconds: Utc::now().timestamp(),
            nanos: 0,
        };
        task.created_at = Some(now.clone());
        task.updated_at = Some(now);
        
        // Set defaults
        if task.created_by.is_empty() {
            task.created_by = "system".to_string();
        }
        
        let mut tasks = self.tasks.write().unwrap();
        tasks.insert(task.id.clone(), task.clone());
        
        Ok(Response::new(task))
    }
    
    async fn update_task(
        &self,
        request: Request<UpdateTaskRequest>,
    ) -> Result<Response<Task>, Status> {
        let req = request.into_inner();
        let update_task = req.task.ok_or_else(|| Status::invalid_argument("Task data is required"))?;
        
        if update_task.id.is_empty() {
            return Err(Status::invalid_argument("Task ID is required"));
        }
        
        let mut tasks = self.tasks.write().unwrap();
        
        match tasks.get_mut(&update_task.id) {
            Some(task) => {
                // Apply updates based on update_mask
                for field in &req.update_mask {
                    match field.as_str() {
                        "title" => task.title = update_task.title.clone(),
                        "description" => task.description = update_task.description.clone(),
                        "status" => task.status = update_task.status,
                        "priority" => task.priority = update_task.priority,
                        "tags" => task.tags = update_task.tags.clone(),
                        "assigned_to" => task.assigned_to = update_task.assigned_to.clone(),
                        "due_date" => task.due_date = update_task.due_date.clone(),
                        _ => {}
                    }
                }
                
                // Update timestamp
                task.updated_at = Some(prost_types::Timestamp {
                    seconds: Utc::now().timestamp(),
                    nanos: 0,
                });
                
                Ok(Response::new(task.clone()))
            }
            None => Err(Status::not_found(format!("Task with ID {} not found", update_task.id))),
        }
    }
    
    async fn delete_task(
        &self,
        request: Request<DeleteTaskRequest>,
    ) -> Result<Response<Empty>, Status> {
        let task_id = request.into_inner().id;
        let mut tasks = self.tasks.write().unwrap();
        
        match tasks.remove(&task_id) {
            Some(_) => Ok(Response::new(Empty {})),
            None => Err(Status::not_found(format!("Task with ID {} not found", task_id))),
        }
    }
    
    type WatchTasksStream = Pin<Box<dyn Stream<Item = Result<TaskEvent, Status>> + Send>>;
    
    async fn watch_tasks(
        &self,
        request: Request<Streaming<WatchTasksRequest>>,
    ) -> Result<Response<Self::WatchTasksStream>, Status> {
        let mut stream = request.into_inner();
        let tasks = self.tasks.clone();
        
        let (tx, rx) = mpsc::channel(128);
        
        tokio::spawn(async move {
            while let Some(result) = stream.next().await {
                match result {
                    Ok(req) => {
                        let tasks = tasks.read().unwrap();
                        
                        if req.watch_all {
                            for task in tasks.values() {
                                let event = TaskEvent {
                                    event_type: tasks::task_event::EventType::Updated as i32,
                                    task: Some(task.clone()),
                                    timestamp: Some(prost_types::Timestamp {
                                        seconds: Utc::now().timestamp(),
                                        nanos: 0,
                                    }),
                                };
                                if tx.send(Ok(event)).await.is_err() {
                                    break;
                                }
                            }
                        } else if !req.task_ids.is_empty() {
                            for task_id in &req.task_ids {
                                if let Some(task) = tasks.get(task_id) {
                                    let event = TaskEvent {
                                        event_type: tasks::task_event::EventType::Updated as i32,
                                        task: Some(task.clone()),
                                        timestamp: Some(prost_types::Timestamp {
                                            seconds: Utc::now().timestamp(),
                                            nanos: 0,
                                        }),
                                    };
                                    if tx.send(Ok(event)).await.is_err() {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    Err(e) => {
                        log::error!("Error in watch stream: {}", e);
                        break;
                    }
                }
            }
        });
        
        let output_stream = ReceiverStream::new(rx);
        Ok(Response::new(Box::pin(output_stream)))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();
    
    let addr = "[::]:50051".parse()?;
    let task_service = TaskServiceImpl::new();
    
    log::info!("Rust gRPC server starting on {}", addr);
    
    Server::builder()
        .add_service(TaskServiceServer::new(task_service))
        .serve(addr)
        .await?;
    
    Ok(())
}