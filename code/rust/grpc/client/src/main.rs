use anyhow::Result;
use tokio_stream::StreamExt;
use tonic::{transport::Channel, Request, Streaming};

// Include the generated proto code
pub mod tasks {
    include!("tasks.rs");
}

use tasks::{
    task_service_client::TaskServiceClient,
    CreateTaskRequest, DeleteTaskRequest, GetTaskRequest, ListTasksRequest,
    Task, TaskEvent, TaskPriority, TaskStatus, UpdateTaskRequest, WatchTasksRequest,
};

pub struct TaskGrpcClient {
    client: TaskServiceClient<Channel>,
}

impl TaskGrpcClient {
    pub async fn connect(addr: &str) -> Result<Self> {
        let client = TaskServiceClient::connect(addr.to_string()).await?;
        Ok(Self { client })
    }
    
    pub async fn list_tasks(
        &mut self,
        page_size: i32,
        page_token: String,
        status: Option<TaskStatus>,
        assigned_to: String,
        tags: Vec<String>,
        sort_order: i32,
    ) -> Result<Vec<Task>> {
        let request = Request::new(ListTasksRequest {
            page_size,
            page_token,
            status: status.map(|s| s as i32).unwrap_or(0),
            assigned_to,
            tags,
            sort_order,
        });
        
        let mut stream = self.client.list_tasks(request).await?.into_inner();
        let mut tasks = Vec::new();
        
        while let Some(task) = stream.next().await {
            tasks.push(task?);
        }
        
        Ok(tasks)
    }
    
    pub async fn get_task(&mut self, task_id: &str) -> Result<Task> {
        let request = Request::new(GetTaskRequest {
            id: task_id.to_string(),
        });
        
        let response = self.client.get_task(request).await?;
        Ok(response.into_inner())
    }
    
    pub async fn create_task(
        &mut self,
        title: &str,
        description: &str,
        priority: TaskPriority,
        tags: Vec<String>,
        assigned_to: &str,
    ) -> Result<Task> {
        let task = Task {
            title: title.to_string(),
            description: description.to_string(),
            priority: priority as i32,
            tags,
            assigned_to: assigned_to.to_string(),
            ..Default::default()
        };
        
        let request = Request::new(CreateTaskRequest {
            task: Some(task),
        });
        
        let response = self.client.create_task(request).await?;
        Ok(response.into_inner())
    }
    
    pub async fn update_task(
        &mut self,
        task_id: &str,
        title: Option<String>,
        description: Option<String>,
        status: Option<TaskStatus>,
        priority: Option<TaskPriority>,
        tags: Option<Vec<String>>,
        assigned_to: Option<String>,
    ) -> Result<Task> {
        let mut task = Task {
            id: task_id.to_string(),
            ..Default::default()
        };
        
        let mut update_mask = Vec::new();
        
        if let Some(title) = title {
            task.title = title;
            update_mask.push("title".to_string());
        }
        if let Some(description) = description {
            task.description = description;
            update_mask.push("description".to_string());
        }
        if let Some(status) = status {
            task.status = status as i32;
            update_mask.push("status".to_string());
        }
        if let Some(priority) = priority {
            task.priority = priority as i32;
            update_mask.push("priority".to_string());
        }
        if let Some(tags) = tags {
            task.tags = tags;
            update_mask.push("tags".to_string());
        }
        if let Some(assigned_to) = assigned_to {
            task.assigned_to = assigned_to;
            update_mask.push("assigned_to".to_string());
        }
        
        let request = Request::new(UpdateTaskRequest {
            task: Some(task),
            update_mask,
        });
        
        let response = self.client.update_task(request).await?;
        Ok(response.into_inner())
    }
    
    pub async fn delete_task(&mut self, task_id: &str) -> Result<()> {
        let request = Request::new(DeleteTaskRequest {
            id: task_id.to_string(),
        });
        
        self.client.delete_task(request).await?;
        Ok(())
    }
    
    pub async fn watch_tasks(
        &mut self,
        watch_all: bool,
        task_ids: Vec<String>,
        assigned_to: String,
    ) -> Result<Streaming<TaskEvent>> {
        let request_stream = tokio_stream::once(WatchTasksRequest {
            watch_all,
            task_ids,
            assigned_to,
        });
        
        let response = self.client.watch_tasks(request_stream).await?;
        Ok(response.into_inner())
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    
    log::info!("Note: Run 'cargo build' first to generate proto files.");
    
    let mut client = TaskGrpcClient::connect("http://[::1]:50051").await?;
    
    // Create a task
    log::info!("Creating a new task...");
    let task = client.create_task(
        "Test Rust gRPC Client",
        "Testing the Rust gRPC client implementation",
        TaskPriority::High,
        vec!["test".to_string(), "rust".to_string(), "grpc".to_string()],
        "dev-team",
    ).await?;
    
    println!("Created task: {} - {}", task.id, task.title);
    
    // Get the task
    log::info!("Retrieving task {}...", task.id);
    let retrieved = client.get_task(&task.id).await?;
    println!("Retrieved task: {} - Status: {}", retrieved.title, retrieved.status);
    
    // Update task status
    log::info!("Updating task status to IN_PROGRESS...");
    let updated = client.update_task(
        &task.id,
        None,
        None,
        Some(TaskStatus::InProgress),
        None,
        None,
        None,
    ).await?;
    println!("Updated task status: {}", updated.status);
    
    // List all tasks
    log::info!("Listing all tasks...");
    let tasks = client.list_tasks(
        10,
        String::new(),
        None,
        String::new(),
        Vec::new(),
        1, // CREATED_DESC
    ).await?;
    
    for task in &tasks {
        println!("[{}] {} - {}", task.status, task.title, task.id);
    }
    
    // Watch for task changes
    log::info!("Watching for task changes...");
    let mut stream = client.watch_tasks(true, Vec::new(), String::new()).await?;
    
    // Get a few events
    let mut count = 0;
    while let Some(event) = stream.next().await {
        if let Ok(event) = event {
            if let Some(task) = event.task {
                println!("Event: {} - Task: {}", event.event_type, task.title);
            }
        }
        count += 1;
        if count >= 3 {
            break;
        }
    }
    
    // Delete the task
    log::info!("Deleting task {}...", task.id);
    client.delete_task(&task.id).await?;
    println!("Task deleted successfully");
    
    Ok(())
}