use anyhow::Result;
use chrono::{DateTime, Utc};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum TaskStatus {
    Pending,
    InProgress,
    Completed,
    Cancelled,
    Archived,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum TaskPriority {
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Task {
    pub id: String,
    pub title: String,
    pub description: Option<String>,
    pub status: TaskStatus,
    pub priority: TaskPriority,
    pub tags: Vec<String>,
    pub assigned_to: Option<String>,
    pub due_date: Option<DateTime<Utc>>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub created_by: Option<String>,
    pub updated_by: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateTaskRequest {
    pub title: String,
    pub description: Option<String>,
    pub priority: Option<TaskPriority>,
    pub tags: Vec<String>,
    pub assigned_to: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdateTaskRequest {
    pub title: Option<String>,
    pub description: Option<String>,
    pub status: Option<TaskStatus>,
    pub priority: Option<TaskPriority>,
    pub tags: Option<Vec<String>>,
    pub assigned_to: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct UpdateStatusRequest {
    pub status: TaskStatus,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TasksResponse {
    pub items: Vec<Task>,
    pub page_size: usize,
    pub next_page_token: Option<String>,
    pub total_count: usize,
}

pub struct TaskApiClient {
    client: Client,
    base_url: String,
}

impl TaskApiClient {
    pub fn new(base_url: &str) -> Self {
        Self {
            client: Client::new(),
            base_url: base_url.trim_end_matches('/').to_string(),
        }
    }
    
    pub async fn list_tasks(
        &self,
        page_size: Option<usize>,
        page_token: Option<&str>,
        status: Option<&TaskStatus>,
        assigned_to: Option<&str>,
        tags: Option<Vec<&str>>,
        sort_order: Option<&str>,
    ) -> Result<TasksResponse> {
        let mut params = HashMap::new();
        
        if let Some(size) = page_size {
            params.insert("pageSize", size.to_string());
        }
        if let Some(token) = page_token {
            params.insert("pageToken", token.to_string());
        }
        if let Some(status) = status {
            params.insert("status", format!("{:?}", status));
        }
        if let Some(assigned) = assigned_to {
            params.insert("assignedTo", assigned.to_string());
        }
        if let Some(tags) = tags {
            params.insert("tags", tags.join(","));
        }
        if let Some(order) = sort_order {
            params.insert("sortOrder", order.to_string());
        }
        
        let response = self.client
            .get(format!("{}/api/v1/tasks", self.base_url))
            .query(&params)
            .send()
            .await?;
        
        response.json::<TasksResponse>().await.map_err(Into::into)
    }
    
    pub async fn get_task(&self, task_id: &str) -> Result<Option<Task>> {
        let response = self.client
            .get(format!("{}/api/v1/tasks/{}", self.base_url, task_id))
            .send()
            .await?;
        
        if response.status() == 404 {
            return Ok(None);
        }
        
        response.json::<Task>().await.map(Some).map_err(Into::into)
    }
    
    pub async fn create_task(
        &self,
        title: &str,
        description: Option<&str>,
        priority: Option<TaskPriority>,
        tags: Vec<String>,
        assigned_to: Option<&str>,
    ) -> Result<Task> {
        let request = CreateTaskRequest {
            title: title.to_string(),
            description: description.map(|s| s.to_string()),
            priority,
            tags,
            assigned_to: assigned_to.map(|s| s.to_string()),
        };
        
        let response = self.client
            .post(format!("{}/api/v1/tasks", self.base_url))
            .json(&request)
            .send()
            .await?;
        
        response.json::<Task>().await.map_err(Into::into)
    }
    
    pub async fn update_task(
        &self,
        task_id: &str,
        updates: UpdateTaskRequest,
    ) -> Result<Option<Task>> {
        let response = self.client
            .put(format!("{}/api/v1/tasks/{}", self.base_url, task_id))
            .json(&updates)
            .send()
            .await?;
        
        if response.status() == 404 {
            return Ok(None);
        }
        
        response.json::<Task>().await.map(Some).map_err(Into::into)
    }
    
    pub async fn update_task_status(
        &self,
        task_id: &str,
        status: TaskStatus,
    ) -> Result<Option<Task>> {
        let request = UpdateStatusRequest { status };
        
        let response = self.client
            .patch(format!("{}/api/v1/tasks/{}/status", self.base_url, task_id))
            .json(&request)
            .send()
            .await?;
        
        if response.status() == 404 {
            return Ok(None);
        }
        
        response.json::<Task>().await.map(Some).map_err(Into::into)
    }
    
    pub async fn delete_task(&self, task_id: &str) -> Result<bool> {
        let response = self.client
            .delete(format!("{}/api/v1/tasks/{}", self.base_url, task_id))
            .send()
            .await?;
        
        Ok(response.status() == 204)
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();
    
    let client = TaskApiClient::new("http://localhost:8080");
    
    // Create a task
    log::info!("Creating a new task...");
    let task = client.create_task(
        "Test Rust REST Client",
        Some("Testing the Rust REST client implementation"),
        Some(TaskPriority::High),
        vec!["test".to_string(), "rust".to_string(), "rest".to_string()],
        Some("dev-team"),
    ).await?;
    
    println!("Created task: {} - {}", task.id, task.title);
    
    // Get the task
    log::info!("Retrieving task {}...", task.id);
    if let Some(retrieved) = client.get_task(&task.id).await? {
        println!("Retrieved task: {} - Status: {:?}", retrieved.title, retrieved.status);
    }
    
    // Update task status
    log::info!("Updating task status to IN_PROGRESS...");
    if let Some(updated) = client.update_task_status(&task.id, TaskStatus::InProgress).await? {
        println!("Updated task status: {:?}", updated.status);
    }
    
    // List all tasks
    log::info!("Listing all tasks...");
    let tasks = client.list_tasks(Some(10), None, None, None, None, Some("created_desc")).await?;
    for task in &tasks.items {
        println!("[{:?}] {} - {}", task.status, task.title, task.id);
    }
    println!("Total tasks: {}", tasks.total_count);
    
    // Delete the task
    log::info!("Deleting task {}...", task.id);
    let deleted = client.delete_task(&task.id).await?;
    println!("Task deleted: {}", deleted);
    
    Ok(())
}