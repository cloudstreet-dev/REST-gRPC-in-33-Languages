# Chapter 10: Rust - Memory Safety and Performance with Actix-web and Tonic

## Introduction

Rust, first appearing in 2010 and reaching stability in 2015, has revolutionized systems programming with its unique approach to memory safety without garbage collection. Its ownership system, zero-cost abstractions, and fearless concurrency have made it the most loved programming language in Stack Overflow surveys for multiple consecutive years. With frameworks like Actix-web for REST and Tonic for gRPC, Rust delivers exceptional performance while preventing entire classes of bugs at compile time.

In this chapter, we'll implement our Task Management API using Actix-web for REST and Tonic for gRPC, showcasing Rust's strengths in building safe, concurrent, and blazingly fast APIs.

## Why Rust for APIs?

Rust offers compelling advantages for API development:

1. **Memory Safety**: Prevents null pointer dereferences, buffer overflows, and data races at compile time
2. **Performance**: Zero-cost abstractions deliver C-like performance without manual memory management
3. **Concurrency**: Fearless concurrency through ownership and borrowing rules
4. **Type Safety**: Expressive type system catches errors at compile time
5. **No Garbage Collector**: Predictable performance without GC pauses
6. **Modern Tooling**: Cargo package manager and excellent compiler diagnostics
7. **Growing Ecosystem**: Rapidly expanding collection of high-quality libraries

## Setting Up the Development Environment

### Installing Rust

Using rustup, the official Rust toolchain installer:

```bash
# Install rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Add Rust to PATH
source $HOME/.cargo/env

# Verify installation
rustc --version
cargo --version

# Install additional tools
rustup component add rustfmt clippy
```

### Installing Protocol Buffer Compiler

For gRPC support:

```bash
# macOS
brew install protobuf

# Ubuntu/Debian
sudo apt update
sudo apt install -y protobuf-compiler

# Verify installation
protoc --version
```

### Project Structure

```
code/rust/
├── rest/
│   ├── server/
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── main.rs
│   │       ├── models.rs
│   │       ├── handlers.rs
│   │       └── state.rs
│   └── client/
│       ├── Cargo.toml
│       └── src/
│           └── main.rs
└── grpc/
    ├── server/
    │   ├── Cargo.toml
    │   ├── build.rs
    │   └── src/
    │       └── main.rs
    └── client/
        ├── Cargo.toml
        ├── build.rs
        └── src/
            └── main.rs
```

## Implementing the REST API with Actix-web

### Dependencies Configuration

Actix-web provides a powerful, pragmatic, and extremely fast web framework:

```toml
[package]
name = "task-rest-server"
version = "1.0.0"
edition = "2021"

[dependencies]
actix-web = "4.4"
actix-cors = "0.6"
tokio = { version = "1.35", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
chrono = { version = "0.4", features = ["serde"] }
uuid = { version = "1.6", features = ["v4", "serde"] }
env_logger = "0.11"
log = "0.4"
validator = { version = "0.16", features = ["derive"] }
utoipa = { version = "4.1", features = ["actix_extras", "chrono", "uuid"] }
utoipa-swagger-ui = { version = "5.0", features = ["actix-web"] }
```

### Domain Models with Serde

Rust's type system and serde provide excellent serialization:

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use uuid::Uuid;
use validator::Validate;

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema, PartialEq)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum TaskStatus {
    Pending,
    InProgress,
    Completed,
    Cancelled,
    Archived,
}

#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
#[serde(rename_all = "camelCase")]
pub struct Task {
    pub id: String,
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub status: TaskStatus,
    pub priority: TaskPriority,
    #[serde(default)]
    pub tags: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assigned_to: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub due_date: Option<DateTime<Utc>>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

impl Task {
    pub fn new(title: String) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4().to_string(),
            title,
            description: None,
            status: TaskStatus::Pending,
            priority: TaskPriority::Medium,
            tags: Vec::new(),
            assigned_to: None,
            due_date: None,
            created_at: now,
            updated_at: now,
            created_by: None,
            updated_by: None,
        }
    }
}

#[derive(Debug, Deserialize, Validate, ToSchema)]
#[serde(rename_all = "camelCase")]
pub struct CreateTaskRequest {
    #[validate(length(min = 1, max = 200))]
    pub title: String,
    pub description: Option<String>,
    pub priority: Option<TaskPriority>,
    #[serde(default)]
    pub tags: Vec<String>,
    pub assigned_to: Option<String>,
    pub due_date: Option<DateTime<Utc>>,
}
```

### Application State

Thread-safe state management with RwLock:

```rust
use std::collections::HashMap;
use std::sync::RwLock;

pub struct AppState {
    pub tasks: RwLock<HashMap<String, Task>>,
}

impl AppState {
    pub fn new() -> Self {
        let mut tasks = HashMap::new();
        
        // Initialize with sample data
        let mut task1 = Task::new("Complete project documentation".to_string());
        task1.description = Some("Write comprehensive documentation".to_string());
        task1.status = TaskStatus::InProgress;
        task1.priority = TaskPriority::High;
        task1.tags = vec!["documentation".to_string(), "api".to_string()];
        task1.assigned_to = Some("dev-team".to_string());
        tasks.insert(task1.id.clone(), task1);
        
        Self {
            tasks: RwLock::new(tasks),
        }
    }
}
```

### Request Handlers

Type-safe handlers with automatic JSON serialization:

```rust
#[utoipa::path(
    get,
    path = "/api/v1/tasks",
    tag = "tasks",
    responses(
        (status = 200, description = "List tasks successfully", body = TasksResponse)
    ),
    params(
        ("page_size" = Option<usize>, Query, description = "Number of items per page"),
        ("page_token" = Option<String>, Query, description = "Token for the next page"),
        ("status" = Option<TaskStatus>, Query, description = "Filter by status"),
    )
)]
pub async fn list_tasks(
    state: web::Data<AppState>,
    query: web::Query<QueryParams>,
) -> Result<HttpResponse> {
    let tasks = state.tasks.read().unwrap();
    
    let mut filtered_tasks: Vec<Task> = tasks
        .values()
        .filter(|task| {
            if let Some(ref status) = query.status {
                if task.status != *status {
                    return false;
                }
            }
            if let Some(ref assigned_to) = query.assigned_to {
                if task.assigned_to.as_ref() != Some(assigned_to) {
                    return false;
                }
            }
            true
        })
        .cloned()
        .collect();
    
    // Sort tasks
    match query.sort_order.as_deref() {
        Some("priority_desc") => {
            filtered_tasks.sort_by(|a, b| b.priority.cmp(&a.priority))
        },
        Some("created_desc") => {
            filtered_tasks.sort_by(|a, b| b.created_at.cmp(&a.created_at))
        },
        _ => filtered_tasks.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
    }
    
    // Apply pagination
    let page_size = query.page_size.unwrap_or(20).min(100);
    let start_index = query.page_token
        .as_ref()
        .and_then(|t| t.parse::<usize>().ok())
        .unwrap_or(0);
    
    let total_count = filtered_tasks.len();
    let end_index = (start_index + page_size).min(total_count);
    let items = filtered_tasks[start_index..end_index].to_vec();
    
    let next_page_token = if end_index < total_count {
        Some(end_index.to_string())
    } else {
        None
    };
    
    Ok(HttpResponse::Ok().json(TasksResponse {
        items,
        page_size,
        next_page_token,
        total_count,
    }))
}

#[utoipa::path(
    post,
    path = "/api/v1/tasks",
    tag = "tasks",
    request_body = CreateTaskRequest,
    responses(
        (status = 201, description = "Task created", body = Task),
        (status = 400, description = "Invalid request", body = ErrorResponse)
    )
)]
pub async fn create_task(
    state: web::Data<AppState>,
    req: web::Json<CreateTaskRequest>,
) -> Result<HttpResponse> {
    // Validate request
    if let Err(e) = req.validate() {
        return Ok(HttpResponse::BadRequest().json(ErrorResponse {
            error: "validation_error".to_string(),
            message: format!("Validation failed: {:?}", e),
        }));
    }
    
    let mut task = Task::new(req.title.clone());
    
    // Apply optional fields
    if let Some(ref desc) = req.description {
        task.description = Some(desc.clone());
    }
    if let Some(ref priority) = req.priority {
        task.priority = priority.clone();
    }
    task.tags = req.tags.clone();
    
    // Store task
    let mut tasks = state.tasks.write().unwrap();
    tasks.insert(task.id.clone(), task.clone());
    
    Ok(HttpResponse::Created().json(task))
}
```

### Main Application

Setting up the Actix-web server:

```rust
use actix_cors::Cors;
use actix_web::{web, App, HttpServer, middleware};
use env_logger::Env;
use utoipa::OpenApi;
use utoipa_swagger_ui::SwaggerUi;

#[derive(OpenApi)]
#[openapi(
    paths(
        handlers::list_tasks,
        handlers::get_task,
        handlers::create_task,
        handlers::update_task,
        handlers::update_task_status,
        handlers::delete_task,
    ),
    components(schemas(/* ... */)),
    tags(
        (name = "tasks", description = "Task management endpoints")
    ),
    info(
        title = "Task Management API",
        version = "1.0.0",
        description = "RESTful API for managing tasks built with Actix-web"
    )
)]
struct ApiDoc;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(Env::default().default_filter_or("info"));
    
    log::info!("Starting Rust REST server on port 8080...");
    
    let app_state = web::Data::new(state::AppState::new());
    
    HttpServer::new(move || {
        let cors = Cors::default()
            .allow_any_origin()
            .allow_any_method()
            .allow_any_header()
            .max_age(3600);
        
        App::new()
            .app_data(app_state.clone())
            .wrap(cors)
            .wrap(middleware::Logger::default())
            .service(
                web::scope("/api/v1")
                    .route("/tasks", web::get().to(handlers::list_tasks))
                    .route("/tasks", web::post().to(handlers::create_task))
                    .route("/tasks/{id}", web::get().to(handlers::get_task))
                    .route("/tasks/{id}", web::put().to(handlers::update_task))
                    .route("/tasks/{id}/status", 
                           web::patch().to(handlers::update_task_status))
                    .route("/tasks/{id}", web::delete().to(handlers::delete_task))
            )
            .service(
                SwaggerUi::new("/swagger-ui/{_:.*}")
                    .url("/api-docs/openapi.json", ApiDoc::openapi())
            )
    })
    .bind(("0.0.0.0", 8080))?
    .run()
    .await
}
```

### Running the REST Server

```bash
cd code/rust/rest/server
cargo run

# In release mode for optimal performance
cargo run --release

# Access Swagger UI
open http://localhost:8080/swagger-ui/
```

## Implementing the REST Client

### Async HTTP Client with Reqwest

```rust
use anyhow::Result;
use reqwest::Client;
use serde::{Deserialize, Serialize};

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
        // Add other parameters...
        
        let response = self.client
            .get(format!("{}/api/v1/tasks", self.base_url))
            .query(&params)
            .send()
            .await?;
        
        response.json::<TasksResponse>().await.map_err(Into::into)
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
    
    pub async fn update_task_status(
        &self,
        task_id: &str,
        status: TaskStatus,
    ) -> Result<Option<Task>> {
        let request = UpdateStatusRequest { status };
        
        let response = self.client
            .patch(format!("{}/api/v1/tasks/{}/status", 
                          self.base_url, task_id))
            .json(&request)
            .send()
            .await?;
        
        if response.status() == 404 {
            return Ok(None);
        }
        
        response.json::<Task>().await.map(Some).map_err(Into::into)
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let client = TaskApiClient::new("http://localhost:8080");
    
    // Create a task
    let task = client.create_task(
        "Test Rust REST Client",
        Some("Testing the Rust REST client implementation"),
        Some(TaskPriority::High),
        vec!["test".to_string(), "rust".to_string()],
        Some("dev-team"),
    ).await?;
    
    println!("Created task: {} - {}", task.id, task.title);
    
    // Update status
    if let Some(updated) = client.update_task_status(
        &task.id, 
        TaskStatus::InProgress
    ).await? {
        println!("Updated task status: {:?}", updated.status);
    }
    
    Ok(())
}
```

## Implementing gRPC Services with Tonic

### Build Configuration

Tonic provides code generation from proto files:

```rust
// build.rs
fn main() -> Result<(), Box<dyn std::error::Error>> {
    tonic_build::configure()
        .build_server(true)
        .build_client(false)
        .out_dir("src")
        .compile(
            &["../../../shared/protos/tasks.proto"],
            &["../../../shared/protos"],
        )?;
    Ok(())
}
```

### gRPC Service Implementation

```rust
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::{Arc, RwLock};
use tokio::sync::mpsc;
use tokio_stream::{wrappers::ReceiverStream, Stream, StreamExt};
use tonic::{transport::Server, Request, Response, Status, Streaming};

// Include generated proto code
pub mod tasks {
    include!("tasks.rs");
}

use tasks::{
    task_service_server::{TaskService, TaskServiceServer},
    CreateTaskRequest, DeleteTaskRequest, GetTaskRequest, 
    ListTasksRequest, Task, TaskEvent, UpdateTaskRequest, 
    WatchTasksRequest, Empty,
};

#[derive(Debug, Default)]
pub struct TaskServiceImpl {
    tasks: Arc<RwLock<HashMap<String, Task>>>,
}

#[tonic::async_trait]
impl TaskService for TaskServiceImpl {
    type ListTasksStream = Pin<Box<
        dyn Stream<Item = Result<Task, Status>> + Send
    >>;
    
    async fn list_tasks(
        &self,
        request: Request<ListTasksRequest>,
    ) -> Result<Response<Self::ListTasksStream>, Status> {
        let req = request.into_inner();
        let tasks = self.tasks.read().unwrap();
        
        // Filter tasks
        let mut filtered_tasks: Vec<Task> = tasks
            .values()
            .filter(|task| {
                if req.status != 0 && task.status != req.status {
                    return false;
                }
                if !req.assigned_to.is_empty() && 
                   task.assigned_to != req.assigned_to {
                    return false;
                }
                true
            })
            .cloned()
            .collect();
        
        // Sort tasks
        match req.sort_order {
            1 => filtered_tasks.sort_by(|a, b| 
                b.created_at.cmp(&a.created_at)),
            2 => filtered_tasks.sort_by(|a, b| 
                b.updated_at.cmp(&a.updated_at)),
            3 => filtered_tasks.sort_by(|a, b| 
                b.priority.cmp(&a.priority)),
            _ => {}
        }
        
        // Apply pagination
        let page_size = if req.page_size > 0 { 
            req.page_size as usize 
        } else { 
            20 
        }.min(100);
        
        let start_index = if !req.page_token.is_empty() {
            req.page_token.parse::<usize>().unwrap_or(0)
        } else {
            0
        };
        
        let end_index = (start_index + page_size)
            .min(filtered_tasks.len());
        let paginated_tasks = filtered_tasks[start_index..end_index]
            .to_vec();
        
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
    
    async fn create_task(
        &self,
        request: Request<CreateTaskRequest>,
    ) -> Result<Response<Task>, Status> {
        let req = request.into_inner();
        let mut task = req.task
            .ok_or_else(|| Status::invalid_argument("Task data is required"))?;
        
        // Validate
        if task.title.is_empty() {
            return Err(Status::invalid_argument("Title is required"));
        }
        if task.title.len() > 200 {
            return Err(Status::invalid_argument(
                "Title must be 200 characters or less"
            ));
        }
        
        // Generate ID and timestamps
        task.id = Uuid::new_v4().to_string();
        let now = prost_types::Timestamp {
            seconds: Utc::now().timestamp(),
            nanos: 0,
        };
        task.created_at = Some(now.clone());
        task.updated_at = Some(now);
        
        // Store task
        let mut tasks = self.tasks.write().unwrap();
        tasks.insert(task.id.clone(), task.clone());
        
        Ok(Response::new(task))
    }
    
    type WatchTasksStream = Pin<Box<
        dyn Stream<Item = Result<TaskEvent, Status>> + Send
    >>;
    
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
                                    event_type: 2, // UPDATED
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
```

### gRPC Server

```rust
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
```

### gRPC Client

```rust
use tokio_stream::StreamExt;
use tonic::{transport::Channel, Request};

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
        
        let mut stream = self.client.list_tasks(request)
            .await?
            .into_inner();
        let mut tasks = Vec::new();
        
        while let Some(task) = stream.next().await {
            tasks.push(task?);
        }
        
        Ok(tasks)
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
```

## Advanced Features

### Error Handling

Custom error types with thiserror:

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TaskError {
    #[error("Task not found: {0}")]
    NotFound(String),
    
    #[error("Validation error: {0}")]
    ValidationError(String),
    
    #[error("Database error: {0}")]
    DatabaseError(#[from] sqlx::Error),
    
    #[error("Internal server error")]
    InternalError,
}

impl actix_web::error::ResponseError for TaskError {
    fn error_response(&self) -> HttpResponse {
        match self {
            TaskError::NotFound(msg) => {
                HttpResponse::NotFound().json(json!({
                    "error": "not_found",
                    "message": msg
                }))
            }
            TaskError::ValidationError(msg) => {
                HttpResponse::BadRequest().json(json!({
                    "error": "validation_error",
                    "message": msg
                }))
            }
            _ => HttpResponse::InternalServerError().json(json!({
                "error": "internal_error",
                "message": "An internal error occurred"
            }))
        }
    }
}
```

### Database Integration with SQLx

Type-safe SQL queries:

```rust
use sqlx::{PgPool, FromRow};

#[derive(FromRow)]
pub struct TaskEntity {
    pub id: String,
    pub title: String,
    pub description: Option<String>,
    pub status: String,
    pub priority: String,
    pub tags: Vec<String>,
    pub assigned_to: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

pub struct TaskRepository {
    pool: PgPool,
}

impl TaskRepository {
    pub async fn list_tasks(
        &self,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<TaskEntity>> {
        let tasks = sqlx::query_as!(
            TaskEntity,
            r#"
            SELECT id, title, description, status, priority, 
                   tags, assigned_to, created_at, updated_at
            FROM tasks
            ORDER BY created_at DESC
            LIMIT $1 OFFSET $2
            "#,
            limit,
            offset
        )
        .fetch_all(&self.pool)
        .await?;
        
        Ok(tasks)
    }
    
    pub async fn create_task(&self, task: &Task) -> Result<TaskEntity> {
        let result = sqlx::query_as!(
            TaskEntity,
            r#"
            INSERT INTO tasks (id, title, description, status, priority, 
                              tags, assigned_to, created_at, updated_at)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
            RETURNING *
            "#,
            task.id,
            task.title,
            task.description,
            task.status.to_string(),
            task.priority.to_string(),
            &task.tags,
            task.assigned_to,
            task.created_at,
            task.updated_at
        )
        .fetch_one(&self.pool)
        .await?;
        
        Ok(result)
    }
}
```

### Middleware and Guards

Custom middleware for authentication:

```rust
use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;

pub struct AuthMiddleware;

impl<S, B> Transform<S, ServiceRequest> for AuthMiddleware
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = AuthMiddlewareService<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;
    
    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AuthMiddlewareService { service }))
    }
}

pub struct AuthMiddlewareService<S> {
    service: S,
}

impl<S, B> Service<ServiceRequest> for AuthMiddlewareService<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;
    
    forward_ready!(service);
    
    fn call(&self, req: ServiceRequest) -> Self::Future {
        let token = req.headers()
            .get("Authorization")
            .and_then(|h| h.to_str().ok())
            .and_then(|h| h.strip_prefix("Bearer "));
        
        if let Some(token) = token {
            // Validate token
            if validate_token(token) {
                req.extensions_mut().insert(AuthUser {
                    id: "user123".to_string(),
                    role: "admin".to_string(),
                });
            }
        }
        
        let fut = self.service.call(req);
        Box::pin(fut)
    }
}
```

### Performance Optimization

Using Arc for shared immutable data:

```rust
use std::sync::Arc;
use dashmap::DashMap;

pub struct OptimizedState {
    // DashMap provides better concurrent performance than RwLock<HashMap>
    tasks: Arc<DashMap<String, Task>>,
    // Cache frequently accessed data
    cache: Arc<DashMap<String, CachedData>>,
}

impl OptimizedState {
    pub fn get_task(&self, id: &str) -> Option<Task> {
        // Check cache first
        if let Some(cached) = self.cache.get(id) {
            if cached.is_valid() {
                return Some(cached.task.clone());
            }
        }
        
        // Get from storage
        if let Some(task) = self.tasks.get(id) {
            let task = task.clone();
            // Update cache
            self.cache.insert(id.to_string(), CachedData::new(task.clone()));
            Some(task)
        } else {
            None
        }
    }
}
```

## Testing

### Unit Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_task_creation() {
        let task = Task::new("Test Task".to_string());
        
        assert!(!task.id.is_empty());
        assert_eq!(task.title, "Test Task");
        assert_eq!(task.status, TaskStatus::Pending);
        assert_eq!(task.priority, TaskPriority::Medium);
    }
    
    #[tokio::test]
    async fn test_list_tasks_pagination() {
        let state = AppState::new();
        
        // Add test tasks
        let mut tasks = state.tasks.write().unwrap();
        for i in 0..25 {
            let task = Task::new(format!("Task {}", i));
            tasks.insert(task.id.clone(), task);
        }
        drop(tasks);
        
        // Test pagination
        let result = list_tasks_with_params(&state, 10, None).await;
        assert_eq!(result.items.len(), 10);
        assert!(result.next_page_token.is_some());
    }
}
```

### Integration Testing

```rust
#[cfg(test)]
mod integration_tests {
    use actix_web::{test, App};
    
    #[actix_web::test]
    async fn test_create_and_get_task() {
        let app = test::init_service(
            App::new()
                .app_data(web::Data::new(AppState::new()))
                .service(web::scope("/api/v1")
                    .route("/tasks", web::post().to(create_task))
                    .route("/tasks/{id}", web::get().to(get_task))
                )
        ).await;
        
        // Create task
        let create_req = test::TestRequest::post()
            .uri("/api/v1/tasks")
            .set_json(&json!({
                "title": "Integration Test Task",
                "priority": "HIGH"
            }))
            .to_request();
        
        let resp = test::call_service(&app, create_req).await;
        assert!(resp.status().is_success());
        
        let task: Task = test::read_body_json(resp).await;
        assert_eq!(task.title, "Integration Test Task");
        
        // Get task
        let get_req = test::TestRequest::get()
            .uri(&format!("/api/v1/tasks/{}", task.id))
            .to_request();
        
        let resp = test::call_service(&app, get_req).await;
        assert!(resp.status().is_success());
    }
}
```

## Deployment

### Building for Production

```bash
# Build optimized binary
cargo build --release

# Run tests
cargo test

# Check for common issues
cargo clippy

# Format code
cargo fmt

# Create minimal Docker image
docker build -t task-api-rust .
```

### Multi-stage Dockerfile

```dockerfile
# Build stage
FROM rust:1.75 as builder

WORKDIR /app
COPY Cargo.toml Cargo.lock ./
COPY src ./src

# Build dependencies separately for caching
RUN cargo build --release

# Runtime stage
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libssl3 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/task-rest-server /usr/local/bin/

EXPOSE 8080 50051

CMD ["task-rest-server"]
```

### Performance Tuning

```rust
// Cargo.toml optimizations
[profile.release]
opt-level = 3
lto = true
codegen-units = 1
strip = true
panic = "abort"

// Runtime configuration
#[tokio::main(flavor = "multi_thread", worker_threads = 4)]
async fn main() {
    // Application code
}
```

## Best Practices

### 1. Use the Type System

```rust
// Use newtypes for domain concepts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskId(String);

impl TaskId {
    pub fn new() -> Self {
        Self(Uuid::new_v4().to_string())
    }
    
    pub fn parse(s: &str) -> Result<Self, ParseError> {
        // Validation logic
        Ok(Self(s.to_string()))
    }
}

// Use enums for state machines
pub enum TaskLifecycle {
    Draft { created_at: DateTime<Utc> },
    Active { started_at: DateTime<Utc> },
    Completed { finished_at: DateTime<Utc> },
    Archived { archived_at: DateTime<Utc> },
}
```

### 2. Handle Errors Explicitly

```rust
pub async fn safe_operation() -> Result<Task, TaskError> {
    let task = get_task()
        .await
        .map_err(|e| TaskError::DatabaseError(e))?;
    
    validate_task(&task)
        .map_err(|e| TaskError::ValidationError(e.to_string()))?;
    
    process_task(task)
        .await
        .map_err(|_| TaskError::InternalError)
}
```

### 3. Use Iterators and Functional Patterns

```rust
let high_priority_tasks: Vec<Task> = tasks
    .iter()
    .filter(|t| t.priority == TaskPriority::High)
    .filter(|t| t.status != TaskStatus::Completed)
    .take(10)
    .cloned()
    .collect();

let total_tags: HashSet<String> = tasks
    .iter()
    .flat_map(|t| t.tags.iter())
    .cloned()
    .collect();
```

## Conclusion

Rust brings unprecedented safety and performance to API development. The combination of memory safety without garbage collection, zero-cost abstractions, and fearless concurrency makes it ideal for building high-performance, reliable APIs. Actix-web and Tonic leverage Rust's strengths to deliver exceptional throughput while preventing entire classes of bugs at compile time.

Key takeaways:
- Memory safety without runtime overhead
- Fearless concurrency through ownership
- Expressive type system catches errors early
- Zero-cost abstractions deliver C-like performance
- Excellent tooling with Cargo and rustfmt
- Growing ecosystem of high-quality libraries
- Predictable performance without GC pauses

While Rust has a steeper learning curve than many languages, the investment pays off in production systems where reliability and performance are critical. The compiler acts as a pair programmer, catching issues that would be runtime errors in other languages, making Rust an excellent choice for mission-critical API services.