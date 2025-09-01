use crate::models::*;
use crate::state::AppState;
use actix_web::{web, HttpResponse, Result};
use chrono::Utc;
use utoipa::Path;
use uuid::Uuid;
use validator::Validate;

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
        ("assigned_to" = Option<String>, Query, description = "Filter by assigned user"),
        ("tags" = Option<String>, Query, description = "Filter by tags (comma-separated)"),
        ("sort_order" = Option<String>, Query, description = "Sort order")
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
            if let Some(ref tags_str) = query.tags {
                let required_tags: Vec<String> = tags_str.split(',').map(|s| s.to_string()).collect();
                if !required_tags.iter().all(|tag| task.tags.contains(tag)) {
                    return false;
                }
            }
            true
        })
        .cloned()
        .collect();
    
    // Sort tasks
    match query.sort_order.as_deref() {
        Some("created_desc") => filtered_tasks.sort_by(|a, b| b.created_at.cmp(&a.created_at)),
        Some("updated_desc") => filtered_tasks.sort_by(|a, b| b.updated_at.cmp(&a.updated_at)),
        Some("priority_desc") => filtered_tasks.sort_by(|a, b| b.priority.cmp(&a.priority)),
        _ => filtered_tasks.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
    }
    
    // Pagination
    let page_size = query.page_size.unwrap_or(20).min(100);
    let start_index = if let Some(ref token) = query.page_token {
        token.parse::<usize>().unwrap_or(0)
    } else {
        0
    };
    
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
    get,
    path = "/api/v1/tasks/{id}",
    tag = "tasks",
    responses(
        (status = 200, description = "Task found", body = Task),
        (status = 404, description = "Task not found", body = ErrorResponse)
    ),
    params(
        ("id" = String, Path, description = "Task ID")
    )
)]
pub async fn get_task(
    state: web::Data<AppState>,
    path: web::Path<String>,
) -> Result<HttpResponse> {
    let task_id = path.into_inner();
    let tasks = state.tasks.read().unwrap();
    
    match tasks.get(&task_id) {
        Some(task) => Ok(HttpResponse::Ok().json(task)),
        None => Ok(HttpResponse::NotFound().json(ErrorResponse {
            error: "not_found".to_string(),
            message: format!("Task with ID {} not found", task_id),
        })),
    }
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
    if let Err(e) = req.validate() {
        return Ok(HttpResponse::BadRequest().json(ErrorResponse {
            error: "validation_error".to_string(),
            message: format!("Validation failed: {:?}", e),
        }));
    }
    
    let mut task = Task::new(req.title.clone());
    
    if let Some(ref desc) = req.description {
        task.description = Some(desc.clone());
    }
    if let Some(ref priority) = req.priority {
        task.priority = priority.clone();
    }
    if !req.tags.is_empty() {
        task.tags = req.tags.clone();
    }
    if let Some(ref assigned_to) = req.assigned_to {
        task.assigned_to = Some(assigned_to.clone());
    }
    if let Some(ref due_date) = req.due_date {
        task.due_date = Some(*due_date);
    }
    
    let mut tasks = state.tasks.write().unwrap();
    tasks.insert(task.id.clone(), task.clone());
    
    Ok(HttpResponse::Created().json(task))
}

#[utoipa::path(
    put,
    path = "/api/v1/tasks/{id}",
    tag = "tasks",
    request_body = UpdateTaskRequest,
    responses(
        (status = 200, description = "Task updated", body = Task),
        (status = 404, description = "Task not found", body = ErrorResponse),
        (status = 400, description = "Invalid request", body = ErrorResponse)
    ),
    params(
        ("id" = String, Path, description = "Task ID")
    )
)]
pub async fn update_task(
    state: web::Data<AppState>,
    path: web::Path<String>,
    req: web::Json<UpdateTaskRequest>,
) -> Result<HttpResponse> {
    if let Err(e) = req.validate() {
        return Ok(HttpResponse::BadRequest().json(ErrorResponse {
            error: "validation_error".to_string(),
            message: format!("Validation failed: {:?}", e),
        }));
    }
    
    let task_id = path.into_inner();
    let mut tasks = state.tasks.write().unwrap();
    
    match tasks.get_mut(&task_id) {
        Some(task) => {
            if let Some(ref title) = req.title {
                task.title = title.clone();
            }
            if let Some(ref desc) = req.description {
                task.description = Some(desc.clone());
            }
            if let Some(ref status) = req.status {
                task.status = status.clone();
            }
            if let Some(ref priority) = req.priority {
                task.priority = priority.clone();
            }
            if let Some(ref tags) = req.tags {
                task.tags = tags.clone();
            }
            if let Some(ref assigned_to) = req.assigned_to {
                task.assigned_to = Some(assigned_to.clone());
            }
            if let Some(ref due_date) = req.due_date {
                task.due_date = Some(*due_date);
            }
            
            task.updated_at = Utc::now();
            
            Ok(HttpResponse::Ok().json(task.clone()))
        }
        None => Ok(HttpResponse::NotFound().json(ErrorResponse {
            error: "not_found".to_string(),
            message: format!("Task with ID {} not found", task_id),
        })),
    }
}

#[utoipa::path(
    patch,
    path = "/api/v1/tasks/{id}/status",
    tag = "tasks",
    request_body = UpdateStatusRequest,
    responses(
        (status = 200, description = "Status updated", body = Task),
        (status = 404, description = "Task not found", body = ErrorResponse)
    ),
    params(
        ("id" = String, Path, description = "Task ID")
    )
)]
pub async fn update_task_status(
    state: web::Data<AppState>,
    path: web::Path<String>,
    req: web::Json<UpdateStatusRequest>,
) -> Result<HttpResponse> {
    let task_id = path.into_inner();
    let mut tasks = state.tasks.write().unwrap();
    
    match tasks.get_mut(&task_id) {
        Some(task) => {
            task.status = req.status.clone();
            task.updated_at = Utc::now();
            
            Ok(HttpResponse::Ok().json(task.clone()))
        }
        None => Ok(HttpResponse::NotFound().json(ErrorResponse {
            error: "not_found".to_string(),
            message: format!("Task with ID {} not found", task_id),
        })),
    }
}

#[utoipa::path(
    delete,
    path = "/api/v1/tasks/{id}",
    tag = "tasks",
    responses(
        (status = 204, description = "Task deleted"),
        (status = 404, description = "Task not found", body = ErrorResponse)
    ),
    params(
        ("id" = String, Path, description = "Task ID")
    )
)]
pub async fn delete_task(
    state: web::Data<AppState>,
    path: web::Path<String>,
) -> Result<HttpResponse> {
    let task_id = path.into_inner();
    let mut tasks = state.tasks.write().unwrap();
    
    match tasks.remove(&task_id) {
        Some(_) => Ok(HttpResponse::NoContent().finish()),
        None => Ok(HttpResponse::NotFound().json(ErrorResponse {
            error: "not_found".to_string(),
            message: format!("Task with ID {} not found", task_id),
        })),
    }
}