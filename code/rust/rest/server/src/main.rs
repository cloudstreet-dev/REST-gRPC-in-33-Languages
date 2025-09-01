mod models;
mod handlers;
mod state;

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
    components(
        schemas(
            models::Task,
            models::TaskStatus,
            models::TaskPriority,
            models::CreateTaskRequest,
            models::UpdateTaskRequest,
            models::UpdateStatusRequest,
            models::TasksResponse,
            models::ErrorResponse,
        )
    ),
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
                    .route("/tasks/{id}/status", web::patch().to(handlers::update_task_status))
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