require "kemal"
require "json"
require "./models/task"
require "./services/task_service"

# Initialize service
task_service = Services::TaskService.new

# Configure Kemal
Kemal.config.env = "production"
Kemal.config.port = (ENV["PORT"]? || 3000).to_i

# CORS Middleware
before_all do |env|
  env.response.headers["Access-Control-Allow-Origin"] = "*"
  env.response.headers["Access-Control-Allow-Methods"] = "GET, POST, PUT, PATCH, DELETE, OPTIONS"
  env.response.headers["Access-Control-Allow-Headers"] = "Content-Type, Authorization, X-API-Key"
  env.response.content_type = "application/json"
end

# Handle OPTIONS requests for CORS
options "/*" do |env|
  env.response.headers["Access-Control-Max-Age"] = "86400"
  env.response.status_code = 204
  ""
end

# Error handlers
error 400 do |env, exc|
  Models::ErrorResponse.new("VALIDATION_ERROR", exc.message || "Bad Request").to_json
end

error 404 do |env, exc|
  Models::ErrorResponse.new("NOT_FOUND", exc.message || "Not Found").to_json
end

error 500 do |env, exc|
  Models::ErrorResponse.new("INTERNAL_ERROR", exc.message || "Internal Server Error").to_json
end

# Health check endpoint
get "/health" do |env|
  {
    status: "healthy",
    timestamp: Time.utc.to_rfc3339,
    service: "task-api-crystal",
    version: "1.0.0"
  }.to_json
end

# List tasks
get "/api/v1/tasks" do |env|
  params = env.params.query
  
  page_size = params["page_size"]?.try(&.to_i?) || 20
  page_token = params["page_token"]?
  status = params["status"]?
  assigned_to = params["assigned_to"]?
  tags = params["tags"]?
  sort_order = params["sort_order"]?
  
  begin
    response = task_service.list_tasks(
      page_size: page_size,
      page_token: page_token,
      status: status,
      assigned_to: assigned_to,
      tags: tags,
      sort_order: sort_order
    )
    response.to_json
  rescue ex : Services::ValidationError
    env.response.status_code = 400
    Models::ErrorResponse.new("VALIDATION_ERROR", ex.message || "Validation failed").to_json
  rescue ex
    env.response.status_code = 500
    Models::ErrorResponse.new("INTERNAL_ERROR", ex.message || "Internal error").to_json
  end
end

# Get task by ID
get "/api/v1/tasks/:id" do |env|
  id = env.params.url["id"]
  
  begin
    task = task_service.get_task(id)
    task.to_json
  rescue ex : Services::NotFoundError
    env.response.status_code = 404
    Models::ErrorResponse.new("NOT_FOUND", ex.message || "Task not found").to_json
  rescue ex
    env.response.status_code = 500
    Models::ErrorResponse.new("INTERNAL_ERROR", ex.message || "Internal error").to_json
  end
end

# Create task
post "/api/v1/tasks" do |env|
  begin
    body = env.request.body.try(&.gets_to_end) || "{}"
    request = Models::CreateTaskRequest.from_json(body)
    
    task = task_service.create_task(
      title: request.title,
      description: request.description,
      priority: request.priority,
      tags: request.tags,
      assigned_to: request.assigned_to,
      due_date: request.due_date
    )
    
    env.response.status_code = 201
    env.response.headers["Location"] = "/api/v1/tasks/#{task.id}"
    task.to_json
  rescue ex : JSON::ParseException
    env.response.status_code = 400
    Models::ErrorResponse.new("INVALID_JSON", "Invalid JSON in request body").to_json
  rescue ex : Services::ValidationError
    env.response.status_code = 400
    Models::ErrorResponse.new("VALIDATION_ERROR", ex.message || "Validation failed").to_json
  rescue ex
    env.response.status_code = 500
    Models::ErrorResponse.new("INTERNAL_ERROR", ex.message || "Internal error").to_json
  end
end

# Update task
put "/api/v1/tasks/:id" do |env|
  id = env.params.url["id"]
  
  begin
    body = env.request.body.try(&.gets_to_end) || "{}"
    request = Models::UpdateTaskRequest.from_json(body)
    
    task = task_service.update_task(id, request)
    task.to_json
  rescue ex : JSON::ParseException
    env.response.status_code = 400
    Models::ErrorResponse.new("INVALID_JSON", "Invalid JSON in request body").to_json
  rescue ex : Services::NotFoundError
    env.response.status_code = 404
    Models::ErrorResponse.new("NOT_FOUND", ex.message || "Task not found").to_json
  rescue ex : Services::ValidationError
    env.response.status_code = 400
    Models::ErrorResponse.new("VALIDATION_ERROR", ex.message || "Validation failed").to_json
  rescue ex
    env.response.status_code = 500
    Models::ErrorResponse.new("INTERNAL_ERROR", ex.message || "Internal error").to_json
  end
end

# Update task status
patch "/api/v1/tasks/:id/status" do |env|
  id = env.params.url["id"]
  
  begin
    body = env.request.body.try(&.gets_to_end) || "{}"
    request = Models::UpdateTaskStatusRequest.from_json(body)
    
    task = task_service.update_task_status(id, request.status)
    task.to_json
  rescue ex : JSON::ParseException
    env.response.status_code = 400
    Models::ErrorResponse.new("INVALID_JSON", "Invalid JSON in request body").to_json
  rescue ex : Services::NotFoundError
    env.response.status_code = 404
    Models::ErrorResponse.new("NOT_FOUND", ex.message || "Task not found").to_json
  rescue ex : Services::ValidationError
    env.response.status_code = 400
    Models::ErrorResponse.new("VALIDATION_ERROR", ex.message || "Validation failed").to_json
  rescue ex
    env.response.status_code = 500
    Models::ErrorResponse.new("INTERNAL_ERROR", ex.message || "Internal error").to_json
  end
end

# Delete task
delete "/api/v1/tasks/:id" do |env|
  id = env.params.url["id"]
  
  begin
    task_service.delete_task(id)
    env.response.status_code = 204
    ""
  rescue ex : Services::NotFoundError
    env.response.status_code = 404
    Models::ErrorResponse.new("NOT_FOUND", ex.message || "Task not found").to_json
  rescue ex
    env.response.status_code = 500
    Models::ErrorResponse.new("INTERNAL_ERROR", ex.message || "Internal error").to_json
  end
end

# Start the server
puts "Crystal REST API server starting on port #{Kemal.config.port}..."
Kemal.run