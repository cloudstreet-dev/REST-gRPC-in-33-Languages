require 'sinatra/base'
require 'sinatra/json'
require 'sinatra/cors'
require 'json'
require_relative 'services/task_service'
require_relative 'models/task'

class TaskAPI < Sinatra::Base
  register Sinatra::Cors
  
  set :allow_origin, "*"
  set :allow_methods, "GET,HEAD,POST,PUT,PATCH,DELETE,OPTIONS"
  set :allow_headers, "content-type,if-modified-since,authorization,x-api-key"
  set :expose_headers, "location,link"
  set :allow_credentials, true
  
  configure do
    set :show_exceptions, false
    set :raise_errors, false
  end
  
  def initialize
    super
    @task_service = Services::TaskService.new
  end
  
  # Middleware
  before do
    content_type :json
    
    # Parse JSON body for POST/PUT/PATCH requests
    if request.post? || request.put? || request.patch?
      request.body.rewind
      body = request.body.read
      unless body.empty?
        begin
          params.merge!(JSON.parse(body, symbolize_names: true))
        rescue JSON::ParserError => e
          halt 400, json(error: { code: 'INVALID_JSON', message: e.message })
        end
      end
    end
  end
  
  # Error handlers
  error Services::ValidationError do |e|
    status 400
    json(error: { code: 'VALIDATION_ERROR', message: e.message })
  end
  
  error Services::NotFoundError do |e|
    status 404
    json(error: { code: 'NOT_FOUND', message: e.message })
  end
  
  error StandardError do |e|
    status 500
    json(error: { code: 'INTERNAL_ERROR', message: e.message })
  end
  
  # Routes
  get '/health' do
    json(
      status: 'healthy',
      timestamp: Time.now.iso8601,
      service: 'task-api-ruby',
      version: '1.0.0'
    )
  end
  
  # List tasks
  get '/api/v1/tasks' do
    result = @task_service.list_tasks(
      page_size: params[:page_size] || 20,
      page_token: params[:page_token],
      status: params[:status],
      assigned_to: params[:assigned_to],
      tags: params[:tags],
      sort_order: params[:sort_order]
    )
    json(result)
  end
  
  # Get task by ID
  get '/api/v1/tasks/:id' do
    task = @task_service.get_task(params[:id])
    json(task)
  end
  
  # Create task
  post '/api/v1/tasks' do
    task = @task_service.create_task(params)
    status 201
    headers['Location'] = "/api/v1/tasks/#{task[:id]}"
    json(task)
  end
  
  # Update task
  put '/api/v1/tasks/:id' do
    task = @task_service.update_task(params[:id], params)
    json(task)
  end
  
  # Update task status
  patch '/api/v1/tasks/:id/status' do
    unless params[:status]
      halt 400, json(error: { code: 'VALIDATION_ERROR', message: 'Status is required' })
    end
    
    task = @task_service.update_task_status(params[:id], params[:status])
    json(task)
  end
  
  # Delete task
  delete '/api/v1/tasks/:id' do
    @task_service.delete_task(params[:id])
    status 204
    ''
  end
  
  # Handle OPTIONS for CORS
  options '*' do
    response.headers["Allow"] = "GET, POST, PUT, PATCH, DELETE, OPTIONS"
    response.headers["Access-Control-Allow-Headers"] = "Authorization, Content-Type, Accept, X-User-Email, X-Auth-Token, X-API-Key"
    response.headers["Access-Control-Allow-Origin"] = "*"
    200
  end
  
  run! if app_file == $0
end