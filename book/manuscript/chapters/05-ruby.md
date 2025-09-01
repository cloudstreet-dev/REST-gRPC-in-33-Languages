# Chapter 5: Ruby - Elegant APIs with Sinatra and gRPC

Ruby, created by Yukihiro "Matz" Matsumoto in 1995, is a dynamic, object-oriented programming language designed for programmer happiness and productivity. With its principle of least astonishment and focus on human-friendly syntax, Ruby has become a favorite for web development, particularly through the Ruby on Rails framework. In this chapter, we'll explore Ruby's capabilities for building both REST and gRPC APIs, leveraging its expressive syntax and rich ecosystem.

## Why Ruby for APIs?

Ruby brings several compelling advantages to API development:

1. **Developer Productivity**: Ruby's clean, expressive syntax allows developers to write less code while accomplishing more, leading to faster development cycles.

2. **Rich Ecosystem**: The RubyGems repository offers thousands of libraries for every conceivable need, from web frameworks to testing tools.

3. **Metaprogramming Power**: Ruby's dynamic nature and metaprogramming capabilities enable powerful abstractions and DSLs (Domain Specific Languages).

4. **Strong Community**: Ruby has a vibrant, welcoming community that values code quality, testing, and developer happiness.

5. **Battle-Tested Frameworks**: Frameworks like Rails and Sinatra have powered countless production applications, proving Ruby's reliability.

## Ruby in the API Landscape

Ruby has established itself as a significant player in the API development space:

- **Rails API Mode**: Ruby on Rails offers a streamlined API-only mode, perfect for building robust backend services
- **Sinatra**: A lightweight, flexible framework ideal for microservices and simple APIs
- **Grape**: A REST-like API framework designed to run on Rack or complement existing web applications
- **Hanami**: A modern web framework emphasizing simplicity, stability, and security

Ruby's influence extends beyond its own ecosystem. Many API design patterns and best practices originated in the Ruby community, particularly around RESTful design and developer ergonomics.

## Setting Up Ruby

Ruby offers multiple installation methods depending on your operating system and preferences:

### macOS

```bash
# Using Homebrew
brew install ruby

# Using rbenv (recommended for version management)
brew install rbenv ruby-build
rbenv install 3.3.0
rbenv global 3.3.0

# Add to your shell profile
echo 'eval "$(rbenv init -)"' >> ~/.zshrc
```

### Linux

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install ruby-full

# Using rbenv
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(rbenv init -)"' >> ~/.bashrc
exec $SHELL
rbenv install 3.3.0
rbenv global 3.3.0
```

### Windows

```powershell
# Using RubyInstaller
# Download from https://rubyinstaller.org/
# Run the installer and follow the prompts

# Using Chocolatey
choco install ruby
```

### Verifying Installation

```bash
ruby --version
# ruby 3.3.0

gem --version
# 3.5.3

bundler --version
# Bundler version 2.5.3
```

## REST API with Sinatra

Sinatra is a DSL for quickly creating web applications in Ruby with minimal effort. It's perfect for building lightweight APIs and microservices.

### Project Structure

```
code/ruby/rest/
├── server/
│   ├── Gemfile
│   ├── config.ru
│   ├── app.rb
│   ├── models/
│   │   └── task.rb
│   └── services/
│       └── task_service.rb
└── client/
    ├── Gemfile
    └── task_api_client.rb
```

### Installing Dependencies

```bash
cd code/ruby/rest/server
bundle install

# For development with auto-reload
bundle add rerun --group development
```

### Implementing the Task Model

```ruby
# models/task.rb
require 'json'
require 'time'
require 'securerandom'

module Models
  class Task
    VALID_STATUSES = %w[pending in_progress completed cancelled on_hold].freeze
    VALID_PRIORITIES = %w[low medium high critical].freeze
    
    attr_accessor :id, :title, :description, :status, :priority, :tags,
                  :created_by, :assigned_to, :created_at, :updated_at,
                  :due_date, :completed_at
    
    def initialize(attrs = {})
      @id = attrs[:id] || SecureRandom.uuid
      @title = attrs[:title] || ''
      @description = attrs[:description] || ''
      @status = attrs[:status] || 'pending'
      @priority = attrs[:priority] || 'medium'
      @tags = attrs[:tags] || []
      @created_by = attrs[:created_by] || 'system'
      @assigned_to = attrs[:assigned_to]
      @created_at = attrs[:created_at] || Time.now
      @updated_at = attrs[:updated_at] || Time.now
      @due_date = attrs[:due_date]
      @completed_at = attrs[:completed_at]
    end
    
    def valid?
      return false if @title.nil? || @title.empty?
      return false if @title.length > 200
      return false unless VALID_STATUSES.include?(@status)
      return false unless VALID_PRIORITIES.include?(@priority)
      true
    end
    
    def validation_errors
      errors = []
      errors << "Title is required" if @title.nil? || @title.empty?
      errors << "Title must be 200 characters or less" if @title && @title.length > 200
      errors << "Invalid status: #{@status}" unless VALID_STATUSES.include?(@status)
      errors << "Invalid priority: #{@priority}" unless VALID_PRIORITIES.include?(@priority)
      errors
    end
    
    def update!(attrs)
      @title = attrs[:title] if attrs.key?(:title)
      @description = attrs[:description] if attrs.key?(:description)
      @status = attrs[:status] if attrs.key?(:status)
      @priority = attrs[:priority] if attrs.key?(:priority)
      @tags = attrs[:tags] if attrs.key?(:tags)
      @assigned_to = attrs[:assigned_to] if attrs.key?(:assigned_to)
      @due_date = attrs[:due_date] if attrs.key?(:due_date)
      
      # Update completed_at if status changes to completed
      if @status == 'completed' && @completed_at.nil?
        @completed_at = Time.now
      end
      
      @updated_at = Time.now
      self
    end
    
    def to_h
      {
        id: @id,
        title: @title,
        description: @description,
        status: @status,
        priority: @priority,
        tags: @tags,
        created_by: @created_by,
        assigned_to: @assigned_to,
        created_at: @created_at.iso8601,
        updated_at: @updated_at.iso8601,
        due_date: @due_date&.iso8601,
        completed_at: @completed_at&.iso8601
      }.compact
    end
    
    def to_json(*args)
      to_h.to_json(*args)
    end
  end
end
```

### Creating the Service Layer

```ruby
# services/task_service.rb
require_relative '../models/task'

module Services
  class TaskService
    def initialize
      @tasks = {}
      @mutex = Mutex.new
      initialize_sample_data
    end
    
    def list_tasks(page_size: 20, page_token: nil, status: nil, 
                   assigned_to: nil, tags: nil, sort_order: nil)
      @mutex.synchronize do
        # Filter tasks
        filtered_tasks = @tasks.values.select do |task|
          task.matches_filters?(status: status, assigned_to: assigned_to, tags: tags)
        end
        
        # Sort tasks
        sorted_tasks = sort_tasks(filtered_tasks, sort_order)
        
        # Paginate
        page_size = [[page_size.to_i, 1].max, 100].min
        start_index = page_token ? page_token.to_i : 0
        end_index = start_index + page_size
        
        tasks_page = sorted_tasks[start_index...end_index] || []
        
        next_page_token = end_index < sorted_tasks.length ? end_index.to_s : nil
        
        {
          tasks: tasks_page.map(&:to_h),
          next_page_token: next_page_token,
          total_count: sorted_tasks.length
        }
      end
    end
    
    def get_task(id)
      @mutex.synchronize do
        task = @tasks[id]
        raise NotFoundError, "Task with ID #{id} not found" unless task
        task.to_h
      end
    end
    
    def create_task(attrs)
      task = Models::Task.new(attrs)
      
      unless task.valid?
        raise ValidationError, task.validation_errors.join(', ')
      end
      
      @mutex.synchronize do
        @tasks[task.id] = task
      end
      
      task.to_h
    end
    
    def update_task(id, attrs)
      @mutex.synchronize do
        task = @tasks[id]
        raise NotFoundError, "Task with ID #{id} not found" unless task
        
        task.update!(attrs)
        
        unless task.valid?
          raise ValidationError, task.validation_errors.join(', ')
        end
        
        task.to_h
      end
    end
    
    def delete_task(id)
      @mutex.synchronize do
        task = @tasks.delete(id)
        raise NotFoundError, "Task with ID #{id} not found" unless task
        nil
      end
    end
    
    private
    
    def sort_tasks(tasks, sort_order)
      case sort_order
      when 'created_at_asc'
        tasks.sort_by(&:created_at)
      when 'created_at_desc'
        tasks.sort_by(&:created_at).reverse
      when 'due_date_asc'
        tasks.sort_by { |t| t.due_date || Time.new(9999) }
      when 'due_date_desc'
        tasks.sort_by { |t| t.due_date || Time.new(0) }.reverse
      when 'priority_asc'
        tasks.sort_by(&:priority_value)
      when 'priority_desc'
        tasks.sort_by(&:priority_value).reverse
      else
        tasks
      end
    end
  end
  
  class ValidationError < StandardError; end
  class NotFoundError < StandardError; end
end
```

### Building the Sinatra Application

```ruby
# app.rb
require 'sinatra/base'
require 'sinatra/json'
require 'sinatra/cors'
require 'json'
require_relative 'services/task_service'
require_relative 'models/task'

class TaskAPI < Sinatra::Base
  register Sinatra::Cors
  
  # Configure CORS
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
  
  # Middleware for JSON parsing
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
  
  # Health check endpoint
  get '/health' do
    json(
      status: 'healthy',
      timestamp: Time.now.iso8601,
      service: 'task-api-ruby',
      version: '1.0.0'
    )
  end
  
  # Task endpoints
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
  
  get '/api/v1/tasks/:id' do
    task = @task_service.get_task(params[:id])
    json(task)
  end
  
  post '/api/v1/tasks' do
    task = @task_service.create_task(params)
    status 201
    headers['Location'] = "/api/v1/tasks/#{task[:id]}"
    json(task)
  end
  
  put '/api/v1/tasks/:id' do
    task = @task_service.update_task(params[:id], params)
    json(task)
  end
  
  patch '/api/v1/tasks/:id/status' do
    unless params[:status]
      halt 400, json(error: { code: 'VALIDATION_ERROR', message: 'Status is required' })
    end
    
    task = @task_service.update_task_status(params[:id], params[:status])
    json(task)
  end
  
  delete '/api/v1/tasks/:id' do
    @task_service.delete_task(params[:id])
    status 204
    ''
  end
end
```

### Running the Server

```bash
# Development with auto-reload
bundle exec rerun -- rackup -p 4567

# Production
bundle exec rackup -p 4567

# Or directly with Ruby
ruby app.rb -p 4567
```

## Ruby REST Client

Creating a clean, reusable client library for our REST API:

### HTTParty Client Implementation

```ruby
# task_api_client.rb
require 'httparty'
require 'json'

class TaskAPIClient
  include HTTParty
  
  def initialize(base_url = 'http://localhost:4567/api/v1')
    @base_url = base_url
    self.class.base_uri @base_url
    @headers = {
      'Content-Type' => 'application/json',
      'Accept' => 'application/json'
    }
  end
  
  def set_auth_token(token)
    @headers['Authorization'] = "Bearer #{token}"
  end
  
  def set_api_key(api_key)
    @headers['X-API-Key'] = api_key
  end
  
  def list_tasks(page_size: nil, page_token: nil, status: nil, 
                 assigned_to: nil, tags: nil, sort_order: nil)
    query = {}
    query[:page_size] = page_size if page_size
    query[:page_token] = page_token if page_token
    query[:status] = status if status
    query[:assigned_to] = assigned_to if assigned_to
    query[:tags] = tags if tags
    query[:sort_order] = sort_order if sort_order
    
    response = self.class.get('/tasks', 
      headers: @headers,
      query: query
    )
    
    handle_response(response)
  end
  
  def get_task(id)
    response = self.class.get("/tasks/#{id}", headers: @headers)
    handle_response(response)
  end
  
  def create_task(title:, description: nil, priority: nil, 
                  tags: nil, assigned_to: nil, due_date: nil)
    body = {
      title: title,
      description: description,
      priority: priority,
      tags: tags,
      assigned_to: assigned_to,
      due_date: due_date
    }.compact
    
    response = self.class.post('/tasks',
      headers: @headers,
      body: body.to_json
    )
    
    handle_response(response)
  end
  
  def update_task(id, **updates)
    response = self.class.put("/tasks/#{id}",
      headers: @headers,
      body: updates.to_json
    )
    
    handle_response(response)
  end
  
  def update_task_status(id, status)
    response = self.class.patch("/tasks/#{id}/status",
      headers: @headers,
      body: { status: status }.to_json
    )
    
    handle_response(response)
  end
  
  def delete_task(id)
    response = self.class.delete("/tasks/#{id}", headers: @headers)
    
    if response.code == 204
      true
    else
      handle_response(response)
    end
  end
  
  private
  
  def handle_response(response)
    case response.code
    when 200, 201
      response.parsed_response
    when 204
      nil
    when 400
      raise ValidationError, "Validation failed: #{response.parsed_response['error']['message']}"
    when 404
      raise NotFoundError, "Resource not found: #{response.parsed_response['error']['message']}"
    when 401
      raise AuthenticationError, "Authentication failed"
    when 403
      raise AuthorizationError, "Access denied"
    when 500
      raise ServerError, "Server error: #{response.parsed_response['error']['message']}"
    else
      raise APIError, "Unexpected response: #{response.code}"
    end
  end
  
  class APIError < StandardError; end
  class ValidationError < APIError; end
  class NotFoundError < APIError; end
  class AuthenticationError < APIError; end
  class AuthorizationError < APIError; end
  class ServerError < APIError; end
end
```

### Using the Client

```ruby
client = TaskAPIClient.new

# Create a task
task = client.create_task(
  title: 'Learn Ruby metaprogramming',
  description: 'Dive deep into Ruby metaprogramming techniques',
  priority: 'high',
  tags: ['ruby', 'learning', 'advanced']
)
puts "Created task: #{task['id']}"

# List tasks with filtering
result = client.list_tasks(
  status: 'pending',
  tags: 'ruby',
  sort_order: 'priority_desc',
  page_size: 10
)

result['tasks'].each do |task|
  puts "[#{task['status'].upcase}] #{task['title']}"
  puts "  Priority: #{task['priority']}"
  puts "  Tags: #{task['tags'].join(', ')}" if task['tags'].any?
end

# Update task
updated = client.update_task(task['id'],
  status: 'in_progress',
  assigned_to: 'ruby-developer'
)

# Delete task
client.delete_task(task['id'])
```

## gRPC in Ruby

Ruby's gRPC support provides a clean, idiomatic interface for building high-performance RPC services.

### Installing gRPC Dependencies

```bash
cd code/ruby/grpc
bundle install

# Generate Ruby code from proto files
bundle exec grpc_tools_ruby_protoc \
  -I../../shared/protos \
  --ruby_out=./lib \
  --grpc_out=./lib \
  ../../shared/protos/tasks.proto
```

### gRPC Server Implementation

```ruby
# server/task_service.rb
require 'grpc'
require 'securerandom'
require 'time'

# Require generated proto files
$LOAD_PATH.unshift(File.expand_path('../lib', __dir__))
require 'tasks_pb'
require 'tasks_services_pb'

module TaskServer
  class TaskServiceImpl < Tasks::V1::TaskService::Service
    def initialize
      @tasks = {}
      @mutex = Mutex.new
      initialize_sample_data
    end
    
    def list_tasks(request, _call)
      enum = Enumerator.new do |yielder|
        @mutex.synchronize do
          tasks = filter_and_sort_tasks(request)
          
          # Apply pagination
          page_size = request.page_size > 0 ? request.page_size : 20
          page_size = [page_size, 100].min
          
          start_index = 0
          if request.page_token && !request.page_token.empty?
            start_index = request.page_token.to_i
          end
          
          tasks[start_index, page_size].each do |task|
            yielder << task
          end
        end
      end
      
      enum
    end
    
    def get_task(request, _call)
      @mutex.synchronize do
        task = @tasks[request.id]
        raise GRPC::NotFound.new("Task with ID #{request.id} not found") unless task
        task
      end
    end
    
    def create_task(request, _call)
      task = request.task
      
      # Generate ID and set timestamps
      task.id = SecureRandom.uuid
      task.created_at = Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
      task.updated_at = Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
      
      # Set defaults
      task.status = Tasks::V1::TASK_STATUS_PENDING if task.status == Tasks::V1::TASK_STATUS_UNSPECIFIED
      task.priority = Tasks::V1::TASK_PRIORITY_MEDIUM if task.priority == Tasks::V1::TASK_PRIORITY_UNSPECIFIED
      task.created_by = 'system' if task.created_by.empty?
      
      # Validate
      raise GRPC::InvalidArgument.new("Title is required") if task.title.empty?
      raise GRPC::InvalidArgument.new("Title must be 200 characters or less") if task.title.length > 200
      
      @mutex.synchronize do
        @tasks[task.id] = task
      end
      
      task
    end
    
    def update_task(request, _call)
      @mutex.synchronize do
        existing_task = @tasks[request.task.id]
        raise GRPC::NotFound.new("Task with ID #{request.task.id} not found") unless existing_task
        
        # Apply updates based on update_mask
        if request.update_mask.empty?
          # Update all fields if no mask provided
          request.task.updated_at = Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
          @tasks[request.task.id] = request.task
        else
          # Selective update based on mask
          request.update_mask.each do |field|
            case field
            when 'title'
              existing_task.title = request.task.title
            when 'description'
              existing_task.description = request.task.description
            when 'status'
              existing_task.status = request.task.status
              if existing_task.status == Tasks::V1::TASK_STATUS_COMPLETED && !existing_task.completed_at
                existing_task.completed_at = Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
              end
            when 'priority'
              existing_task.priority = request.task.priority
            when 'tags'
              existing_task.tags.clear
              existing_task.tags.concat(request.task.tags)
            when 'assigned_to'
              existing_task.assigned_to = request.task.assigned_to
            when 'due_date'
              existing_task.due_date = request.task.due_date
            end
          end
          existing_task.updated_at = Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
        end
        
        @tasks[request.task.id]
      end
    end
    
    def delete_task(request, _call)
      @mutex.synchronize do
        task = @tasks.delete(request.id)
        raise GRPC::NotFound.new("Task with ID #{request.id} not found") unless task
        Google::Protobuf::Empty.new
      end
    end
    
    def watch_tasks(requests, _call)
      enum = Enumerator.new do |yielder|
        requests.each do |request|
          @mutex.synchronize do
            tasks_to_watch = if request.watch_all
              @tasks.values
            elsif !request.task_ids.empty?
              request.task_ids.map { |id| @tasks[id] }.compact
            elsif !request.assigned_to.empty?
              @tasks.values.select { |t| t.assigned_to == request.assigned_to }
            else
              []
            end
            
            tasks_to_watch.each do |task|
              event = Tasks::V1::TaskEvent.new(
                event_type: Tasks::V1::EVENT_TYPE_UPDATED,
                task: task,
                timestamp: Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
              )
              yielder << event
            end
          end
        end
      end
      
      enum
    end
    
    private
    
    def filter_and_sort_tasks(request)
      tasks = @tasks.values
      
      # Apply filters
      if request.status != Tasks::V1::TASK_STATUS_UNSPECIFIED
        tasks = tasks.select { |t| t.status == request.status }
      end
      
      unless request.assigned_to.empty?
        tasks = tasks.select { |t| t.assigned_to == request.assigned_to }
      end
      
      unless request.tags.empty?
        tasks = tasks.select do |t|
          (request.tags - t.tags).empty?
        end
      end
      
      # Sort tasks
      case request.sort_order
      when Tasks::V1::SORT_ORDER_CREATED_AT_ASC
        tasks.sort_by { |t| t.created_at.seconds }
      when Tasks::V1::SORT_ORDER_CREATED_AT_DESC
        tasks.sort_by { |t| -t.created_at.seconds }
      when Tasks::V1::SORT_ORDER_DUE_DATE_ASC
        tasks.sort_by { |t| t.due_date ? t.due_date.seconds : Float::INFINITY }
      when Tasks::V1::SORT_ORDER_DUE_DATE_DESC
        tasks.sort_by { |t| t.due_date ? -t.due_date.seconds : -Float::INFINITY }
      when Tasks::V1::SORT_ORDER_PRIORITY_ASC
        tasks.sort_by { |t| priority_value(t.priority) }
      when Tasks::V1::SORT_ORDER_PRIORITY_DESC
        tasks.sort_by { |t| -priority_value(t.priority) }
      else
        tasks
      end
    end
    
    def priority_value(priority)
      case priority
      when Tasks::V1::TASK_PRIORITY_LOW then 1
      when Tasks::V1::TASK_PRIORITY_MEDIUM then 2
      when Tasks::V1::TASK_PRIORITY_HIGH then 3
      when Tasks::V1::TASK_PRIORITY_CRITICAL then 4
      else 0
      end
    end
  end
  
  def self.start
    port = ENV['PORT'] || '50051'
    addr = "0.0.0.0:#{port}"
    
    server = GRPC::RpcServer.new
    server.add_http2_port(addr, :this_port_is_insecure)
    server.handle(TaskServiceImpl.new)
    
    puts "Ruby gRPC server listening on #{addr}"
    
    # Run the server
    server.run_till_terminated_or_interrupted([1, 'int', 'TERM'])
  end
end

# Start server if run directly
if __FILE__ == $0
  TaskServer.start
end
```

### gRPC Client Implementation

```ruby
# client/task_client.rb
require 'grpc'
require 'securerandom'

# Require generated proto files
$LOAD_PATH.unshift(File.expand_path('../lib', __dir__))
require 'tasks_pb'
require 'tasks_services_pb'

class TaskGrpcClient
  def initialize(host = 'localhost:50051')
    @stub = Tasks::V1::TaskService::Stub.new(host, :this_channel_is_insecure)
  end
  
  def list_tasks(page_size: 20, page_token: nil, status: nil, 
                 assigned_to: nil, tags: [], sort_order: nil)
    request = Tasks::V1::ListTasksRequest.new(
      page_size: page_size
    )
    
    request.page_token = page_token if page_token
    request.status = status if status
    request.assigned_to = assigned_to if assigned_to
    request.tags.concat(tags) unless tags.empty?
    request.sort_order = sort_order if sort_order
    
    tasks = []
    @stub.list_tasks(request).each do |task|
      tasks << task
    end
    tasks
  end
  
  def get_task(id)
    request = Tasks::V1::GetTaskRequest.new(id: id)
    @stub.get_task(request)
  end
  
  def create_task(title:, description: '', priority: nil, 
                  tags: [], assigned_to: nil, due_date: nil)
    task = Tasks::V1::Task.new(
      title: title,
      description: description,
      tags: tags
    )
    
    task.priority = priority if priority
    task.assigned_to = assigned_to if assigned_to
    task.due_date = due_date if due_date
    
    request = Tasks::V1::CreateTaskRequest.new(task: task)
    @stub.create_task(request)
  end
  
  def update_task(id, **updates)
    task = Tasks::V1::Task.new(id: id)
    update_mask = []
    
    if updates[:title]
      task.title = updates[:title]
      update_mask << 'title'
    end
    
    if updates[:description]
      task.description = updates[:description]
      update_mask << 'description'
    end
    
    if updates[:status]
      task.status = updates[:status]
      update_mask << 'status'
    end
    
    if updates[:priority]
      task.priority = updates[:priority]
      update_mask << 'priority'
    end
    
    if updates[:tags]
      task.tags.concat(updates[:tags])
      update_mask << 'tags'
    end
    
    request = Tasks::V1::UpdateTaskRequest.new(
      task: task,
      update_mask: update_mask
    )
    
    @stub.update_task(request)
  end
  
  def delete_task(id)
    request = Tasks::V1::DeleteTaskRequest.new(id: id)
    @stub.delete_task(request)
  end
  
  def watch_tasks(task_ids: [], watch_all: false, assigned_to: nil, &block)
    requests = []
    request = Tasks::V1::WatchTasksRequest.new
    
    if watch_all
      request.watch_all = true
    elsif !task_ids.empty?
      request.task_ids.concat(task_ids)
    elsif assigned_to
      request.assigned_to = assigned_to
    end
    
    requests << request
    
    enum = Enumerator.new do |yielder|
      requests.each { |req| yielder << req }
    end
    
    @stub.watch_tasks(enum).each do |event|
      if block_given?
        yield event
      else
        print_event(event)
      end
    end
  end
end
```

## Testing in Ruby

Ruby's testing culture is one of its greatest strengths. Let's explore testing our APIs with RSpec:

### RSpec for REST API Testing

```ruby
# spec/app_spec.rb
require 'spec_helper'
require 'rack/test'
require_relative '../app'

RSpec.describe TaskAPI do
  include Rack::Test::Methods
  
  def app
    TaskAPI
  end
  
  describe 'GET /health' do
    it 'returns health status' do
      get '/health'
      
      expect(last_response.status).to eq(200)
      
      json = JSON.parse(last_response.body)
      expect(json['status']).to eq('healthy')
      expect(json['service']).to eq('task-api-ruby')
    end
  end
  
  describe 'POST /api/v1/tasks' do
    it 'creates a new task' do
      task_data = {
        title: 'Test Task',
        description: 'A test task',
        priority: 'high'
      }
      
      post '/api/v1/tasks', task_data.to_json, 
           'CONTENT_TYPE' => 'application/json'
      
      expect(last_response.status).to eq(201)
      expect(last_response.headers['Location']).to match(%r{/api/v1/tasks/[\w-]+})
      
      json = JSON.parse(last_response.body)
      expect(json['title']).to eq('Test Task')
      expect(json['status']).to eq('pending')
      expect(json['priority']).to eq('high')
    end
    
    it 'returns validation error for missing title' do
      post '/api/v1/tasks', {}.to_json,
           'CONTENT_TYPE' => 'application/json'
      
      expect(last_response.status).to eq(400)
      
      json = JSON.parse(last_response.body)
      expect(json['error']['code']).to eq('VALIDATION_ERROR')
    end
  end
  
  describe 'GET /api/v1/tasks' do
    before do
      # Create some test tasks
      3.times do |i|
        post '/api/v1/tasks', 
             { title: "Task #{i}", priority: %w[low medium high][i] }.to_json,
             'CONTENT_TYPE' => 'application/json'
      end
    end
    
    it 'lists tasks with pagination' do
      get '/api/v1/tasks', page_size: 2
      
      expect(last_response.status).to eq(200)
      
      json = JSON.parse(last_response.body)
      expect(json['tasks'].length).to eq(2)
      expect(json['next_page_token']).not_to be_nil
      expect(json['total_count']).to be >= 3
    end
    
    it 'filters tasks by status' do
      get '/api/v1/tasks', status: 'pending'
      
      expect(last_response.status).to eq(200)
      
      json = JSON.parse(last_response.body)
      json['tasks'].each do |task|
        expect(task['status']).to eq('pending')
      end
    end
  end
end
```

### Testing gRPC Services

```ruby
# spec/grpc_spec.rb
require 'spec_helper'
require_relative '../server/task_service'

RSpec.describe TaskServer::TaskServiceImpl do
  let(:service) { described_class.new }
  
  describe '#create_task' do
    it 'creates a task with generated ID' do
      task = Tasks::V1::Task.new(
        title: 'Test Task',
        description: 'Testing gRPC',
        priority: Tasks::V1::TASK_PRIORITY_HIGH
      )
      
      request = Tasks::V1::CreateTaskRequest.new(task: task)
      result = service.create_task(request, nil)
      
      expect(result.id).not_to be_empty
      expect(result.title).to eq('Test Task')
      expect(result.status).to eq(Tasks::V1::TASK_STATUS_PENDING)
      expect(result.created_at).not_to be_nil
    end
    
    it 'raises InvalidArgument for missing title' do
      task = Tasks::V1::Task.new(description: 'No title')
      request = Tasks::V1::CreateTaskRequest.new(task: task)
      
      expect {
        service.create_task(request, nil)
      }.to raise_error(GRPC::InvalidArgument, /Title is required/)
    end
  end
  
  describe '#list_tasks' do
    before do
      # Create test tasks
      3.times do |i|
        task = Tasks::V1::Task.new(
          title: "Task #{i}",
          priority: [
            Tasks::V1::TASK_PRIORITY_LOW,
            Tasks::V1::TASK_PRIORITY_MEDIUM,
            Tasks::V1::TASK_PRIORITY_HIGH
          ][i]
        )
        request = Tasks::V1::CreateTaskRequest.new(task: task)
        service.create_task(request, nil)
      end
    end
    
    it 'returns tasks as a stream' do
      request = Tasks::V1::ListTasksRequest.new(page_size: 10)
      
      tasks = []
      service.list_tasks(request, nil).each do |task|
        tasks << task
      end
      
      expect(tasks.length).to be >= 3
      tasks.each do |task|
        expect(task).to be_a(Tasks::V1::Task)
      end
    end
    
    it 'filters tasks by status' do
      request = Tasks::V1::ListTasksRequest.new(
        status: Tasks::V1::TASK_STATUS_PENDING
      )
      
      tasks = service.list_tasks(request, nil).to_a
      
      tasks.each do |task|
        expect(task.status).to eq(Tasks::V1::TASK_STATUS_PENDING)
      end
    end
  end
end
```

## Performance Optimization

Ruby offers several strategies for optimizing API performance:

### Connection Pooling

```ruby
# Using connection_pool gem for database connections
require 'connection_pool'

class TaskService
  def initialize
    @pool = ConnectionPool.new(size: 5, timeout: 5) do
      # Create database connection
      PG.connect(dbname: 'tasks')
    end
  end
  
  def get_task(id)
    @pool.with do |conn|
      result = conn.exec_params('SELECT * FROM tasks WHERE id = $1', [id])
      result.first
    end
  end
end
```

### Caching with Redis

```ruby
require 'redis'
require 'json'

class CachedTaskService
  def initialize(task_service)
    @task_service = task_service
    @redis = Redis.new
  end
  
  def get_task(id)
    cache_key = "task:#{id}"
    
    # Try to get from cache
    cached = @redis.get(cache_key)
    return JSON.parse(cached, symbolize_names: true) if cached
    
    # Get from service and cache
    task = @task_service.get_task(id)
    @redis.setex(cache_key, 300, task.to_json) # Cache for 5 minutes
    
    task
  end
  
  def invalidate_task(id)
    @redis.del("task:#{id}")
  end
end
```

### Background Job Processing

```ruby
# Using Sidekiq for background jobs
require 'sidekiq'

class TaskNotificationWorker
  include Sidekiq::Worker
  
  def perform(task_id, event_type)
    task = TaskService.new.get_task(task_id)
    
    case event_type
    when 'created'
      send_creation_notification(task)
    when 'completed'
      send_completion_notification(task)
    end
  end
  
  private
  
  def send_creation_notification(task)
    # Send email, push notification, etc.
    EmailService.send_task_created(task)
  end
  
  def send_completion_notification(task)
    EmailService.send_task_completed(task)
  end
end

# In your API
class TaskAPI < Sinatra::Base
  post '/api/v1/tasks' do
    task = @task_service.create_task(params)
    
    # Queue background job
    TaskNotificationWorker.perform_async(task[:id], 'created')
    
    status 201
    json(task)
  end
end
```

## Production Deployment

### Using Puma for Production

```ruby
# config/puma.rb
workers Integer(ENV['WEB_CONCURRENCY'] || 2)
threads_count = Integer(ENV['MAX_THREADS'] || 5)
threads threads_count, threads_count

preload_app!

rackup      DefaultRackup
port        ENV['PORT']     || 3000
environment ENV['RACK_ENV'] || 'development'

on_worker_boot do
  # Worker specific setup for Rails 4.1+
  # See: https://devcenter.heroku.com/articles/deploying-rails-applications-with-the-puma-web-server#on-worker-boot
end
```

### Docker Deployment

```dockerfile
# Dockerfile
FROM ruby:3.3.0-slim

RUN apt-get update -qq && apt-get install -y \
  build-essential \
  libpq-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY Gemfile Gemfile.lock ./
RUN bundle install --without development test

COPY . .

EXPOSE 3000

CMD ["bundle", "exec", "puma", "-C", "config/puma.rb"]
```

### Monitoring and Metrics

```ruby
# Using prometheus-client for metrics
require 'prometheus/client'
require 'prometheus/client/push'

class MetricsMiddleware
  def initialize(app)
    @app = app
    @registry = Prometheus::Client.registry
    
    @request_counter = Prometheus::Client::Counter.new(
      :http_requests_total,
      docstring: 'Total number of HTTP requests',
      labels: [:method, :path, :status]
    )
    
    @request_duration = Prometheus::Client::Histogram.new(
      :http_request_duration_seconds,
      docstring: 'HTTP request duration',
      labels: [:method, :path]
    )
    
    @registry.register(@request_counter)
    @registry.register(@request_duration)
  end
  
  def call(env)
    start_time = Time.now
    
    status, headers, body = @app.call(env)
    
    duration = Time.now - start_time
    
    @request_counter.increment(
      labels: {
        method: env['REQUEST_METHOD'],
        path: env['PATH_INFO'],
        status: status
      }
    )
    
    @request_duration.observe(
      duration,
      labels: {
        method: env['REQUEST_METHOD'],
        path: env['PATH_INFO']
      }
    )
    
    [status, headers, body]
  end
end

# In your app
use MetricsMiddleware
```

## Ruby-Specific Best Practices

### 1. Use Semantic Versioning for APIs

```ruby
class TaskAPI < Sinatra::Base
  API_VERSION = '1.0.0'
  
  before do
    headers['X-API-Version'] = API_VERSION
  end
  
  # Support version in URL
  namespace '/api/v1' do
    # v1 endpoints
  end
  
  namespace '/api/v2' do
    # v2 endpoints with breaking changes
  end
end
```

### 2. Implement Rate Limiting

```ruby
require 'redis'

class RateLimiter
  def initialize(app, options = {})
    @app = app
    @redis = Redis.new
    @limit = options[:limit] || 100
    @window = options[:window] || 3600 # 1 hour
  end
  
  def call(env)
    client_id = identify_client(env)
    key = "rate_limit:#{client_id}"
    
    count = @redis.incr(key)
    @redis.expire(key, @window) if count == 1
    
    if count > @limit
      return [
        429,
        {
          'Content-Type' => 'application/json',
          'X-RateLimit-Limit' => @limit.to_s,
          'X-RateLimit-Remaining' => '0',
          'X-RateLimit-Reset' => (@redis.ttl(key) + Time.now.to_i).to_s
        },
        [{ error: 'Rate limit exceeded' }.to_json]
      ]
    end
    
    status, headers, body = @app.call(env)
    
    headers['X-RateLimit-Limit'] = @limit.to_s
    headers['X-RateLimit-Remaining'] = (@limit - count).to_s
    headers['X-RateLimit-Reset'] = (@redis.ttl(key) + Time.now.to_i).to_s
    
    [status, headers, body]
  end
  
  private
  
  def identify_client(env)
    # Use API key, auth token, or IP address
    env['HTTP_X_API_KEY'] || 
    env['HTTP_AUTHORIZATION'] || 
    env['REMOTE_ADDR']
  end
end

# Use in your app
use RateLimiter, limit: 100, window: 3600
```

### 3. Implement Idempotency

```ruby
class IdempotencyMiddleware
  def initialize(app)
    @app = app
    @redis = Redis.new
  end
  
  def call(env)
    if env['REQUEST_METHOD'] == 'POST' && env['HTTP_IDEMPOTENCY_KEY']
      key = "idempotency:#{env['HTTP_IDEMPOTENCY_KEY']}"
      
      # Check if we've seen this request before
      cached = @redis.get(key)
      if cached
        data = JSON.parse(cached)
        return [data['status'], data['headers'], [data['body']]]
      end
      
      # Process request and cache response
      status, headers, body = @app.call(env)
      body_str = body.respond_to?(:join) ? body.join : body.to_s
      
      @redis.setex(key, 86400, { # Cache for 24 hours
        status: status,
        headers: headers,
        body: body_str
      }.to_json)
      
      [status, headers, [body_str]]
    else
      @app.call(env)
    end
  end
end
```

## Conclusion

Ruby demonstrates that API development doesn't have to sacrifice elegance for functionality. Through this chapter, we've seen how Ruby's expressive syntax, combined with powerful frameworks like Sinatra and robust gRPC support, creates a development experience that prioritizes both developer happiness and production reliability.

The language's philosophy of "optimizing for programmer happiness" shines through in API development. From the clean DSL of Sinatra to the intuitive testing with RSpec, Ruby makes it enjoyable to build, test, and maintain APIs. The strong emphasis on convention over configuration means you can focus on your business logic rather than boilerplate code.

Key takeaways from Ruby API development:

1. **Sinatra's Simplicity**: For microservices and lightweight APIs, Sinatra provides just enough structure without the overhead of larger frameworks

2. **Testing Culture**: Ruby's testing ecosystem, particularly RSpec, makes it natural to write comprehensive tests for your APIs

3. **Metaprogramming Power**: Ruby's dynamic nature allows for powerful abstractions that can significantly reduce code duplication

4. **Performance Considerations**: While Ruby may not be the fastest language, proper use of caching, background jobs, and connection pooling can create highly performant APIs

5. **Developer Experience**: Ruby's focus on readability and expressiveness makes APIs easier to understand and maintain over time

As we continue our journey through different languages, Ruby stands as an example of how a language designed for developer happiness can also deliver robust, production-ready API solutions. Whether you're building a simple REST API or implementing complex gRPC services, Ruby provides the tools and patterns to do so elegantly and efficiently.