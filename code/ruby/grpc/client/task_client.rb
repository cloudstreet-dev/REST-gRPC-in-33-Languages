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
  
  def list_tasks(page_size: 20, page_token: nil, status: nil, assigned_to: nil, tags: [], sort_order: nil)
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
  
  def create_task(title:, description: '', priority: nil, tags: [], assigned_to: nil, due_date: nil)
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
    
    if updates[:assigned_to]
      task.assigned_to = updates[:assigned_to]
      update_mask << 'assigned_to'
    end
    
    if updates[:due_date]
      task.due_date = updates[:due_date]
      update_mask << 'due_date'
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
  
  private
  
  def print_event(event)
    type = case event.event_type
           when Tasks::V1::EVENT_TYPE_CREATED then 'CREATED'
           when Tasks::V1::EVENT_TYPE_UPDATED then 'UPDATED'
           when Tasks::V1::EVENT_TYPE_DELETED then 'DELETED'
           when Tasks::V1::EVENT_TYPE_STATUS_CHANGED then 'STATUS_CHANGED'
           else 'UNKNOWN'
           end
    
    puts "[#{type}] Task #{event.task.id}: #{event.task.title}"
  end
  
  def print_task(task)
    status = status_string(task.status)
    priority = priority_string(task.priority)
    
    puts "[#{status}] #{task.title} (Priority: #{priority})"
    puts "  ID: #{task.id}"
    puts "  Description: #{task.description}" unless task.description.empty?
    puts "  Tags: #{task.tags.join(', ')}" unless task.tags.empty?
    puts "  Assigned to: #{task.assigned_to}" unless task.assigned_to.empty?
  end
  
  def status_string(status)
    case status
    when Tasks::V1::TASK_STATUS_PENDING then 'PENDING'
    when Tasks::V1::TASK_STATUS_IN_PROGRESS then 'IN_PROGRESS'
    when Tasks::V1::TASK_STATUS_COMPLETED then 'COMPLETED'
    when Tasks::V1::TASK_STATUS_CANCELLED then 'CANCELLED'
    when Tasks::V1::TASK_STATUS_ON_HOLD then 'ON_HOLD'
    else 'UNSPECIFIED'
    end
  end
  
  def priority_string(priority)
    case priority
    when Tasks::V1::TASK_PRIORITY_LOW then 'LOW'
    when Tasks::V1::TASK_PRIORITY_MEDIUM then 'MEDIUM'
    when Tasks::V1::TASK_PRIORITY_HIGH then 'HIGH'
    when Tasks::V1::TASK_PRIORITY_CRITICAL then 'CRITICAL'
    else 'UNSPECIFIED'
    end
  end
end

# Example usage
if __FILE__ == $0
  client = TaskGrpcClient.new
  
  begin
    # Create a task
    puts "Creating task..."
    task = client.create_task(
      title: 'Test Ruby gRPC Client',
      description: 'Testing the Ruby gRPC client',
      priority: Tasks::V1::TASK_PRIORITY_HIGH,
      tags: ['test', 'ruby', 'grpc']
    )
    puts "Created task: #{task.id}"
    
    # List tasks
    puts "\nListing tasks..."
    tasks = client.list_tasks(page_size: 10)
    tasks.each do |t|
      client.send(:print_task, t)
      puts
    end
    
    # Update task
    puts "Updating task..."
    updated = client.update_task(
      task.id,
      status: Tasks::V1::TASK_STATUS_IN_PROGRESS
    )
    puts "Task updated: #{updated.title} - Status: #{client.send(:status_string, updated.status)}"
    
    # Get single task
    puts "\nFetching task..."
    fetched = client.get_task(task.id)
    client.send(:print_task, fetched)
    
    # Delete task
    puts "\nDeleting task..."
    client.delete_task(task.id)
    puts "Task deleted successfully"
    
  rescue GRPC::BadStatus => e
    puts "gRPC Error: #{e.message}"
  rescue => e
    puts "Error: #{e.message}"
    puts e.backtrace
  end
end