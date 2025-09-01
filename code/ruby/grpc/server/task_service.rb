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
      # Simple implementation: yield existing tasks matching the filter
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
    
    def initialize_sample_data
      sample_tasks = [
        {
          title: 'Implement Ruby gRPC API',
          description: 'Create gRPC API using native Ruby support',
          priority: Tasks::V1::TASK_PRIORITY_HIGH,
          tags: ['ruby', 'grpc', 'api']
        },
        {
          title: 'Add streaming support',
          description: 'Implement bidirectional streaming for real-time updates',
          priority: Tasks::V1::TASK_PRIORITY_MEDIUM,
          tags: ['ruby', 'grpc', 'streaming']
        }
      ]
      
      sample_tasks.each do |attrs|
        task = Tasks::V1::Task.new(
          id: SecureRandom.uuid,
          title: attrs[:title],
          description: attrs[:description],
          status: Tasks::V1::TASK_STATUS_PENDING,
          priority: attrs[:priority],
          tags: attrs[:tags],
          created_by: 'system',
          created_at: Google::Protobuf::Timestamp.new(seconds: Time.now.to_i),
          updated_at: Google::Protobuf::Timestamp.new(seconds: Time.now.to_i)
        )
        @tasks[task.id] = task
      end
    end
    
    def filter_and_sort_tasks(request)
      tasks = @tasks.values
      
      # Filter by status
      if request.status != Tasks::V1::TASK_STATUS_UNSPECIFIED
        tasks = tasks.select { |t| t.status == request.status }
      end
      
      # Filter by assigned_to
      unless request.assigned_to.empty?
        tasks = tasks.select { |t| t.assigned_to == request.assigned_to }
      end
      
      # Filter by tags
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