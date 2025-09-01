require_relative '../models/task'

module Services
  class TaskService
    def initialize
      @tasks = {}
      @mutex = Mutex.new
      initialize_sample_data
    end
    
    def list_tasks(page_size: 20, page_token: nil, status: nil, assigned_to: nil, tags: nil, sort_order: nil)
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
    
    def update_task_status(id, status)
      update_task(id, { status: status })
    end
    
    def delete_task(id)
      @mutex.synchronize do
        task = @tasks.delete(id)
        raise NotFoundError, "Task with ID #{id} not found" unless task
        nil
      end
    end
    
    private
    
    def initialize_sample_data
      sample_tasks = [
        {
          title: 'Implement Ruby REST API',
          description: 'Create REST API using Sinatra framework',
          priority: 'high',
          tags: ['ruby', 'rest', 'api']
        },
        {
          title: 'Add gRPC support',
          description: 'Implement gRPC server with grpc gem',
          priority: 'medium',
          tags: ['ruby', 'grpc', 'protobuf']
        },
        {
          title: 'Write tests',
          description: 'Add RSpec tests for API endpoints',
          priority: 'high',
          tags: ['ruby', 'testing', 'rspec']
        }
      ]
      
      sample_tasks.each { |attrs| create_task(attrs) }
    end
    
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