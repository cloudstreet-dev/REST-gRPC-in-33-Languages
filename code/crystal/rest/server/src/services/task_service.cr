require "../models/task"

module Services
  class TaskService
    @tasks : Hash(String, Models::Task)
    @mutex : Mutex

    def initialize
      @tasks = {} of String => Models::Task
      @mutex = Mutex.new
      initialize_sample_data
    end

    def list_tasks(
      page_size : Int32 = 20,
      page_token : String? = nil,
      status : String? = nil,
      assigned_to : String? = nil,
      tags : String? = nil,
      sort_order : String? = nil
    ) : Models::ListTasksResponse
      @mutex.synchronize do
        # Filter tasks
        filtered_tasks = @tasks.values.select do |task|
          task.matches_filters?(status: status, assigned_to: assigned_to, tags: tags)
        end

        # Sort tasks
        sorted_tasks = sort_tasks(filtered_tasks, sort_order)

        # Paginate
        page_size = Math.min(Math.max(page_size, 1), 100)
        start_index = page_token ? page_token.to_i : 0
        end_index = start_index + page_size

        tasks_page = sorted_tasks[start_index...end_index]? || [] of Models::Task

        next_page_token = end_index < sorted_tasks.size ? end_index.to_s : nil

        Models::ListTasksResponse.new(
          tasks: tasks_page,
          next_page_token: next_page_token,
          total_count: sorted_tasks.size
        )
      end
    end

    def get_task(id : String) : Models::Task
      @mutex.synchronize do
        task = @tasks[id]?
        raise NotFoundError.new("Task with ID #{id} not found") unless task
        task
      end
    end

    def create_task(
      title : String,
      description : String? = nil,
      priority : String? = nil,
      tags : Array(String)? = nil,
      assigned_to : String? = nil,
      due_date : String? = nil
    ) : Models::Task
      task = Models::Task.new(
        title: title,
        description: description || "",
        priority: priority ? Models::TaskPriority.from_string(priority) : Models::TaskPriority::Medium,
        tags: tags || [] of String,
        assigned_to: assigned_to,
        due_date: due_date
      )

      unless task.valid?
        raise ValidationError.new(task.validation_errors.join(", "))
      end

      @mutex.synchronize do
        @tasks[task.id] = task
      end

      task
    end

    def update_task(id : String, updates : Models::UpdateTaskRequest) : Models::Task
      @mutex.synchronize do
        task = @tasks[id]?
        raise NotFoundError.new("Task with ID #{id} not found") unless task

        task.update!(
          title: updates.title,
          description: updates.description,
          status: updates.status,
          priority: updates.priority,
          tags: updates.tags,
          assigned_to: updates.assigned_to,
          due_date: updates.due_date
        )

        unless task.valid?
          raise ValidationError.new(task.validation_errors.join(", "))
        end

        task
      end
    end

    def update_task_status(id : String, status : String) : Models::Task
      @mutex.synchronize do
        task = @tasks[id]?
        raise NotFoundError.new("Task with ID #{id} not found") unless task

        task.update!(status: status)

        unless task.valid?
          raise ValidationError.new(task.validation_errors.join(", "))
        end

        task
      end
    end

    def delete_task(id : String) : Nil
      @mutex.synchronize do
        task = @tasks.delete(id)
        raise NotFoundError.new("Task with ID #{id} not found") unless task
        nil
      end
    end

    private def initialize_sample_data
      sample_tasks = [
        {
          title: "Implement Crystal REST API",
          description: "Create REST API using Kemal framework",
          priority: "high",
          tags: ["crystal", "rest", "api"],
        },
        {
          title: "Add gRPC support",
          description: "Implement gRPC server with Crystal",
          priority: "medium",
          tags: ["crystal", "grpc", "protobuf"],
        },
        {
          title: "Write tests",
          description: "Add spec tests for API endpoints",
          priority: "high",
          tags: ["crystal", "testing", "spec"],
        },
      ]

      sample_tasks.each do |attrs|
        create_task(
          title: attrs[:title],
          description: attrs[:description],
          priority: attrs[:priority],
          tags: attrs[:tags]
        )
      end
    end

    private def sort_tasks(tasks : Array(Models::Task), sort_order : String?) : Array(Models::Task)
      case sort_order
      when "created_at_asc"
        tasks.sort_by { |t| t.created_at }
      when "created_at_desc"
        tasks.sort_by { |t| t.created_at }.reverse
      when "due_date_asc"
        tasks.sort_by { |t| t.due_date || "9999-12-31" }
      when "due_date_desc"
        tasks.sort_by { |t| t.due_date || "0000-01-01" }.reverse
      when "priority_asc"
        tasks.sort_by { |t| t.priority_enum.value }
      when "priority_desc"
        tasks.sort_by { |t| t.priority_enum.value }.reverse
      else
        tasks
      end
    end
  end

  class ValidationError < Exception; end
  class NotFoundError < Exception; end
end