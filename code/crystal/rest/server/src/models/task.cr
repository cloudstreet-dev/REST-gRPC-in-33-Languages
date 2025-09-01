require "json"
require "uuid"

module Models
  enum TaskStatus
    Pending
    InProgress
    Completed
    Cancelled
    OnHold

    def to_s : String
      case self
      when Pending    then "pending"
      when InProgress then "in_progress"
      when Completed  then "completed"
      when Cancelled  then "cancelled"
      when OnHold     then "on_hold"
      else "pending"
      end
    end

    def self.from_string(value : String) : TaskStatus
      case value.downcase
      when "pending"     then Pending
      when "in_progress" then InProgress
      when "completed"   then Completed
      when "cancelled"   then Cancelled
      when "on_hold"     then OnHold
      else Pending
      end
    end
  end

  enum TaskPriority
    Low
    Medium
    High
    Critical

    def to_s : String
      case self
      when Low      then "low"
      when Medium   then "medium"
      when High     then "high"
      when Critical then "critical"
      else "medium"
      end
    end

    def self.from_string(value : String) : TaskPriority
      case value.downcase
      when "low"      then Low
      when "medium"   then Medium
      when "high"     then High
      when "critical" then Critical
      else Medium
      end
    end

    def value : Int32
      case self
      when Low      then 1
      when Medium   then 2
      when High     then 3
      when Critical then 4
      else 2
      end
    end
  end

  class Task
    include JSON::Serializable

    property id : String
    property title : String
    property description : String
    property status : String
    property priority : String
    property tags : Array(String)
    property created_by : String
    property assigned_to : String?
    property created_at : String
    property updated_at : String
    property due_date : String?
    property completed_at : String?

    def initialize(
      @title : String,
      @description : String = "",
      status : TaskStatus = TaskStatus::Pending,
      priority : TaskPriority = TaskPriority::Medium,
      @tags : Array(String) = [] of String,
      @created_by : String = "system",
      @assigned_to : String? = nil,
      @due_date : String? = nil
    )
      @id = UUID.random.to_s
      @status = status.to_s
      @priority = priority.to_s
      @created_at = Time.utc.to_rfc3339
      @updated_at = Time.utc.to_rfc3339
      @completed_at = nil
    end

    def self.from_json(json : String) : Task
      parsed = JSON.parse(json)
      task = Task.new(
        title: parsed["title"].as_s,
        description: parsed["description"]?.try(&.as_s) || "",
        status: TaskStatus.from_string(parsed["status"]?.try(&.as_s) || "pending"),
        priority: TaskPriority.from_string(parsed["priority"]?.try(&.as_s) || "medium"),
        tags: (parsed["tags"]?.try(&.as_a?) || [] of JSON::Any).map(&.as_s),
        created_by: parsed["created_by"]?.try(&.as_s) || "system",
        assigned_to: parsed["assigned_to"]?.try(&.as_s?),
        due_date: parsed["due_date"]?.try(&.as_s?)
      )
      
      # Override generated fields if provided
      if id = parsed["id"]?.try(&.as_s?)
        task.id = id
      end
      
      task
    end

    def status_enum : TaskStatus
      TaskStatus.from_string(@status)
    end

    def priority_enum : TaskPriority
      TaskPriority.from_string(@priority)
    end

    def valid? : Bool
      return false if @title.empty?
      return false if @title.size > 200
      return false unless valid_status?
      return false unless valid_priority?
      true
    end

    def validation_errors : Array(String)
      errors = [] of String
      errors << "Title is required" if @title.empty?
      errors << "Title must be 200 characters or less" if @title.size > 200
      errors << "Invalid status: #{@status}" unless valid_status?
      errors << "Invalid priority: #{@priority}" unless valid_priority?
      errors
    end

    def update!(
      title : String? = nil,
      description : String? = nil,
      status : String? = nil,
      priority : String? = nil,
      tags : Array(String)? = nil,
      assigned_to : String? = nil,
      due_date : String? = nil
    )
      @title = title if title
      @description = description if description
      
      if status
        @status = status
        if status == "completed" && @completed_at.nil?
          @completed_at = Time.utc.to_rfc3339
        end
      end
      
      @priority = priority if priority
      @tags = tags if tags
      @assigned_to = assigned_to unless assigned_to.nil?
      @due_date = due_date unless due_date.nil?
      
      @updated_at = Time.utc.to_rfc3339
    end

    def matches_filters?(
      status : String? = nil,
      assigned_to : String? = nil,
      tags : String? = nil
    ) : Bool
      return false if status && @status != status
      return false if assigned_to && @assigned_to != assigned_to
      
      if tags && !tags.empty?
        tag_list = tags.split(',').map(&.strip)
        return false unless (tag_list - @tags).empty?
      end
      
      true
    end

    private def valid_status? : Bool
      ["pending", "in_progress", "completed", "cancelled", "on_hold"].includes?(@status)
    end

    private def valid_priority? : Bool
      ["low", "medium", "high", "critical"].includes?(@priority)
    end
  end

  class CreateTaskRequest
    include JSON::Serializable

    property title : String
    property description : String?
    property priority : String?
    property tags : Array(String)?
    property assigned_to : String?
    property due_date : String?
  end

  class UpdateTaskRequest
    include JSON::Serializable

    property title : String?
    property description : String?
    property status : String?
    property priority : String?
    property tags : Array(String)?
    property assigned_to : String?
    property due_date : String?
  end

  class UpdateTaskStatusRequest
    include JSON::Serializable

    property status : String
  end

  class ListTasksResponse
    include JSON::Serializable

    property tasks : Array(Task)
    property next_page_token : String?
    property total_count : Int32

    def initialize(@tasks : Array(Task), @next_page_token : String?, @total_count : Int32)
    end
  end

  class ErrorResponse
    include JSON::Serializable

    property error : ErrorDetail

    def initialize(code : String, message : String)
      @error = ErrorDetail.new(code, message)
    end
  end

  class ErrorDetail
    include JSON::Serializable

    property code : String
    property message : String

    def initialize(@code : String, @message : String)
    end
  end
end