require "crest"
require "json"

class TaskAPIClient
  @base_url : String
  @headers : Hash(String, String)

  def initialize(@base_url : String = "http://localhost:3000/api/v1")
    @headers = {
      "Content-Type" => "application/json",
      "Accept" => "application/json"
    }
  end

  def set_auth_token(token : String)
    @headers["Authorization"] = "Bearer #{token}"
  end

  def set_api_key(api_key : String)
    @headers["X-API-Key"] = api_key
  end

  def list_tasks(
    page_size : Int32? = nil,
    page_token : String? = nil,
    status : String? = nil,
    assigned_to : String? = nil,
    tags : String? = nil,
    sort_order : String? = nil
  ) : JSON::Any
    params = {} of String => String
    params["page_size"] = page_size.to_s if page_size
    params["page_token"] = page_token if page_token
    params["status"] = status if status
    params["assigned_to"] = assigned_to if assigned_to
    params["tags"] = tags if tags
    params["sort_order"] = sort_order if sort_order

    response = Crest.get(
      "#{@base_url}/tasks",
      headers: @headers,
      params: params
    )
    
    handle_response(response)
  end

  def get_task(id : String) : JSON::Any
    response = Crest.get(
      "#{@base_url}/tasks/#{id}",
      headers: @headers
    )
    
    handle_response(response)
  end

  def create_task(
    title : String,
    description : String? = nil,
    priority : String? = nil,
    tags : Array(String)? = nil,
    assigned_to : String? = nil,
    due_date : String? = nil
  ) : JSON::Any
    body = {
      "title" => title,
      "description" => description,
      "priority" => priority,
      "tags" => tags,
      "assigned_to" => assigned_to,
      "due_date" => due_date
    }.compact

    response = Crest.post(
      "#{@base_url}/tasks",
      headers: @headers,
      json: body
    )
    
    handle_response(response)
  end

  def update_task(
    id : String,
    title : String? = nil,
    description : String? = nil,
    status : String? = nil,
    priority : String? = nil,
    tags : Array(String)? = nil,
    assigned_to : String? = nil,
    due_date : String? = nil
  ) : JSON::Any
    body = {
      "title" => title,
      "description" => description,
      "status" => status,
      "priority" => priority,
      "tags" => tags,
      "assigned_to" => assigned_to,
      "due_date" => due_date
    }.compact

    response = Crest.put(
      "#{@base_url}/tasks/#{id}",
      headers: @headers,
      json: body
    )
    
    handle_response(response)
  end

  def update_task_status(id : String, status : String) : JSON::Any
    response = Crest.patch(
      "#{@base_url}/tasks/#{id}/status",
      headers: @headers,
      json: {"status" => status}
    )
    
    handle_response(response)
  end

  def delete_task(id : String) : Bool
    response = Crest.delete(
      "#{@base_url}/tasks/#{id}",
      headers: @headers
    )
    
    response.status_code == 204
  rescue ex : Crest::NotFound
    raise NotFoundError.new("Task with ID #{id} not found")
  rescue ex : Crest::RequestFailed
    raise APIError.new("Failed to delete task: #{ex.message}")
  end

  private def handle_response(response : Crest::Response) : JSON::Any
    case response.status_code
    when 200, 201
      JSON.parse(response.body)
    when 204
      JSON::Any.new(nil)
    else
      error_body = JSON.parse(response.body)
      error_message = error_body["error"]["message"].as_s
      
      case response.status_code
      when 400
        raise ValidationError.new("Validation failed: #{error_message}")
      when 404
        raise NotFoundError.new("Resource not found: #{error_message}")
      when 401
        raise AuthenticationError.new("Authentication failed")
      when 403
        raise AuthorizationError.new("Access denied")
      when 500
        raise ServerError.new("Server error: #{error_message}")
      else
        raise APIError.new("Unexpected response: #{response.status_code}")
      end
    end
  rescue ex : Crest::NotFound
    raise NotFoundError.new("Resource not found")
  rescue ex : Crest::RequestFailed
    raise APIError.new("Request failed: #{ex.message}")
  end

  class APIError < Exception; end
  class ValidationError < APIError; end
  class NotFoundError < APIError; end
  class AuthenticationError < APIError; end
  class AuthorizationError < APIError; end
  class ServerError < APIError; end
end

# Helper to print task list
def print_task_list(tasks : Array(JSON::Any))
  tasks.each do |task|
    status = task["status"].as_s.upcase
    priority = task["priority"].as_s.upcase
    title = task["title"].as_s
    
    puts "[#{status}] #{title} (Priority: #{priority})"
    
    if description = task["description"]?.try(&.as_s?)
      puts "  Description: #{description}" unless description.empty?
    end
    
    if tags = task["tags"]?.try(&.as_a?)
      unless tags.empty?
        puts "  Tags: #{tags.map(&.as_s).join(", ")}"
      end
    end
    
    if assigned_to = task["assigned_to"]?.try(&.as_s?)
      puts "  Assigned to: #{assigned_to}"
    end
    
    if due_date = task["due_date"]?.try(&.as_s?)
      puts "  Due: #{due_date}"
    end
    
    puts
  end
end