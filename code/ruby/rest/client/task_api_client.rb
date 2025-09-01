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
  
  def list_tasks(page_size: nil, page_token: nil, status: nil, assigned_to: nil, tags: nil, sort_order: nil)
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
  
  def create_task(title:, description: nil, priority: nil, tags: nil, assigned_to: nil, due_date: nil)
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

# Example usage
if __FILE__ == $0
  client = TaskAPIClient.new
  
  begin
    # Create a task
    puts "Creating task..."
    task = client.create_task(
      title: 'Test Ruby Client',
      description: 'Testing the Ruby REST client',
      priority: 'high',
      tags: ['test', 'ruby']
    )
    puts "Created task: #{task['id']}"
    
    # List tasks
    puts "\nListing tasks..."
    result = client.list_tasks(page_size: 10)
    result['tasks'].each do |t|
      puts "  [#{t['status'].upcase}] #{t['title']} (Priority: #{t['priority']})"
    end
    
    # Update task status
    puts "\nUpdating task status..."
    updated = client.update_task_status(task['id'], 'in_progress')
    puts "Task status updated to: #{updated['status']}"
    
    # Get single task
    puts "\nFetching task details..."
    fetched = client.get_task(task['id'])
    puts "Task: #{fetched['title']} - Status: #{fetched['status']}"
    
    # Delete task
    puts "\nDeleting task..."
    client.delete_task(task['id'])
    puts "Task deleted successfully"
    
  rescue => e
    puts "Error: #{e.message}"
  end
end