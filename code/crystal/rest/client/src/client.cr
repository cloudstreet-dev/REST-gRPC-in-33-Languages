require "./task_api_client"

# Example usage
client = TaskAPIClient.new

begin
  # Create a task
  puts "Creating task..."
  task = client.create_task(
    title: "Test Crystal Client",
    description: "Testing the Crystal REST client",
    priority: "high",
    tags: ["test", "crystal"]
  )
  task_id = task["id"].as_s
  puts "Created task: #{task_id}"
  
  # List tasks
  puts "\nListing tasks..."
  result = client.list_tasks(page_size: 10)
  tasks = result["tasks"].as_a
  print_task_list(tasks)
  
  # Update task status
  puts "Updating task status..."
  updated = client.update_task_status(task_id, "in_progress")
  puts "Task status updated to: #{updated["status"].as_s}"
  
  # Get single task
  puts "\nFetching task details..."
  fetched = client.get_task(task_id)
  puts "Task: #{fetched["title"].as_s} - Status: #{fetched["status"].as_s}"
  
  # Delete task
  puts "\nDeleting task..."
  if client.delete_task(task_id)
    puts "Task deleted successfully"
  end
  
rescue ex : TaskAPIClient::APIError
  puts "Error: #{ex.message}"
rescue ex
  puts "Unexpected error: #{ex.message}"
end