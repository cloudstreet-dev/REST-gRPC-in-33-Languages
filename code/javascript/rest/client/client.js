class TaskAPIClient {
  constructor(baseURL = 'http://localhost:8080/api/v1') {
    this.baseURL = baseURL;
    this.headers = {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    };
  }
  
  setAuthToken(token) {
    this.headers['Authorization'] = `Bearer ${token}`;
  }
  
  setApiKey(apiKey) {
    this.headers['X-API-Key'] = apiKey;
  }
  
  async request(method, path, body = null) {
    const url = `${this.baseURL}${path}`;
    const options = {
      method,
      headers: this.headers
    };
    
    if (body && ['POST', 'PUT', 'PATCH'].includes(method)) {
      options.body = JSON.stringify(body);
    }
    
    try {
      const response = await fetch(url, options);
      
      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || `HTTP ${response.status}`);
      }
      
      if (response.status === 204) {
        return null; // No content
      }
      
      return await response.json();
    } catch (error) {
      console.error(`API request failed: ${error.message}`);
      throw error;
    }
  }
  
  // Task operations
  async listTasks(filters = {}) {
    const params = new URLSearchParams(filters);
    return this.request('GET', `/tasks?${params}`);
  }
  
  async getTask(id) {
    return this.request('GET', `/tasks/${id}`);
  }
  
  async createTask(task) {
    return this.request('POST', '/tasks', task);
  }
  
  async updateTask(id, updates) {
    return this.request('PUT', `/tasks/${id}`, updates);
  }
  
  async updateTaskStatus(id, status) {
    return this.request('PATCH', `/tasks/${id}/status`, { status });
  }
  
  async deleteTask(id) {
    return this.request('DELETE', `/tasks/${id}`);
  }
}

// Example usage
async function demonstrateClient() {
  const client = new TaskAPIClient();
  
  try {
    // Create a task
    const newTask = await client.createTask({
      title: 'Learn JavaScript REST APIs',
      description: 'Complete Chapter 1 of the book',
      priority: 'high',
      tags: ['learning', 'javascript', 'api']
    });
    console.log('Created task:', newTask);
    
    // List all tasks
    const { tasks } = await client.listTasks({ 
      status: 'pending',
      sort_order: 'priority_desc' 
    });
    console.log(`Found ${tasks.length} pending tasks`);
    
    // Update task status
    const updated = await client.updateTaskStatus(newTask.id, 'in_progress');
    console.log('Updated task status:', updated.status);
    
    // Delete task
    await client.deleteTask(newTask.id);
    console.log('Task deleted successfully');
    
  } catch (error) {
    console.error('Client demo failed:', error);
  }
}

// Run if called directly
if (typeof module !== 'undefined' && require.main === module) {
  demonstrateClient();
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { TaskAPIClient };
}