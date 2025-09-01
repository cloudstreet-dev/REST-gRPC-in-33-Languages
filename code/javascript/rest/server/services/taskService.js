import { v4 as uuidv4 } from 'uuid';

export class TaskService {
  constructor() {
    this.tasks = new Map();
    this.initializeSampleData();
  }

  initializeSampleData() {
    const sampleTasks = [
      {
        title: 'Complete JavaScript chapter',
        description: 'Write comprehensive REST and gRPC examples',
        priority: 'high',
        status: 'in_progress',
        tags: ['book', 'javascript', 'api'],
        assigned_to: 'david'
      },
      {
        title: 'Review TypeScript patterns',
        description: 'Ensure TypeScript examples follow best practices',
        priority: 'medium',
        status: 'pending',
        tags: ['book', 'typescript', 'review'],
        assigned_to: 'david'
      }
    ];

    sampleTasks.forEach(task => this.createTask(task));
  }

  async listTasks(filters = {}) {
    let tasks = Array.from(this.tasks.values());
    
    // Apply filters
    if (filters.status) {
      tasks = tasks.filter(task => task.status === filters.status);
    }
    
    if (filters.assigned_to) {
      tasks = tasks.filter(task => task.assigned_to === filters.assigned_to);
    }
    
    if (filters.tags && filters.tags.length > 0) {
      const filterTags = filters.tags.split(',').map(t => t.trim());
      tasks = tasks.filter(task => 
        filterTags.every(tag => task.tags.includes(tag))
      );
    }
    
    // Apply sorting
    tasks = this.sortTasks(tasks, filters.sort_order);
    
    // Apply pagination
    const page_size = Math.min(filters.page_size || 20, 100);
    const start_index = filters.page_token ? parseInt(filters.page_token) : 0;
    const paginatedTasks = tasks.slice(start_index, start_index + page_size);
    
    return {
      tasks: paginatedTasks,
      next_page_token: start_index + page_size < tasks.length 
        ? String(start_index + page_size) 
        : null,
      total_count: tasks.length
    };
  }

  async getTask(id) {
    const task = this.tasks.get(id);
    if (!task) {
      throw new NotFoundError(`Task with ID ${id} not found`);
    }
    return task;
  }

  async createTask(taskData) {
    const now = new Date().toISOString();
    const task = {
      id: uuidv4(),
      title: taskData.title,
      description: taskData.description || '',
      status: taskData.status || 'pending',
      priority: taskData.priority || 'medium',
      tags: taskData.tags || [],
      created_by: taskData.created_by || 'system',
      assigned_to: taskData.assigned_to || null,
      created_at: now,
      updated_at: now,
      due_date: taskData.due_date || null,
      completed_at: null
    };
    
    this.tasks.set(task.id, task);
    return task;
  }

  async updateTask(id, updates) {
    const task = await this.getTask(id);
    
    const updatedTask = {
      ...task,
      ...updates,
      id: task.id, // Prevent ID changes
      created_at: task.created_at, // Preserve creation time
      updated_at: new Date().toISOString()
    };
    
    // Handle status change to completed
    if (updates.status === 'completed' && task.status !== 'completed') {
      updatedTask.completed_at = new Date().toISOString();
    }
    
    this.tasks.set(id, updatedTask);
    return updatedTask;
  }

  async deleteTask(id) {
    const task = await this.getTask(id);
    this.tasks.delete(id);
    return task;
  }

  sortTasks(tasks, sortOrder) {
    switch (sortOrder) {
      case 'created_at_asc':
        return tasks.sort((a, b) => new Date(a.created_at) - new Date(b.created_at));
      case 'created_at_desc':
        return tasks.sort((a, b) => new Date(b.created_at) - new Date(a.created_at));
      case 'due_date_asc':
        return tasks.sort((a, b) => {
          if (!a.due_date) return 1;
          if (!b.due_date) return -1;
          return new Date(a.due_date) - new Date(b.due_date);
        });
      case 'due_date_desc':
        return tasks.sort((a, b) => {
          if (!a.due_date) return 1;
          if (!b.due_date) return -1;
          return new Date(b.due_date) - new Date(a.due_date);
        });
      case 'priority_asc':
        return tasks.sort((a, b) => this.priorityValue(a.priority) - this.priorityValue(b.priority));
      case 'priority_desc':
        return tasks.sort((a, b) => this.priorityValue(b.priority) - this.priorityValue(a.priority));
      default:
        return tasks;
    }
  }

  priorityValue(priority) {
    const values = { low: 1, medium: 2, high: 3, critical: 4 };
    return values[priority] || 2;
  }
}

export class NotFoundError extends Error {
  constructor(message) {
    super(message);
    this.name = 'NotFoundError';
    this.statusCode = 404;
  }
}

export class ValidationError extends Error {
  constructor(message) {
    super(message);
    this.name = 'ValidationError';
    this.statusCode = 400;
  }
}