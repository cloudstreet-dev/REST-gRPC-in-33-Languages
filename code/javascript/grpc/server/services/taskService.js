import grpc from '@grpc/grpc-js';
import { v4 as uuidv4 } from 'uuid';

export class TaskServiceImpl {
  constructor() {
    this.tasks = new Map();
    this.watchers = new Set();
    this.initializeSampleData();
  }

  initializeSampleData() {
    const sampleTasks = [
      {
        id: uuidv4(),
        title: 'Implement gRPC server',
        description: 'Create gRPC server in JavaScript',
        status: 'TASK_STATUS_IN_PROGRESS',
        priority: 'TASK_PRIORITY_HIGH',
        tags: ['grpc', 'javascript', 'server'],
        created_by: 'system',
        assigned_to: 'developer',
        created_at: this.getCurrentTimestamp(),
        updated_at: this.getCurrentTimestamp()
      },
      {
        id: uuidv4(),
        title: 'Test bidirectional streaming',
        description: 'Implement and test Watch functionality',
        status: 'TASK_STATUS_PENDING',
        priority: 'TASK_PRIORITY_MEDIUM',
        tags: ['grpc', 'streaming', 'testing'],
        created_by: 'system',
        assigned_to: 'developer',
        created_at: this.getCurrentTimestamp(),
        updated_at: this.getCurrentTimestamp()
      }
    ];
    
    sampleTasks.forEach(task => {
      this.tasks.set(task.id, task);
    });
  }

  getCurrentTimestamp() {
    const now = new Date();
    return {
      seconds: Math.floor(now.getTime() / 1000),
      nanos: (now.getTime() % 1000) * 1000000
    };
  }

  // Server streaming: List tasks
  listTasks(call) {
    const request = call.request;
    let tasks = Array.from(this.tasks.values());
    
    // Apply filters
    if (request.status && request.status !== 'TASK_STATUS_UNSPECIFIED') {
      tasks = tasks.filter(task => task.status === request.status);
    }
    
    if (request.assigned_to) {
      tasks = tasks.filter(task => task.assigned_to === request.assigned_to);
    }
    
    if (request.tags && request.tags.length > 0) {
      tasks = tasks.filter(task =>
        request.tags.every(tag => task.tags.includes(tag))
      );
    }
    
    // Apply sorting
    tasks = this.sortTasks(tasks, request.sort_order);
    
    // Apply pagination
    const pageSize = request.page_size || 20;
    const startIndex = request.page_token ? parseInt(request.page_token) : 0;
    const paginatedTasks = tasks.slice(startIndex, startIndex + pageSize);
    
    // Stream tasks to client
    paginatedTasks.forEach(task => {
      call.write(task);
    });
    
    call.end();
  }

  // Unary call: Get single task
  getTask(call, callback) {
    const { id } = call.request;
    const task = this.tasks.get(id);
    
    if (!task) {
      return callback({
        code: grpc.status.NOT_FOUND,
        message: `Task with ID ${id} not found`
      });
    }
    
    callback(null, task);
  }

  // Unary call: Create task
  createTask(call, callback) {
    const { task } = call.request;
    
    if (!task.title) {
      return callback({
        code: grpc.status.INVALID_ARGUMENT,
        message: 'Task title is required'
      });
    }
    
    const newTask = {
      id: uuidv4(),
      title: task.title,
      description: task.description || '',
      status: task.status || 'TASK_STATUS_PENDING',
      priority: task.priority || 'TASK_PRIORITY_MEDIUM',
      tags: task.tags || [],
      created_by: task.created_by || 'system',
      assigned_to: task.assigned_to || '',
      created_at: this.getCurrentTimestamp(),
      updated_at: this.getCurrentTimestamp(),
      due_date: task.due_date || null,
      completed_at: null
    };
    
    this.tasks.set(newTask.id, newTask);
    
    // Notify watchers
    this.notifyWatchers('EVENT_TYPE_CREATED', newTask);
    
    callback(null, newTask);
  }

  // Unary call: Update task
  updateTask(call, callback) {
    const { task, update_mask } = call.request;
    
    if (!task.id) {
      return callback({
        code: grpc.status.INVALID_ARGUMENT,
        message: 'Task ID is required'
      });
    }
    
    const existingTask = this.tasks.get(task.id);
    if (!existingTask) {
      return callback({
        code: grpc.status.NOT_FOUND,
        message: `Task with ID ${task.id} not found`
      });
    }
    
    // Apply updates based on update_mask or all provided fields
    const updatedTask = { ...existingTask };
    
    if (update_mask && update_mask.length > 0) {
      // Update only specified fields
      update_mask.forEach(field => {
        if (task[field] !== undefined) {
          updatedTask[field] = task[field];
        }
      });
    } else {
      // Update all provided fields
      Object.keys(task).forEach(key => {
        if (task[key] !== undefined && key !== 'id' && key !== 'created_at') {
          updatedTask[key] = task[key];
        }
      });
    }
    
    updatedTask.updated_at = this.getCurrentTimestamp();
    
    // Handle status change to completed
    if (updatedTask.status === 'TASK_STATUS_COMPLETED' && 
        existingTask.status !== 'TASK_STATUS_COMPLETED') {
      updatedTask.completed_at = this.getCurrentTimestamp();
    }
    
    this.tasks.set(task.id, updatedTask);
    
    // Notify watchers
    this.notifyWatchers('EVENT_TYPE_UPDATED', updatedTask);
    
    callback(null, updatedTask);
  }

  // Unary call: Delete task
  deleteTask(call, callback) {
    const { id } = call.request;
    const task = this.tasks.get(id);
    
    if (!task) {
      return callback({
        code: grpc.status.NOT_FOUND,
        message: `Task with ID ${id} not found`
      });
    }
    
    this.tasks.delete(id);
    
    // Notify watchers
    this.notifyWatchers('EVENT_TYPE_DELETED', task);
    
    callback(null, {});
  }

  // Bidirectional streaming: Watch tasks
  watchTasks(call) {
    // Add this call to watchers
    this.watchers.add(call);
    
    // Handle incoming requests
    call.on('data', (request) => {
      console.log('Watch request received:', request);
      
      // Send current tasks matching the filter
      if (request.watch_all) {
        this.tasks.forEach(task => {
          call.write({
            event_type: 'EVENT_TYPE_CREATED',
            task: task,
            timestamp: this.getCurrentTimestamp()
          });
        });
      } else if (request.task_ids && request.task_ids.length > 0) {
        request.task_ids.forEach(id => {
          const task = this.tasks.get(id);
          if (task) {
            call.write({
              event_type: 'EVENT_TYPE_CREATED',
              task: task,
              timestamp: this.getCurrentTimestamp()
            });
          }
        });
      }
    });
    
    // Handle client disconnect
    call.on('end', () => {
      this.watchers.delete(call);
      call.end();
    });
    
    // Handle errors
    call.on('error', (err) => {
      console.error('Watch stream error:', err);
      this.watchers.delete(call);
    });
  }

  // Helper: Notify all watchers of task changes
  notifyWatchers(eventType, task) {
    const event = {
      event_type: eventType,
      task: task,
      timestamp: this.getCurrentTimestamp()
    };
    
    this.watchers.forEach(watcher => {
      try {
        watcher.write(event);
      } catch (err) {
        console.error('Failed to notify watcher:', err);
        this.watchers.delete(watcher);
      }
    });
  }

  // Helper: Sort tasks
  sortTasks(tasks, sortOrder) {
    switch (sortOrder) {
      case 'SORT_ORDER_CREATED_AT_ASC':
        return tasks.sort((a, b) => a.created_at.seconds - b.created_at.seconds);
      case 'SORT_ORDER_CREATED_AT_DESC':
        return tasks.sort((a, b) => b.created_at.seconds - a.created_at.seconds);
      case 'SORT_ORDER_DUE_DATE_ASC':
        return tasks.sort((a, b) => {
          if (!a.due_date) return 1;
          if (!b.due_date) return -1;
          return a.due_date.seconds - b.due_date.seconds;
        });
      case 'SORT_ORDER_DUE_DATE_DESC':
        return tasks.sort((a, b) => {
          if (!a.due_date) return 1;
          if (!b.due_date) return -1;
          return b.due_date.seconds - a.due_date.seconds;
        });
      case 'SORT_ORDER_PRIORITY_ASC':
        return tasks.sort((a, b) => this.priorityValue(a.priority) - this.priorityValue(b.priority));
      case 'SORT_ORDER_PRIORITY_DESC':
        return tasks.sort((a, b) => this.priorityValue(b.priority) - this.priorityValue(a.priority));
      default:
        return tasks;
    }
  }

  priorityValue(priority) {
    const values = {
      'TASK_PRIORITY_LOW': 1,
      'TASK_PRIORITY_MEDIUM': 2,
      'TASK_PRIORITY_HIGH': 3,
      'TASK_PRIORITY_CRITICAL': 4
    };
    return values[priority] || 2;
  }
}