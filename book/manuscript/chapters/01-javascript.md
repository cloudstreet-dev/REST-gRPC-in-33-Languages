# Chapter 1: JavaScript

## About the JavaScript Programming Language

JavaScript, created by Brendan Eich in 1995 during his time at Netscape, has evolved from a simple scripting language for web browsers into one of the most widely-used programming languages in the world. Originally developed in just 10 days and initially named LiveScript, JavaScript was renamed to capitalize on Java's popularity—despite having little technical relation to Java.

The language's journey from browser-only scripting to full-stack development capability began with the creation of Node.js by Ryan Dahl in 2009. This runtime environment, built on Chrome's V8 JavaScript engine, brought JavaScript to the server-side, enabling developers to use a single language across their entire technology stack.

JavaScript's design philosophy emphasizes flexibility and ease of use. It's a multi-paradigm language supporting object-oriented, imperative, and functional programming styles. Its dynamic typing, first-class functions, and prototype-based inheritance model make it both powerful and accessible to beginners, though these same features can lead to challenges in large-scale applications—a gap that TypeScript (covered in Chapter 2) aims to fill.

The language's ubiquity is perhaps its greatest strength. JavaScript runs on virtually every device with a web browser, making it the de facto language of the web. With Node.js, it also powers server applications, command-line tools, desktop applications (via Electron), mobile applications (via React Native or Ionic), and even embedded systems. This universality has created a massive ecosystem of libraries, frameworks, and tools, making JavaScript one of the most vibrant programming communities.

## JavaScript as a Server (Node.js)

Node.js revolutionized server-side development by bringing JavaScript's event-driven, non-blocking I/O model to backend applications. This architecture makes Node.js particularly well-suited for building scalable network applications, real-time systems, and microservices.

The Node.js ecosystem centers around npm (Node Package Manager), the world's largest software registry. With over 2 million packages, npm provides modules for virtually any functionality you might need, from web frameworks to database drivers to utility libraries.

For our REST API implementation, we'll use Express.js, the de facto standard web framework for Node.js. Express provides a minimal and flexible foundation for building web applications and APIs, with a robust ecosystem of middleware for handling everything from authentication to request parsing.

For gRPC, we'll use the official `@grpc/grpc-js` package, which provides a pure JavaScript implementation of gRPC. This package, maintained by the gRPC team, ensures compatibility with gRPC services written in other languages and provides excellent performance for production applications.

## JavaScript as a Client (Browsers and Node.js)

JavaScript's original domain—the web browser—remains its most important deployment target. Modern JavaScript in the browser has evolved significantly, with ECMAScript 2015 (ES6) and subsequent yearly releases adding features like arrow functions, classes, modules, async/await, and more.

When building REST clients in JavaScript, developers have several options:

1. **Fetch API**: The modern, promise-based standard for making HTTP requests, available in all modern browsers and Node.js (via polyfills)
2. **Axios**: A popular third-party library providing a richer API with interceptors, automatic JSON transformation, and request/response transformation
3. **Native HTTP/HTTPS modules**: Node.js's built-in modules for making HTTP requests

For gRPC clients, the story differs between browsers and Node.js:

- **Node.js**: Can use the full gRPC protocol with `@grpc/grpc-js`
- **Browsers**: Must use gRPC-Web, a subset of gRPC that works over HTTP/1.1 or HTTP/2, as browsers don't expose the low-level HTTP/2 primitives needed for standard gRPC

## REST API Implementation

Let's implement our Task Management REST API using Express.js. This implementation will demonstrate modern JavaScript patterns including ES6+ syntax, async/await for asynchronous operations, and middleware for cross-cutting concerns.

### Server Setup and Configuration

First, let's set up our project structure and dependencies:

```bash
cd code/javascript/rest/server
npm init -y
npm install express cors helmet morgan body-parser uuid
npm install --save-dev nodemon eslint @types/node
```

### Package Configuration

```json
{
  "name": "task-api-rest-server",
  "version": "1.0.0",
  "description": "REST API server for task management in JavaScript",
  "main": "server.js",
  "type": "module",
  "scripts": {
    "start": "node server.js",
    "dev": "nodemon server.js",
    "lint": "eslint ."
  },
  "keywords": ["rest", "api", "tasks", "express"],
  "author": "David Christian Liedle",
  "license": "MIT",
  "dependencies": {
    "express": "^4.18.2",
    "cors": "^2.8.5",
    "helmet": "^7.0.0",
    "morgan": "^1.10.0",
    "body-parser": "^1.20.2",
    "uuid": "^9.0.0"
  },
  "devDependencies": {
    "nodemon": "^3.0.1",
    "eslint": "^8.50.0",
    "@types/node": "^20.0.0"
  }
}
```

### Core Server Implementation

```javascript
// server.js
import express from 'express';
import cors from 'cors';
import helmet from 'helmet';
import morgan from 'morgan';
import bodyParser from 'body-parser';
import { TaskService } from './services/taskService.js';
import { taskRoutes } from './routes/taskRoutes.js';
import { errorHandler } from './middleware/errorHandler.js';
import { validateRequest } from './middleware/validation.js';

const app = express();
const PORT = process.env.PORT || 8080;

// Initialize services
const taskService = new TaskService();

// Middleware
app.use(helmet()); // Security headers
app.use(cors()); // Enable CORS
app.use(morgan('combined')); // Logging
app.use(bodyParser.json()); // Parse JSON bodies
app.use(bodyParser.urlencoded({ extended: true }));

// Health check endpoint
app.get('/health', (req, res) => {
  res.json({ 
    status: 'healthy', 
    timestamp: new Date().toISOString(),
    service: 'task-api-rest-server',
    version: '1.0.0'
  });
});

// API routes
app.use('/api/v1', taskRoutes(taskService));

// Error handling middleware (must be last)
app.use(errorHandler);

// Start server
app.listen(PORT, () => {
  console.log(`REST API server running on http://localhost:${PORT}`);
  console.log(`API documentation: http://localhost:${PORT}/api/v1/docs`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('SIGTERM signal received: closing HTTP server');
  app.close(() => {
    console.log('HTTP server closed');
  });
});
```

### Task Service Implementation

```javascript
// services/taskService.js
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
      status: 'pending',
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
```

### Route Definitions

```javascript
// routes/taskRoutes.js
import { Router } from 'express';
import { validateTask, validateTaskUpdate } from '../middleware/validation.js';

export function taskRoutes(taskService) {
  const router = Router();
  
  // List tasks
  router.get('/tasks', async (req, res, next) => {
    try {
      const result = await taskService.listTasks(req.query);
      res.json(result);
    } catch (error) {
      next(error);
    }
  });
  
  // Get single task
  router.get('/tasks/:id', async (req, res, next) => {
    try {
      const task = await taskService.getTask(req.params.id);
      res.json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Create task
  router.post('/tasks', validateTask, async (req, res, next) => {
    try {
      const task = await taskService.createTask(req.body);
      res.status(201)
         .location(`/api/v1/tasks/${task.id}`)
         .json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Update task
  router.put('/tasks/:id', validateTaskUpdate, async (req, res, next) => {
    try {
      const task = await taskService.updateTask(req.params.id, req.body);
      res.json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Update task status
  router.patch('/tasks/:id/status', async (req, res, next) => {
    try {
      const { status } = req.body;
      if (!status) {
        return res.status(400).json({ 
          error: 'Status is required' 
        });
      }
      
      const task = await taskService.updateTask(req.params.id, { status });
      res.json(task);
    } catch (error) {
      next(error);
    }
  });
  
  // Delete task
  router.delete('/tasks/:id', async (req, res, next) => {
    try {
      await taskService.deleteTask(req.params.id);
      res.status(204).send();
    } catch (error) {
      next(error);
    }
  });
  
  return router;
}
```

### Middleware Implementation

```javascript
// middleware/validation.js
export function validateTask(req, res, next) {
  const { title, priority } = req.body;
  
  const errors = [];
  
  if (!title || title.trim().length === 0) {
    errors.push('Title is required');
  }
  
  if (title && title.length > 200) {
    errors.push('Title must be 200 characters or less');
  }
  
  const validPriorities = ['low', 'medium', 'high', 'critical'];
  if (priority && !validPriorities.includes(priority)) {
    errors.push(`Priority must be one of: ${validPriorities.join(', ')}`);
  }
  
  if (errors.length > 0) {
    return res.status(400).json({ 
      error: 'Validation failed',
      details: errors 
    });
  }
  
  next();
}

export function validateTaskUpdate(req, res, next) {
  const { title, status, priority } = req.body;
  
  const errors = [];
  
  if (title !== undefined) {
    if (title.trim().length === 0) {
      errors.push('Title cannot be empty');
    }
    if (title.length > 200) {
      errors.push('Title must be 200 characters or less');
    }
  }
  
  const validStatuses = ['pending', 'in_progress', 'completed', 'cancelled', 'on_hold'];
  if (status && !validStatuses.includes(status)) {
    errors.push(`Status must be one of: ${validStatuses.join(', ')}`);
  }
  
  const validPriorities = ['low', 'medium', 'high', 'critical'];
  if (priority && !validPriorities.includes(priority)) {
    errors.push(`Priority must be one of: ${validPriorities.join(', ')}`);
  }
  
  if (errors.length > 0) {
    return res.status(400).json({ 
      error: 'Validation failed',
      details: errors 
    });
  }
  
  next();
}
```

```javascript
// middleware/errorHandler.js
export function errorHandler(err, req, res, next) {
  console.error(err.stack);
  
  const statusCode = err.statusCode || 500;
  const message = err.message || 'Internal Server Error';
  
  res.status(statusCode).json({
    error: {
      code: err.name || 'ERROR',
      message: message,
      ...(process.env.NODE_ENV === 'development' && { stack: err.stack })
    }
  });
}
```

### REST Client Implementation

Now let's create a JavaScript client for our REST API:

```javascript
// code/javascript/rest/client/client.js
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
if (import.meta.url === `file://${process.argv[1]}`) {
  demonstrateClient();
}

export { TaskAPIClient };
```

## gRPC Implementation

Now let's implement the same Task Management API using gRPC. This demonstrates the differences between REST and RPC-style APIs, including Protocol Buffers for serialization and the benefits of type-safe service definitions.

### Server Setup

```bash
cd code/javascript/grpc/server
npm init -y
npm install @grpc/grpc-js @grpc/proto-loader uuid
npm install --save-dev nodemon
```

### gRPC Server Implementation

```javascript
// code/javascript/grpc/server/server.js
import grpc from '@grpc/grpc-js';
import protoLoader from '@grpc/proto-loader';
import path from 'path';
import { fileURLToPath } from 'url';
import { TaskServiceImpl } from './services/taskService.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Load proto file
const PROTO_PATH = path.join(__dirname, '../../../shared/protos/tasks.proto');

const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
  keepCase: true,
  longs: String,
  enums: String,
  defaults: true,
  oneofs: true
});

const tasksProto = grpc.loadPackageDefinition(packageDefinition).tasks.v1;

// Create server
const server = new grpc.Server();

// Add service implementation
const taskService = new TaskServiceImpl();

server.addService(tasksProto.TaskService.service, {
  ListTasks: taskService.listTasks.bind(taskService),
  GetTask: taskService.getTask.bind(taskService),
  CreateTask: taskService.createTask.bind(taskService),
  UpdateTask: taskService.updateTask.bind(taskService),
  DeleteTask: taskService.deleteTask.bind(taskService),
  WatchTasks: taskService.watchTasks.bind(taskService)
});

// Start server
const PORT = process.env.GRPC_PORT || '50051';
server.bindAsync(
  `0.0.0.0:${PORT}`,
  grpc.ServerCredentials.createInsecure(),
  (err, port) => {
    if (err) {
      console.error('Failed to bind server:', err);
      return;
    }
    console.log(`gRPC server running on port ${port}`);
    server.start();
  }
);

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('SIGTERM received, shutting down gracefully');
  server.tryShutdown(() => {
    console.log('Server shutdown complete');
  });
});
```

### gRPC Service Implementation

```javascript
// code/javascript/grpc/server/services/taskService.js
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

  // Unary call: List tasks with server streaming
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
    
    // Stream tasks to client
    tasks.forEach(task => {
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
}
```

### gRPC Client Implementation

```javascript
// code/javascript/grpc/client/client.js
import grpc from '@grpc/grpc-js';
import protoLoader from '@grpc/proto-loader';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const PROTO_PATH = path.join(__dirname, '../../../shared/protos/tasks.proto');

class TaskGrpcClient {
  constructor(address = 'localhost:50051') {
    const packageDefinition = protoLoader.loadSync(PROTO_PATH, {
      keepCase: true,
      longs: String,
      enums: String,
      defaults: true,
      oneofs: true
    });
    
    const tasksProto = grpc.loadPackageDefinition(packageDefinition).tasks.v1;
    
    this.client = new tasksProto.TaskService(
      address,
      grpc.credentials.createInsecure()
    );
  }
  
  listTasks(filters = {}) {
    return new Promise((resolve, reject) => {
      const tasks = [];
      const call = this.client.ListTasks(filters);
      
      call.on('data', (task) => {
        tasks.push(task);
      });
      
      call.on('error', (err) => {
        reject(err);
      });
      
      call.on('end', () => {
        resolve(tasks);
      });
    });
  }
  
  getTask(id) {
    return new Promise((resolve, reject) => {
      this.client.GetTask({ id }, (err, task) => {
        if (err) reject(err);
        else resolve(task);
      });
    });
  }
  
  createTask(taskData) {
    return new Promise((resolve, reject) => {
      this.client.CreateTask({ task: taskData }, (err, task) => {
        if (err) reject(err);
        else resolve(task);
      });
    });
  }
  
  updateTask(task, updateMask = []) {
    return new Promise((resolve, reject) => {
      this.client.UpdateTask(
        { task, update_mask: updateMask },
        (err, updated) => {
          if (err) reject(err);
          else resolve(updated);
        }
      );
    });
  }
  
  deleteTask(id) {
    return new Promise((resolve, reject) => {
      this.client.DeleteTask({ id }, (err, response) => {
        if (err) reject(err);
        else resolve(response);
      });
    });
  }
  
  watchTasks(filters = { watch_all: true }) {
    const call = this.client.WatchTasks();
    
    // Send initial watch request
    call.write(filters);
    
    return {
      call,
      onEvent: (callback) => {
        call.on('data', callback);
      },
      onError: (callback) => {
        call.on('error', callback);
      },
      onEnd: (callback) => {
        call.on('end', callback);
      },
      updateFilter: (newFilters) => {
        call.write(newFilters);
      },
      close: () => {
        call.end();
      }
    };
  }
}

// Example usage
async function demonstrateGrpcClient() {
  const client = new TaskGrpcClient();
  
  try {
    // Create a task
    const newTask = await client.createTask({
      title: 'Test gRPC Task',
      description: 'Created via gRPC client',
      priority: 'TASK_PRIORITY_HIGH',
      status: 'TASK_STATUS_PENDING',
      tags: ['test', 'grpc']
    });
    console.log('Created task:', newTask);
    
    // List all tasks
    const tasks = await client.listTasks({
      status: 'TASK_STATUS_PENDING'
    });
    console.log(`Found ${tasks.length} pending tasks`);
    
    // Update task
    const updated = await client.updateTask(
      {
        id: newTask.id,
        status: 'TASK_STATUS_IN_PROGRESS'
      },
      ['status']
    );
    console.log('Updated task status:', updated.status);
    
    // Watch for changes
    const watcher = client.watchTasks({ watch_all: true });
    
    watcher.onEvent((event) => {
      console.log(`Event: ${event.event_type} for task ${event.task.id}`);
    });
    
    // Clean up after 5 seconds
    setTimeout(async () => {
      await client.deleteTask(newTask.id);
      watcher.close();
      console.log('Cleanup complete');
      process.exit(0);
    }, 5000);
    
  } catch (error) {
    console.error('gRPC client error:', error);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demonstrateGrpcClient();
}

export { TaskGrpcClient };
```

## Testing the Implementations

Let's create comprehensive tests for both REST and gRPC implementations:

```javascript
// code/javascript/test/test-runner.js
import { TaskAPIClient } from '../rest/client/client.js';
import { TaskGrpcClient } from '../grpc/client/client.js';

async function testRestAPI() {
  console.log('\n=== Testing REST API ===\n');
  const client = new TaskAPIClient();
  
  try {
    // Test CREATE
    console.log('Testing CREATE...');
    const task = await client.createTask({
      title: 'REST Test Task',
      description: 'Testing REST API',
      priority: 'high'
    });
    console.log('✓ Created task:', task.id);
    
    // Test READ
    console.log('Testing READ...');
    const retrieved = await client.getTask(task.id);
    console.log('✓ Retrieved task:', retrieved.title);
    
    // Test UPDATE
    console.log('Testing UPDATE...');
    const updated = await client.updateTask(task.id, {
      status: 'in_progress'
    });
    console.log('✓ Updated status:', updated.status);
    
    // Test LIST
    console.log('Testing LIST...');
    const { tasks, total_count } = await client.listTasks();
    console.log(`✓ Listed ${tasks.length} of ${total_count} tasks`);
    
    // Test DELETE
    console.log('Testing DELETE...');
    await client.deleteTask(task.id);
    console.log('✓ Deleted task');
    
    console.log('\n✅ All REST API tests passed!\n');
  } catch (error) {
    console.error('❌ REST API test failed:', error.message);
  }
}

async function testGrpcAPI() {
  console.log('\n=== Testing gRPC API ===\n');
  const client = new TaskGrpcClient();
  
  try {
    // Test CREATE
    console.log('Testing CREATE...');
    const task = await client.createTask({
      title: 'gRPC Test Task',
      description: 'Testing gRPC API',
      priority: 'TASK_PRIORITY_HIGH'
    });
    console.log('✓ Created task:', task.id);
    
    // Test READ
    console.log('Testing READ...');
    const retrieved = await client.getTask(task.id);
    console.log('✓ Retrieved task:', retrieved.title);
    
    // Test UPDATE
    console.log('Testing UPDATE...');
    const updated = await client.updateTask({
      id: task.id,
      status: 'TASK_STATUS_IN_PROGRESS'
    });
    console.log('✓ Updated status:', updated.status);
    
    // Test LIST (streaming)
    console.log('Testing LIST...');
    const tasks = await client.listTasks();
    console.log(`✓ Listed ${tasks.length} tasks`);
    
    // Test DELETE
    console.log('Testing DELETE...');
    await client.deleteTask(task.id);
    console.log('✓ Deleted task');
    
    console.log('\n✅ All gRPC API tests passed!\n');
  } catch (error) {
    console.error('❌ gRPC API test failed:', error.message);
  }
}

// Run tests
async function runAllTests() {
  console.log('Starting API tests...');
  
  await testRestAPI();
  await testGrpcAPI();
  
  console.log('All tests complete!');
  process.exit(0);
}

runAllTests();
```

## Performance Comparison

Let's create a performance benchmark comparing REST and gRPC:

```javascript
// code/javascript/test/benchmark.js
import { TaskAPIClient } from '../rest/client/client.js';
import { TaskGrpcClient } from '../grpc/client/client.js';

class PerformanceBenchmark {
  constructor() {
    this.restClient = new TaskAPIClient();
    this.grpcClient = new TaskGrpcClient();
  }
  
  async measureLatency(fn, iterations = 100) {
    const times = [];
    
    for (let i = 0; i < iterations; i++) {
      const start = process.hrtime.bigint();
      await fn();
      const end = process.hrtime.bigint();
      times.push(Number(end - start) / 1000000); // Convert to milliseconds
    }
    
    return {
      min: Math.min(...times),
      max: Math.max(...times),
      avg: times.reduce((a, b) => a + b, 0) / times.length,
      median: times.sort((a, b) => a - b)[Math.floor(times.length / 2)]
    };
  }
  
  async runBenchmarks() {
    console.log('\n=== Performance Benchmarks ===\n');
    
    // Create test data
    const testTask = {
      title: 'Benchmark Task',
      description: 'Performance testing',
      priority: 'medium'
    };
    
    // Benchmark CREATE operations
    console.log('CREATE Operation:');
    const restCreate = await this.measureLatency(
      () => this.restClient.createTask(testTask),
      50
    );
    const grpcCreate = await this.measureLatency(
      () => this.grpcClient.createTask({
        ...testTask,
        priority: 'TASK_PRIORITY_MEDIUM'
      }),
      50
    );
    
    console.log(`  REST: avg ${restCreate.avg.toFixed(2)}ms, median ${restCreate.median.toFixed(2)}ms`);
    console.log(`  gRPC: avg ${grpcCreate.avg.toFixed(2)}ms, median ${grpcCreate.median.toFixed(2)}ms`);
    console.log(`  gRPC is ${(restCreate.avg / grpcCreate.avg).toFixed(2)}x faster\n`);
    
    // Benchmark LIST operations
    console.log('LIST Operation:');
    const restList = await this.measureLatency(
      () => this.restClient.listTasks(),
      100
    );
    const grpcList = await this.measureLatency(
      () => this.grpcClient.listTasks(),
      100
    );
    
    console.log(`  REST: avg ${restList.avg.toFixed(2)}ms, median ${restList.median.toFixed(2)}ms`);
    console.log(`  gRPC: avg ${grpcList.avg.toFixed(2)}ms, median ${grpcList.median.toFixed(2)}ms`);
    console.log(`  gRPC is ${(restList.avg / grpcList.avg).toFixed(2)}x faster\n`);
    
    // Payload size comparison
    console.log('Payload Size Comparison:');
    const restResponse = await this.restClient.listTasks();
    const grpcResponse = await this.grpcClient.listTasks();
    
    const restSize = JSON.stringify(restResponse).length;
    const grpcSize = JSON.stringify(grpcResponse).length; // Approximation
    
    console.log(`  REST: ${restSize} bytes`);
    console.log(`  gRPC: ${grpcSize} bytes (estimated)`);
    console.log(`  gRPC is ${((restSize - grpcSize) / restSize * 100).toFixed(1)}% smaller\n`);
  }
}

// Run benchmarks
const benchmark = new PerformanceBenchmark();
benchmark.runBenchmarks().then(() => {
  console.log('Benchmarks complete!');
  process.exit(0);
});
```

## Docker Deployment

Let's containerize both implementations:

```dockerfile
# code/javascript/Dockerfile
FROM node:18-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy application code
COPY . .

# Expose ports
EXPOSE 8080 50051

# Start both servers
CMD ["node", "start-all.js"]
```

```javascript
// code/javascript/start-all.js
import { spawn } from 'child_process';

// Start REST server
const restServer = spawn('node', ['rest/server/server.js'], {
  stdio: 'inherit',
  env: { ...process.env, PORT: '8080' }
});

// Start gRPC server
const grpcServer = spawn('node', ['grpc/server/server.js'], {
  stdio: 'inherit',
  env: { ...process.env, GRPC_PORT: '50051' }
});

// Handle shutdown
process.on('SIGTERM', () => {
  console.log('Shutting down servers...');
  restServer.kill('SIGTERM');
  grpcServer.kill('SIGTERM');
});
```

## Best Practices and Patterns

### Error Handling

JavaScript's asynchronous nature requires careful error handling:

1. **Use try-catch with async/await**: Always wrap async operations
2. **Create custom error classes**: Extend Error for domain-specific errors
3. **Implement error middleware**: Centralize error handling in Express
4. **Use proper gRPC status codes**: Map application errors to gRPC statuses

### Security Considerations

1. **Input validation**: Validate all inputs on the server
2. **Rate limiting**: Implement rate limiting to prevent abuse
3. **Authentication**: Use JWT tokens or API keys
4. **CORS configuration**: Configure CORS appropriately for your use case
5. **Environment variables**: Never hardcode secrets

### Performance Optimization

1. **Connection pooling**: Reuse connections for database and external services
2. **Caching**: Implement caching strategies (Redis, in-memory)
3. **Compression**: Enable gzip compression for REST responses
4. **Streaming**: Use streaming for large datasets in both REST and gRPC
5. **Worker threads**: Utilize worker threads for CPU-intensive operations

### Code Organization

1. **Modular structure**: Separate concerns into modules
2. **Service layer**: Abstract business logic from transport layer
3. **Dependency injection**: Use DI for better testability
4. **Configuration management**: Centralize configuration
5. **Logging**: Implement structured logging with correlation IDs

## Quick Reference

### Common Commands

```bash
# Development
npm run dev              # Start with nodemon
npm run lint            # Run ESLint
npm test                # Run tests

# Production
npm start               # Start server
npm run build          # Build for production

# Docker
docker build -t task-api-js .
docker run -p 8080:8080 -p 50051:50051 task-api-js

# gRPC tools
npm install -g @grpc/proto-loader
grpc_tools_node_protoc --js_out=. --grpc_out=. tasks.proto
```

### REST Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | /api/v1/tasks | List all tasks |
| GET | /api/v1/tasks/:id | Get single task |
| POST | /api/v1/tasks | Create task |
| PUT | /api/v1/tasks/:id | Update task |
| PATCH | /api/v1/tasks/:id/status | Update status |
| DELETE | /api/v1/tasks/:id | Delete task |

### gRPC Methods

| Method | Type | Description |
|--------|------|-------------|
| ListTasks | Server streaming | List all tasks |
| GetTask | Unary | Get single task |
| CreateTask | Unary | Create task |
| UpdateTask | Unary | Update task |
| DeleteTask | Unary | Delete task |
| WatchTasks | Bidirectional | Watch task changes |

## Conclusion

JavaScript's versatility shines through in these implementations. The REST API leverages Express.js's simplicity and extensive middleware ecosystem, while the gRPC implementation demonstrates JavaScript's ability to handle modern RPC protocols with streaming capabilities.

Key takeaways from this chapter:

1. **JavaScript is truly universal**: The same language works for REST and gRPC, client and server
2. **Ecosystem matters**: npm's vast package repository accelerates development
3. **Async patterns are crucial**: Mastering promises and async/await is essential
4. **Performance varies**: gRPC generally offers better performance, but REST provides broader compatibility
5. **Choose the right tool**: REST for web APIs, gRPC for service-to-service communication

The examples in this chapter provide a foundation for building production-ready APIs in JavaScript. Whether you choose REST for its simplicity and broad support, or gRPC for its performance and type safety, JavaScript's ecosystem provides the tools and libraries to build robust, scalable services.