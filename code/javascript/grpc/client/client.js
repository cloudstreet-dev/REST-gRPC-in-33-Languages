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
    console.log('=== gRPC Client Demo ===\n');
    
    // Create a task
    const newTask = await client.createTask({
      title: 'Test gRPC Task',
      description: 'Created via gRPC client',
      priority: 'TASK_PRIORITY_HIGH',
      status: 'TASK_STATUS_PENDING',
      tags: ['test', 'grpc']
    });
    console.log('Created task:', {
      id: newTask.id,
      title: newTask.title,
      priority: newTask.priority
    });
    
    // List all tasks
    const tasks = await client.listTasks({
      status: 'TASK_STATUS_PENDING'
    });
    console.log(`\nFound ${tasks.length} pending tasks`);
    tasks.forEach(task => {
      console.log(`  - ${task.title} (${task.id})`);
    });
    
    // Get single task
    const retrieved = await client.getTask(newTask.id);
    console.log('\nRetrieved task:', retrieved.title);
    
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
    console.log('\nWatching for task changes (5 seconds)...');
    const watcher = client.watchTasks({ watch_all: true });
    
    watcher.onEvent((event) => {
      console.log(`  Event: ${event.event_type} for task "${event.task.title}"`);
    });
    
    watcher.onError((err) => {
      console.error('  Watch error:', err.message);
    });
    
    // Create another task to trigger watch event
    setTimeout(async () => {
      await client.createTask({
        title: 'Trigger watch event',
        description: 'This should appear in the watch stream',
        priority: 'TASK_PRIORITY_LOW'
      });
    }, 1000);
    
    // Clean up after 5 seconds
    setTimeout(async () => {
      await client.deleteTask(newTask.id);
      watcher.close();
      console.log('\nCleanup complete');
      process.exit(0);
    }, 5000);
    
  } catch (error) {
    console.error('gRPC client error:', error.message);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demonstrateGrpcClient();
}

export { TaskGrpcClient };