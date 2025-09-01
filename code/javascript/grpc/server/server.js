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
    console.log('Available services:');
    console.log('  - TaskService.ListTasks (server streaming)');
    console.log('  - TaskService.GetTask (unary)');
    console.log('  - TaskService.CreateTask (unary)');
    console.log('  - TaskService.UpdateTask (unary)');
    console.log('  - TaskService.DeleteTask (unary)');
    console.log('  - TaskService.WatchTasks (bidirectional streaming)');
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