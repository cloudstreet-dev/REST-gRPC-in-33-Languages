import 'dart:io';
import 'package:grpc/grpc.dart';
import '../lib/services/task_service.dart';

Future<void> main(List<String> arguments) async {
  final port = int.tryParse(Platform.environment['GRPC_PORT'] ?? '50051') ?? 50051;

  // Create service implementation
  final taskService = TaskServiceImpl();

  // Create server
  final server = Server([taskService]);

  // Start server
  await server.serve(port: port);

  print('gRPC server running on port $port');
  print('Available services:');
  print('  - TaskService.ListTasks (server streaming)');
  print('  - TaskService.GetTask (unary)');
  print('  - TaskService.CreateTask (unary)');
  print('  - TaskService.UpdateTask (unary)');
  print('  - TaskService.DeleteTask (unary)');
  print('  - TaskService.WatchTasks (bidirectional streaming)');

  // Handle graceful shutdown
  ProcessSignal.sigterm.watch().listen((_) async {
    print('SIGTERM received, shutting down gracefully');
    await server.shutdown();
    exit(0);
  });
}