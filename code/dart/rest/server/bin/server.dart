import 'dart:io';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;
import 'package:shelf_router/shelf_router.dart';
import '../lib/services/task_service.dart';
import '../lib/routes/task_routes.dart';
import '../lib/middleware/cors_middleware.dart';
import '../lib/middleware/error_middleware.dart';

void main(List<String> arguments) async {
  final port = int.tryParse(Platform.environment['PORT'] ?? '8080') ?? 8080;

  // Initialize services
  final taskService = TaskService();
  final taskRoutes = TaskRoutes(taskService);

  // Create main router
  final router = Router();

  // Health check endpoint
  router.get('/health', (Request request) {
    return Response.ok(
      '{"status":"healthy","timestamp":"${DateTime.now().toIso8601String()}","service":"task-api-dart","version":"1.0.0"}',
      headers: {'Content-Type': 'application/json'},
    );
  });

  // Mount API routes
  router.mount('/api/v1/', taskRoutes.router);

  // Create handler with middleware pipeline
  final handler = Pipeline()
      .addMiddleware(logRequests())
      .addMiddleware(corsMiddleware())
      .addMiddleware(errorMiddleware())
      .addHandler(router);

  // Start server
  final server = await shelf_io.serve(handler, '0.0.0.0', port);
  
  print('REST API server running on http://localhost:${server.port}');
  print('Health check: http://localhost:${server.port}/health');
  print('API endpoint: http://localhost:${server.port}/api/v1/tasks');

  // Graceful shutdown
  ProcessSignal.sigterm.watch().listen((_) async {
    print('SIGTERM received, shutting down gracefully');
    await server.close(force: true);
    exit(0);
  });
}