import 'dart:async';
import '../lib/task_grpc_client.dart';

Future<void> main() async {
  final client = TaskGrpcClient();

  try {
    print('=== Dart gRPC Client Demo ===\n');

    // Create a task
    print('Creating task via gRPC...');
    final newTask = await client.createTask(
      title: 'Test Dart gRPC Task',
      description: 'Created via gRPC client',
      priority: 'TASK_PRIORITY_HIGH',
      tags: ['test', 'grpc', 'dart'],
    );
    print('Created task: ${newTask.title} (${newTask.id})');

    // List tasks (server streaming)
    print('\nListing tasks (server streaming)...');
    await for (final task in client.listTasks(status: 'TASK_STATUS_PENDING')) {
      print('  - ${task.title} (${task.priority})');
    }

    // Get single task
    print('\nGetting task ${newTask.id}...');
    final retrieved = await client.getTask(newTask.id);
    print('Retrieved: ${retrieved.title}');

    // Update task
    print('\nUpdating task...');
    final updated = await client.updateTask(
      newTask.id,
      status: 'TASK_STATUS_IN_PROGRESS',
      updateMask: ['status'],
    );
    print('Updated status: ${updated.status}');

    // Watch for changes (bidirectional streaming)
    print('\nWatching for task changes (5 seconds)...');
    final watchStream = client.watchTasks(watchAll: true);
    
    final watchSubscription = watchStream.listen((event) {
      print('  Event: ${event.eventType} for task "${event.task.title}"');
    });

    // Wait for some events
    await Future.delayed(const Duration(seconds: 5));
    await watchSubscription.cancel();

    // Delete task
    print('\nDeleting task...');
    await client.deleteTask(newTask.id);
    print('Task deleted successfully');

    print('\n✅ All gRPC operations completed successfully!');
  } catch (e) {
    print('❌ Error: $e');
  } finally {
    await client.close();
  }
}