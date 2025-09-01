import '../lib/task_api_client.dart';

Future<void> main() async {
  final client = TaskApiClient();

  try {
    print('=== Dart REST Client Demo ===\n');

    // Create a task
    print('Creating task...');
    final newTask = await client.createTask(
      title: 'Learn Dart REST APIs',
      description: 'Complete Chapter 3 of the book',
      priority: TaskPriority.high,
      tags: ['learning', 'dart', 'api'],
    );
    print('Created task: ${newTask.title} (${newTask.id})');

    // List all tasks
    print('\nListing tasks...');
    final response = await client.listTasks(
      status: TaskStatus.pending,
      sortOrder: SortOrder.priorityDesc,
    );
    print('Found ${response.tasks.length} pending tasks:');
    for (final task in response.tasks) {
      print('  - ${task.title} (${task.priority})');
    }

    // Get single task
    print('\nGetting task ${newTask.id}...');
    final retrieved = await client.getTask(newTask.id);
    print('Retrieved: ${retrieved.title}');

    // Update task status
    print('\nUpdating task status...');
    final updated = await client.updateTaskStatus(
      newTask.id,
      TaskStatus.inProgress,
    );
    print('Updated status: ${updated.status}');

    // Update task
    print('\nUpdating task details...');
    final fullyUpdated = await client.updateTask(
      newTask.id,
      description: 'Updated description',
      tags: ['dart', 'updated'],
    );
    print('Updated tags: ${fullyUpdated.tags.join(", ")}');

    // Delete task
    print('\nDeleting task...');
    await client.deleteTask(newTask.id);
    print('Task deleted successfully');

    print('\n✅ All REST API operations completed successfully!');
  } catch (e) {
    print('❌ Error: $e');
  } finally {
    client.dispose();
  }
}