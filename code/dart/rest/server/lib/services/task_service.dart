import '../models/task.dart';

class TaskService {
  final Map<String, Task> _tasks = {};

  TaskService() {
    _initializeSampleData();
  }

  void _initializeSampleData() {
    final sampleTasks = [
      Task.create(
        title: 'Implement Dart REST API',
        description: 'Create REST API with Shelf framework',
        priority: TaskPriority.high,
        tags: ['dart', 'rest', 'api'],
        assignedTo: 'developer',
      ),
      Task.create(
        title: 'Add Flutter client',
        description: 'Create Flutter app that consumes the API',
        priority: TaskPriority.medium,
        tags: ['dart', 'flutter', 'mobile'],
        assignedTo: 'developer',
      ),
    ];

    for (final task in sampleTasks) {
      _tasks[task.id] = task;
    }
  }

  Future<ListTasksResponse> listTasks({
    int? pageSize,
    String? pageToken,
    TaskStatus? status,
    String? assignedTo,
    List<String>? tags,
    SortOrder? sortOrder,
  }) async {
    var tasks = _tasks.values.toList();

    // Apply filters
    if (status != null) {
      tasks = tasks.where((task) => task.status == status).toList();
    }

    if (assignedTo != null) {
      tasks = tasks.where((task) => task.assignedTo == assignedTo).toList();
    }

    if (tags != null && tags.isNotEmpty) {
      tasks = tasks.where((task) {
        return tags.every((tag) => task.tags.contains(tag));
      }).toList();
    }

    // Apply sorting
    tasks = _sortTasks(tasks, sortOrder);

    // Apply pagination
    final effectivePageSize = (pageSize ?? 20).clamp(1, 100);
    final startIndex = pageToken != null ? int.tryParse(pageToken) ?? 0 : 0;
    final endIndex = startIndex + effectivePageSize;

    final paginatedTasks = tasks.skip(startIndex).take(effectivePageSize).toList();
    final nextPageToken = endIndex < tasks.length ? endIndex.toString() : null;

    return ListTasksResponse(
      tasks: paginatedTasks,
      nextPageToken: nextPageToken,
      totalCount: tasks.length,
    );
  }

  Future<Task> getTask(String id) async {
    final task = _tasks[id];
    if (task == null) {
      throw NotFoundException('Task with ID $id not found');
    }
    return task;
  }

  Future<Task> createTask(CreateTaskRequest request) async {
    final task = Task.create(
      title: request.title,
      description: request.description ?? '',
      priority: request.priority ?? TaskPriority.medium,
      tags: request.tags ?? [],
      assignedTo: request.assignedTo,
      dueDate: request.dueDate,
    );

    _tasks[task.id] = task;
    return task;
  }

  Future<Task> updateTask(String id, UpdateTaskRequest request) async {
    final task = await getTask(id);

    DateTime? completedAt = task.completedAt;
    if (request.status == TaskStatus.completed && 
        task.status != TaskStatus.completed) {
      completedAt = DateTime.now();
    }

    final updatedTask = task.copyWith(
      title: request.title,
      description: request.description,
      status: request.status,
      priority: request.priority,
      tags: request.tags,
      assignedTo: request.assignedTo,
      dueDate: request.dueDate,
      completedAt: completedAt,
    );

    _tasks[id] = updatedTask;
    return updatedTask;
  }

  Future<void> deleteTask(String id) async {
    final task = await getTask(id);
    _tasks.remove(id);
  }

  Future<Task> updateTaskStatus(String id, TaskStatus status) async {
    return updateTask(id, UpdateTaskRequest(status: status));
  }

  List<Task> _sortTasks(List<Task> tasks, SortOrder? sortOrder) {
    if (sortOrder == null) return tasks;

    switch (sortOrder) {
      case SortOrder.createdAtAsc:
        tasks.sort((a, b) => a.createdAt.compareTo(b.createdAt));
        break;
      case SortOrder.createdAtDesc:
        tasks.sort((a, b) => b.createdAt.compareTo(a.createdAt));
        break;
      case SortOrder.dueDateAsc:
        tasks.sort((a, b) {
          if (a.dueDate == null) return 1;
          if (b.dueDate == null) return -1;
          return a.dueDate!.compareTo(b.dueDate!);
        });
        break;
      case SortOrder.dueDateDesc:
        tasks.sort((a, b) {
          if (a.dueDate == null) return 1;
          if (b.dueDate == null) return -1;
          return b.dueDate!.compareTo(a.dueDate!);
        });
        break;
      case SortOrder.priorityAsc:
        tasks.sort((a, b) => _getPriorityValue(a.priority)
            .compareTo(_getPriorityValue(b.priority)));
        break;
      case SortOrder.priorityDesc:
        tasks.sort((a, b) => _getPriorityValue(b.priority)
            .compareTo(_getPriorityValue(a.priority)));
        break;
    }

    return tasks;
  }

  int _getPriorityValue(TaskPriority priority) {
    switch (priority) {
      case TaskPriority.low:
        return 1;
      case TaskPriority.medium:
        return 2;
      case TaskPriority.high:
        return 3;
      case TaskPriority.critical:
        return 4;
    }
  }
}

class NotFoundException implements Exception {
  final String message;
  NotFoundException(this.message);

  @override
  String toString() => message;
}

class ValidationException implements Exception {
  final String message;
  ValidationException(this.message);

  @override
  String toString() => message;
}