import 'dart:async';
import 'package:grpc/grpc.dart';
import 'package:uuid/uuid.dart';

// Manual proto message definitions (normally generated)
class Task {
  String id;
  String title;
  String description;
  String status;
  String priority;
  List<String> tags;
  String createdBy;
  String assignedTo;
  Timestamp? createdAt;
  Timestamp? updatedAt;
  Timestamp? dueDate;
  Timestamp? completedAt;

  Task({
    required this.id,
    required this.title,
    this.description = '',
    this.status = 'TASK_STATUS_PENDING',
    this.priority = 'TASK_PRIORITY_MEDIUM',
    List<String>? tags,
    this.createdBy = 'system',
    this.assignedTo = '',
    this.createdAt,
    this.updatedAt,
    this.dueDate,
    this.completedAt,
  }) : tags = tags ?? [];

  Map<String, dynamic> toProto() => {
        'id': id,
        'title': title,
        'description': description,
        'status': status,
        'priority': priority,
        'tags': tags,
        'created_by': createdBy,
        'assigned_to': assignedTo,
        'created_at': createdAt?.toProto(),
        'updated_at': updatedAt?.toProto(),
        'due_date': dueDate?.toProto(),
        'completed_at': completedAt?.toProto(),
      };
}

class Timestamp {
  final int seconds;
  final int nanos;

  Timestamp({required this.seconds, this.nanos = 0});

  factory Timestamp.fromDateTime(DateTime dt) {
    final ms = dt.millisecondsSinceEpoch;
    return Timestamp(
      seconds: ms ~/ 1000,
      nanos: (ms % 1000) * 1000000,
    );
  }

  Map<String, dynamic> toProto() => {
        'seconds': seconds,
        'nanos': nanos,
      };
}

class ListTasksRequest {
  int? pageSize;
  String? pageToken;
  String? status;
  String? assignedTo;
  List<String> tags;
  String? sortOrder;

  ListTasksRequest({
    this.pageSize,
    this.pageToken,
    this.status,
    this.assignedTo,
    List<String>? tags,
    this.sortOrder,
  }) : tags = tags ?? [];
}

class GetTaskRequest {
  final String id;
  GetTaskRequest(this.id);
}

class CreateTaskRequest {
  final Task task;
  CreateTaskRequest(this.task);
}

class UpdateTaskRequest {
  final Task task;
  final List<String> updateMask;
  UpdateTaskRequest(this.task, this.updateMask);
}

class DeleteTaskRequest {
  final String id;
  DeleteTaskRequest(this.id);
}

class WatchTasksRequest {
  List<String> taskIds;
  bool watchAll;
  String? assignedTo;

  WatchTasksRequest({
    List<String>? taskIds,
    this.watchAll = false,
    this.assignedTo,
  }) : taskIds = taskIds ?? [];
}

class TaskEvent {
  String eventType;
  Task task;
  Timestamp timestamp;

  TaskEvent({
    required this.eventType,
    required this.task,
    required this.timestamp,
  });

  Map<String, dynamic> toProto() => {
        'event_type': eventType,
        'task': task.toProto(),
        'timestamp': timestamp.toProto(),
      };
}

class TaskServiceImpl extends Service {
  final Map<String, Task> _tasks = {};
  final List<StreamController<TaskEvent>> _watchers = [];

  TaskServiceImpl() {
    _initializeSampleData();
  }

  void _initializeSampleData() {
    final sampleTasks = [
      Task(
        id: const Uuid().v4(),
        title: 'Implement Dart gRPC server',
        description: 'Create gRPC server in Dart',
        status: 'TASK_STATUS_IN_PROGRESS',
        priority: 'TASK_PRIORITY_HIGH',
        tags: ['grpc', 'dart', 'server'],
        createdBy: 'system',
        assignedTo: 'developer',
        createdAt: Timestamp.fromDateTime(DateTime.now()),
        updatedAt: Timestamp.fromDateTime(DateTime.now()),
      ),
      Task(
        id: const Uuid().v4(),
        title: 'Test streaming functionality',
        description: 'Implement and test bidirectional streaming',
        status: 'TASK_STATUS_PENDING',
        priority: 'TASK_PRIORITY_MEDIUM',
        tags: ['grpc', 'streaming', 'testing'],
        createdBy: 'system',
        assignedTo: 'developer',
        createdAt: Timestamp.fromDateTime(DateTime.now()),
        updatedAt: Timestamp.fromDateTime(DateTime.now()),
      ),
    ];

    for (final task in sampleTasks) {
      _tasks[task.id] = task;
    }
  }

  // Server streaming: List tasks
  Stream<Task> listTasks(ServiceCall call, ListTasksRequest request) async* {
    var tasks = _tasks.values.toList();

    // Apply filters
    if (request.status != null && request.status != 'TASK_STATUS_UNSPECIFIED') {
      tasks = tasks.where((task) => task.status == request.status).toList();
    }

    if (request.assignedTo != null) {
      tasks = tasks
          .where((task) => task.assignedTo == request.assignedTo)
          .toList();
    }

    if (request.tags.isNotEmpty) {
      tasks = tasks.where((task) {
        return request.tags.every((tag) => task.tags.contains(tag));
      }).toList();
    }

    // Stream tasks to client
    for (final task in tasks) {
      yield task;
    }
  }

  // Unary call: Get single task
  Future<Task> getTask(ServiceCall call, GetTaskRequest request) async {
    final task = _tasks[request.id];
    if (task == null) {
      throw GrpcError.notFound('Task with ID ${request.id} not found');
    }
    return task;
  }

  // Unary call: Create task
  Future<Task> createTask(ServiceCall call, CreateTaskRequest request) async {
    final task = request.task;

    if (task.title.isEmpty) {
      throw GrpcError.invalidArgument('Task title is required');
    }

    final newTask = Task(
      id: const Uuid().v4(),
      title: task.title,
      description: task.description,
      status: task.status.isNotEmpty ? task.status : 'TASK_STATUS_PENDING',
      priority:
          task.priority.isNotEmpty ? task.priority : 'TASK_PRIORITY_MEDIUM',
      tags: task.tags,
      createdBy: task.createdBy.isNotEmpty ? task.createdBy : 'system',
      assignedTo: task.assignedTo,
      createdAt: Timestamp.fromDateTime(DateTime.now()),
      updatedAt: Timestamp.fromDateTime(DateTime.now()),
      dueDate: task.dueDate,
    );

    _tasks[newTask.id] = newTask;

    // Notify watchers
    _notifyWatchers('EVENT_TYPE_CREATED', newTask);

    return newTask;
  }

  // Unary call: Update task
  Future<Task> updateTask(ServiceCall call, UpdateTaskRequest request) async {
    final task = request.task;

    if (task.id.isEmpty) {
      throw GrpcError.invalidArgument('Task ID is required');
    }

    final existingTask = _tasks[task.id];
    if (existingTask == null) {
      throw GrpcError.notFound('Task with ID ${task.id} not found');
    }

    // Apply updates
    final updatedTask = Task(
      id: existingTask.id,
      title: request.updateMask.contains('title') && task.title.isNotEmpty
          ? task.title
          : existingTask.title,
      description: request.updateMask.contains('description')
          ? task.description
          : existingTask.description,
      status: request.updateMask.contains('status') && task.status.isNotEmpty
          ? task.status
          : existingTask.status,
      priority:
          request.updateMask.contains('priority') && task.priority.isNotEmpty
              ? task.priority
              : existingTask.priority,
      tags: request.updateMask.contains('tags') ? task.tags : existingTask.tags,
      createdBy: existingTask.createdBy,
      assignedTo: request.updateMask.contains('assigned_to')
          ? task.assignedTo
          : existingTask.assignedTo,
      createdAt: existingTask.createdAt,
      updatedAt: Timestamp.fromDateTime(DateTime.now()),
      dueDate: request.updateMask.contains('due_date')
          ? task.dueDate
          : existingTask.dueDate,
      completedAt: task.status == 'TASK_STATUS_COMPLETED' &&
              existingTask.status != 'TASK_STATUS_COMPLETED'
          ? Timestamp.fromDateTime(DateTime.now())
          : existingTask.completedAt,
    );

    _tasks[task.id] = updatedTask;

    // Notify watchers
    _notifyWatchers('EVENT_TYPE_UPDATED', updatedTask);

    return updatedTask;
  }

  // Unary call: Delete task
  Future<Empty> deleteTask(ServiceCall call, DeleteTaskRequest request) async {
    final task = _tasks[request.id];
    if (task == null) {
      throw GrpcError.notFound('Task with ID ${request.id} not found');
    }

    _tasks.remove(request.id);

    // Notify watchers
    _notifyWatchers('EVENT_TYPE_DELETED', task);

    return Empty();
  }

  // Bidirectional streaming: Watch tasks
  Stream<TaskEvent> watchTasks(
      ServiceCall call, Stream<WatchTasksRequest> requests) async* {
    final controller = StreamController<TaskEvent>();
    _watchers.add(controller);

    // Handle incoming requests
    final subscription = requests.listen((request) {
      if (request.watchAll) {
        // Send current tasks
        for (final task in _tasks.values) {
          controller.add(TaskEvent(
            eventType: 'EVENT_TYPE_CREATED',
            task: task,
            timestamp: Timestamp.fromDateTime(DateTime.now()),
          ));
        }
      } else if (request.taskIds.isNotEmpty) {
        for (final id in request.taskIds) {
          final task = _tasks[id];
          if (task != null) {
            controller.add(TaskEvent(
              eventType: 'EVENT_TYPE_CREATED',
              task: task,
              timestamp: Timestamp.fromDateTime(DateTime.now()),
            ));
          }
        }
      }
    });

    // Cleanup on cancel
    call.onCancel.then((_) {
      subscription.cancel();
      controller.close();
      _watchers.remove(controller);
    });

    // Stream events
    yield* controller.stream;
  }

  void _notifyWatchers(String eventType, Task task) {
    final event = TaskEvent(
      eventType: eventType,
      task: task,
      timestamp: Timestamp.fromDateTime(DateTime.now()),
    );

    for (final watcher in _watchers) {
      if (!watcher.isClosed) {
        watcher.add(event);
      }
    }

    // Clean up closed watchers
    _watchers.removeWhere((w) => w.isClosed);
  }
}

// Placeholder for protocol buffer Empty message
class Empty {}