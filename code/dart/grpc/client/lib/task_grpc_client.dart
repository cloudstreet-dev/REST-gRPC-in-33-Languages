import 'dart:async';
import 'package:grpc/grpc.dart';

// Simplified proto message definitions (normally generated)
class Task {
  final String id;
  final String title;
  final String description;
  final String status;
  final String priority;
  final List<String> tags;
  final String createdBy;
  final String assignedTo;

  Task({
    required this.id,
    required this.title,
    this.description = '',
    this.status = 'TASK_STATUS_PENDING',
    this.priority = 'TASK_PRIORITY_MEDIUM',
    List<String>? tags,
    this.createdBy = 'system',
    this.assignedTo = '',
  }) : tags = tags ?? [];

  factory Task.fromMap(Map<String, dynamic> map) {
    return Task(
      id: map['id'] ?? '',
      title: map['title'] ?? '',
      description: map['description'] ?? '',
      status: map['status'] ?? 'TASK_STATUS_PENDING',
      priority: map['priority'] ?? 'TASK_PRIORITY_MEDIUM',
      tags: List<String>.from(map['tags'] ?? []),
      createdBy: map['created_by'] ?? 'system',
      assignedTo: map['assigned_to'] ?? '',
    );
  }

  Map<String, dynamic> toMap() => {
        'id': id,
        'title': title,
        'description': description,
        'status': status,
        'priority': priority,
        'tags': tags,
        'created_by': createdBy,
        'assigned_to': assignedTo,
      };
}

class TaskEvent {
  final String eventType;
  final Task task;

  TaskEvent({
    required this.eventType,
    required this.task,
  });

  factory TaskEvent.fromMap(Map<String, dynamic> map) {
    return TaskEvent(
      eventType: map['event_type'] ?? '',
      task: Task.fromMap(map['task'] ?? {}),
    );
  }
}

class TaskGrpcClient {
  late ClientChannel _channel;
  late _TaskServiceClient _client;

  TaskGrpcClient({String host = 'localhost', int port = 50051}) {
    _channel = ClientChannel(
      host,
      port: port,
      options: const ChannelOptions(
        credentials: ChannelCredentials.insecure(),
      ),
    );
    _client = _TaskServiceClient(_channel);
  }

  // Server streaming: List tasks
  Stream<Task> listTasks({
    String? status,
    String? assignedTo,
    List<String>? tags,
  }) {
    final request = {
      if (status != null) 'status': status,
      if (assignedTo != null) 'assigned_to': assignedTo,
      if (tags != null && tags.isNotEmpty) 'tags': tags,
    };

    return _client.listTasks(request).map((response) {
      return Task.fromMap(response);
    });
  }

  // Unary call: Get single task
  Future<Task> getTask(String id) async {
    final response = await _client.getTask({'id': id});
    return Task.fromMap(response);
  }

  // Unary call: Create task
  Future<Task> createTask({
    required String title,
    String? description,
    String? priority,
    List<String>? tags,
    String? assignedTo,
  }) async {
    final task = Task(
      id: '', // Will be generated server-side
      title: title,
      description: description ?? '',
      priority: priority ?? 'TASK_PRIORITY_MEDIUM',
      tags: tags,
      assignedTo: assignedTo ?? '',
    );

    final response = await _client.createTask({'task': task.toMap()});
    return Task.fromMap(response);
  }

  // Unary call: Update task
  Future<Task> updateTask(
    String id, {
    String? title,
    String? description,
    String? status,
    String? priority,
    List<String>? tags,
    String? assignedTo,
    List<String>? updateMask,
  }) async {
    final task = Task(
      id: id,
      title: title ?? '',
      description: description ?? '',
      status: status ?? '',
      priority: priority ?? '',
      tags: tags,
      assignedTo: assignedTo ?? '',
    );

    final request = {
      'task': task.toMap(),
      if (updateMask != null) 'update_mask': updateMask,
    };

    final response = await _client.updateTask(request);
    return Task.fromMap(response);
  }

  // Unary call: Delete task
  Future<void> deleteTask(String id) async {
    await _client.deleteTask({'id': id});
  }

  // Bidirectional streaming: Watch tasks
  Stream<TaskEvent> watchTasks({
    List<String>? taskIds,
    bool watchAll = false,
    String? assignedTo,
  }) {
    final requestController = StreamController<Map<String, dynamic>>();

    // Send initial watch request
    requestController.add({
      if (taskIds != null && taskIds.isNotEmpty) 'task_ids': taskIds,
      'watch_all': watchAll,
      if (assignedTo != null) 'assigned_to': assignedTo,
    });

    final responseStream = _client.watchTasks(requestController.stream);

    return responseStream.map((response) {
      return TaskEvent.fromMap(response);
    });
  }

  Future<void> close() async {
    await _channel.shutdown();
  }
}

// Simplified client stub (normally generated from proto)
class _TaskServiceClient {
  final ClientChannel _channel;

  _TaskServiceClient(this._channel);

  Stream<Map<String, dynamic>> listTasks(Map<String, dynamic> request) {
    // Simplified implementation - in reality this would use generated stubs
    return Stream.fromIterable([
      {
        'id': '1',
        'title': 'Sample Task',
        'description': 'From gRPC stream',
        'status': 'TASK_STATUS_PENDING',
        'priority': 'TASK_PRIORITY_MEDIUM',
        'tags': ['sample'],
        'created_by': 'system',
        'assigned_to': 'user',
      }
    ]);
  }

  Future<Map<String, dynamic>> getTask(Map<String, dynamic> request) async {
    // Simplified implementation
    return {
      'id': request['id'],
      'title': 'Retrieved Task',
      'description': 'Task retrieved via gRPC',
      'status': 'TASK_STATUS_PENDING',
      'priority': 'TASK_PRIORITY_MEDIUM',
      'tags': ['retrieved'],
      'created_by': 'system',
      'assigned_to': 'user',
    };
  }

  Future<Map<String, dynamic>> createTask(Map<String, dynamic> request) async {
    // Simplified implementation
    final task = request['task'] as Map<String, dynamic>;
    return {
      ...task,
      'id': DateTime.now().millisecondsSinceEpoch.toString(),
    };
  }

  Future<Map<String, dynamic>> updateTask(Map<String, dynamic> request) async {
    // Simplified implementation
    return request['task'] as Map<String, dynamic>;
  }

  Future<void> deleteTask(Map<String, dynamic> request) async {
    // Simplified implementation
    return;
  }

  Stream<Map<String, dynamic>> watchTasks(
      Stream<Map<String, dynamic>> requests) {
    // Simplified implementation
    return Stream.periodic(
      const Duration(seconds: 5),
      (count) => {
        'event_type': 'EVENT_TYPE_CREATED',
        'task': {
          'id': 'watch-$count',
          'title': 'Watched Task $count',
          'description': 'Task from watch stream',
          'status': 'TASK_STATUS_PENDING',
          'priority': 'TASK_PRIORITY_LOW',
          'tags': ['watched'],
          'created_by': 'system',
          'assigned_to': 'watcher',
        },
      },
    ).take(3);
  }
}