import 'dart:convert';
import 'package:shelf/shelf.dart';
import 'package:shelf_router/shelf_router.dart';
import '../services/task_service.dart';
import '../models/task.dart';

class TaskRoutes {
  final TaskService _taskService;

  TaskRoutes(this._taskService);

  Router get router {
    final router = Router();

    // List tasks
    router.get('/tasks', _listTasks);

    // Get single task
    router.get('/tasks/<id>', _getTask);

    // Create task
    router.post('/tasks', _createTask);

    // Update task
    router.put('/tasks/<id>', _updateTask);

    // Update task status
    router.patch('/tasks/<id>/status', _updateTaskStatus);

    // Delete task
    router.delete('/tasks/<id>', _deleteTask);

    return router;
  }

  Future<Response> _listTasks(Request request) async {
    final queryParams = request.url.queryParameters;

    final pageSize = queryParams['page_size'] != null
        ? int.tryParse(queryParams['page_size']!)
        : null;

    final pageToken = queryParams['page_token'];

    TaskStatus? status;
    if (queryParams['status'] != null) {
      try {
        status = TaskStatus.values.firstWhere(
          (e) => e.name == queryParams['status'],
        );
      } catch (_) {
        // Invalid status, ignore
      }
    }

    final assignedTo = queryParams['assigned_to'];

    List<String>? tags;
    if (queryParams['tags'] != null) {
      tags = queryParams['tags']!.split(',').map((t) => t.trim()).toList();
    }

    SortOrder? sortOrder;
    if (queryParams['sort_order'] != null) {
      final sortString = queryParams['sort_order']!.replaceAll('_', '');
      try {
        sortOrder = SortOrder.values.firstWhere(
          (e) => e.name.toLowerCase() == sortString.toLowerCase(),
        );
      } catch (_) {
        // Invalid sort order, ignore
      }
    }

    final response = await _taskService.listTasks(
      pageSize: pageSize,
      pageToken: pageToken,
      status: status,
      assignedTo: assignedTo,
      tags: tags,
      sortOrder: sortOrder,
    );

    return Response.ok(
      jsonEncode(response.toJson()),
      headers: {'Content-Type': 'application/json'},
    );
  }

  Future<Response> _getTask(Request request, String id) async {
    final task = await _taskService.getTask(id);
    return Response.ok(
      jsonEncode(task.toJson()),
      headers: {'Content-Type': 'application/json'},
    );
  }

  Future<Response> _createTask(Request request) async {
    final body = await request.readAsString();
    final json = jsonDecode(body) as Map<String, dynamic>;

    // Validate required fields
    if (json['title'] == null || (json['title'] as String).isEmpty) {
      throw ValidationException('Title is required');
    }

    final createRequest = CreateTaskRequest.fromJson(json);
    final task = await _taskService.createTask(createRequest);

    return Response(
      201,
      body: jsonEncode(task.toJson()),
      headers: {
        'Content-Type': 'application/json',
        'Location': '/api/v1/tasks/${task.id}',
      },
    );
  }

  Future<Response> _updateTask(Request request, String id) async {
    final body = await request.readAsString();
    final json = jsonDecode(body) as Map<String, dynamic>;

    final updateRequest = UpdateTaskRequest.fromJson(json);
    final task = await _taskService.updateTask(id, updateRequest);

    return Response.ok(
      jsonEncode(task.toJson()),
      headers: {'Content-Type': 'application/json'},
    );
  }

  Future<Response> _updateTaskStatus(Request request, String id) async {
    final body = await request.readAsString();
    final json = jsonDecode(body) as Map<String, dynamic>;

    if (json['status'] == null) {
      throw ValidationException('Status is required');
    }

    TaskStatus status;
    try {
      status = TaskStatus.values.firstWhere(
        (e) => e.name == json['status'],
      );
    } catch (_) {
      throw ValidationException('Invalid status value');
    }

    final task = await _taskService.updateTaskStatus(id, status);

    return Response.ok(
      jsonEncode(task.toJson()),
      headers: {'Content-Type': 'application/json'},
    );
  }

  Future<Response> _deleteTask(Request request, String id) async {
    await _taskService.deleteTask(id);
    return Response(204);
  }
}