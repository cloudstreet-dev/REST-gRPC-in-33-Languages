import 'dart:convert';
import 'package:http/http.dart' as http;

enum TaskStatus {
  pending,
  inProgress,
  completed,
  cancelled,
  onHold,
}

enum TaskPriority {
  low,
  medium,
  high,
  critical,
}

enum SortOrder {
  createdAtAsc,
  createdAtDesc,
  dueDateAsc,
  dueDateDesc,
  priorityAsc,
  priorityDesc,
}

class Task {
  final String id;
  final String title;
  final String description;
  final String status;
  final String priority;
  final List<String> tags;
  final String createdBy;
  final String? assignedTo;
  final String createdAt;
  final String updatedAt;
  final String? dueDate;
  final String? completedAt;

  Task({
    required this.id,
    required this.title,
    required this.description,
    required this.status,
    required this.priority,
    required this.tags,
    required this.createdBy,
    this.assignedTo,
    required this.createdAt,
    required this.updatedAt,
    this.dueDate,
    this.completedAt,
  });

  factory Task.fromJson(Map<String, dynamic> json) {
    return Task(
      id: json['id'] as String,
      title: json['title'] as String,
      description: json['description'] as String? ?? '',
      status: json['status'] as String,
      priority: json['priority'] as String,
      tags: List<String>.from(json['tags'] ?? []),
      createdBy: json['created_by'] as String? ?? 'system',
      assignedTo: json['assigned_to'] as String?,
      createdAt: json['created_at'] as String,
      updatedAt: json['updated_at'] as String,
      dueDate: json['due_date'] as String?,
      completedAt: json['completed_at'] as String?,
    );
  }
}

class ListTasksResponse {
  final List<Task> tasks;
  final String? nextPageToken;
  final int totalCount;

  ListTasksResponse({
    required this.tasks,
    this.nextPageToken,
    required this.totalCount,
  });

  factory ListTasksResponse.fromJson(Map<String, dynamic> json) {
    return ListTasksResponse(
      tasks: (json['tasks'] as List)
          .map((task) => Task.fromJson(task as Map<String, dynamic>))
          .toList(),
      nextPageToken: json['next_page_token'] as String?,
      totalCount: json['total_count'] as int,
    );
  }
}

class TaskApiClient {
  final String baseUrl;
  final Map<String, String> headers;
  final http.Client _client;

  TaskApiClient({
    this.baseUrl = 'http://localhost:8080/api/v1',
    Map<String, String>? headers,
  })  : headers = headers ??
            {
              'Content-Type': 'application/json',
              'Accept': 'application/json',
            },
        _client = http.Client();

  void setAuthToken(String token) {
    headers['Authorization'] = 'Bearer $token';
  }

  void setApiKey(String apiKey) {
    headers['X-API-Key'] = apiKey;
  }

  Future<T> _request<T>(
    String method,
    String path, {
    Object? body,
    T Function(Map<String, dynamic>)? fromJson,
  }) async {
    final uri = Uri.parse('$baseUrl$path');
    late http.Response response;

    try {
      switch (method) {
        case 'GET':
          response = await _client.get(uri, headers: headers);
          break;
        case 'POST':
          response = await _client.post(
            uri,
            headers: headers,
            body: body != null ? jsonEncode(body) : null,
          );
          break;
        case 'PUT':
          response = await _client.put(
            uri,
            headers: headers,
            body: body != null ? jsonEncode(body) : null,
          );
          break;
        case 'PATCH':
          response = await _client.patch(
            uri,
            headers: headers,
            body: body != null ? jsonEncode(body) : null,
          );
          break;
        case 'DELETE':
          response = await _client.delete(uri, headers: headers);
          break;
        default:
          throw Exception('Unsupported HTTP method: $method');
      }

      if (response.statusCode == 204) {
        return null as T;
      }

      if (response.statusCode >= 200 && response.statusCode < 300) {
        if (fromJson != null && response.body.isNotEmpty) {
          final json = jsonDecode(response.body) as Map<String, dynamic>;
          return fromJson(json);
        }
        return response.body as T;
      } else {
        final error = jsonDecode(response.body);
        throw Exception(
            error['error']?['message'] ?? 'HTTP ${response.statusCode}');
      }
    } catch (e) {
      print('API request failed: $e');
      rethrow;
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
    final queryParams = <String, String>{};

    if (pageSize != null) queryParams['page_size'] = pageSize.toString();
    if (pageToken != null) queryParams['page_token'] = pageToken;
    if (status != null) queryParams['status'] = status.name;
    if (assignedTo != null) queryParams['assigned_to'] = assignedTo;
    if (tags != null && tags.isNotEmpty) queryParams['tags'] = tags.join(',');
    if (sortOrder != null) {
      // Convert camelCase to snake_case
      final sortString = sortOrder.name
          .replaceAllMapped(
              RegExp(r'[A-Z]'), (match) => '_${match.group(0)!.toLowerCase()}')
          .substring(1);
      queryParams['sort_order'] = sortString;
    }

    final queryString = queryParams.entries
        .map((e) => '${e.key}=${Uri.encodeComponent(e.value)}')
        .join('&');

    final path = '/tasks${queryString.isNotEmpty ? '?$queryString' : ''}';

    return _request(
      'GET',
      path,
      fromJson: ListTasksResponse.fromJson,
    );
  }

  Future<Task> getTask(String id) async {
    return _request(
      'GET',
      '/tasks/$id',
      fromJson: Task.fromJson,
    );
  }

  Future<Task> createTask({
    required String title,
    String? description,
    TaskPriority? priority,
    List<String>? tags,
    String? assignedTo,
    DateTime? dueDate,
  }) async {
    final body = {
      'title': title,
      if (description != null) 'description': description,
      if (priority != null) 'priority': priority.name,
      if (tags != null) 'tags': tags,
      if (assignedTo != null) 'assigned_to': assignedTo,
      if (dueDate != null) 'due_date': dueDate.toIso8601String(),
    };

    return _request(
      'POST',
      '/tasks',
      body: body,
      fromJson: Task.fromJson,
    );
  }

  Future<Task> updateTask(
    String id, {
    String? title,
    String? description,
    TaskStatus? status,
    TaskPriority? priority,
    List<String>? tags,
    String? assignedTo,
    DateTime? dueDate,
  }) async {
    final body = {
      if (title != null) 'title': title,
      if (description != null) 'description': description,
      if (status != null) 'status': status.name,
      if (priority != null) 'priority': priority.name,
      if (tags != null) 'tags': tags,
      if (assignedTo != null) 'assigned_to': assignedTo,
      if (dueDate != null) 'due_date': dueDate.toIso8601String(),
    };

    return _request(
      'PUT',
      '/tasks/$id',
      body: body,
      fromJson: Task.fromJson,
    );
  }

  Future<Task> updateTaskStatus(String id, TaskStatus status) async {
    return _request(
      'PATCH',
      '/tasks/$id/status',
      body: {'status': status.name},
      fromJson: Task.fromJson,
    );
  }

  Future<void> deleteTask(String id) async {
    await _request<void>('DELETE', '/tasks/$id');
  }

  void dispose() {
    _client.close();
  }
}