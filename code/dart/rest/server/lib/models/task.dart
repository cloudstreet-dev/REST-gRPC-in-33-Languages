import 'package:uuid/uuid.dart';

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
  final TaskStatus status;
  final TaskPriority priority;
  final List<String> tags;
  final String createdBy;
  final String? assignedTo;
  final DateTime createdAt;
  final DateTime updatedAt;
  final DateTime? dueDate;
  final DateTime? completedAt;

  Task({
    required this.id,
    required this.title,
    this.description = '',
    this.status = TaskStatus.pending,
    this.priority = TaskPriority.medium,
    this.tags = const [],
    this.createdBy = 'system',
    this.assignedTo,
    required this.createdAt,
    required this.updatedAt,
    this.dueDate,
    this.completedAt,
  });

  factory Task.create({
    required String title,
    String description = '',
    TaskPriority priority = TaskPriority.medium,
    List<String> tags = const [],
    String? assignedTo,
    DateTime? dueDate,
    String createdBy = 'system',
  }) {
    final now = DateTime.now();
    return Task(
      id: const Uuid().v4(),
      title: title,
      description: description,
      status: TaskStatus.pending,
      priority: priority,
      tags: tags,
      createdBy: createdBy,
      assignedTo: assignedTo,
      createdAt: now,
      updatedAt: now,
      dueDate: dueDate,
    );
  }

  Task copyWith({
    String? title,
    String? description,
    TaskStatus? status,
    TaskPriority? priority,
    List<String>? tags,
    String? assignedTo,
    DateTime? dueDate,
    DateTime? completedAt,
  }) {
    return Task(
      id: id,
      title: title ?? this.title,
      description: description ?? this.description,
      status: status ?? this.status,
      priority: priority ?? this.priority,
      tags: tags ?? this.tags,
      createdBy: createdBy,
      assignedTo: assignedTo ?? this.assignedTo,
      createdAt: createdAt,
      updatedAt: DateTime.now(),
      dueDate: dueDate ?? this.dueDate,
      completedAt: completedAt ?? this.completedAt,
    );
  }

  Map<String, dynamic> toJson() => {
        'id': id,
        'title': title,
        'description': description,
        'status': status.name,
        'priority': priority.name,
        'tags': tags,
        'created_by': createdBy,
        'assigned_to': assignedTo,
        'created_at': createdAt.toIso8601String(),
        'updated_at': updatedAt.toIso8601String(),
        'due_date': dueDate?.toIso8601String(),
        'completed_at': completedAt?.toIso8601String(),
      };

  factory Task.fromJson(Map<String, dynamic> json) {
    return Task(
      id: json['id'] as String,
      title: json['title'] as String,
      description: json['description'] as String? ?? '',
      status: TaskStatus.values.firstWhere(
        (e) => e.name == json['status'],
        orElse: () => TaskStatus.pending,
      ),
      priority: TaskPriority.values.firstWhere(
        (e) => e.name == json['priority'],
        orElse: () => TaskPriority.medium,
      ),
      tags: List<String>.from(json['tags'] ?? []),
      createdBy: json['created_by'] as String? ?? 'system',
      assignedTo: json['assigned_to'] as String?,
      createdAt: DateTime.parse(json['created_at'] as String),
      updatedAt: DateTime.parse(json['updated_at'] as String),
      dueDate: json['due_date'] != null
          ? DateTime.parse(json['due_date'] as String)
          : null,
      completedAt: json['completed_at'] != null
          ? DateTime.parse(json['completed_at'] as String)
          : null,
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

  Map<String, dynamic> toJson() => {
        'tasks': tasks.map((t) => t.toJson()).toList(),
        'next_page_token': nextPageToken,
        'total_count': totalCount,
      };
}

class CreateTaskRequest {
  final String title;
  final String? description;
  final TaskPriority? priority;
  final List<String>? tags;
  final String? assignedTo;
  final DateTime? dueDate;

  CreateTaskRequest({
    required this.title,
    this.description,
    this.priority,
    this.tags,
    this.assignedTo,
    this.dueDate,
  });

  factory CreateTaskRequest.fromJson(Map<String, dynamic> json) {
    return CreateTaskRequest(
      title: json['title'] as String,
      description: json['description'] as String?,
      priority: json['priority'] != null
          ? TaskPriority.values.firstWhere(
              (e) => e.name == json['priority'],
              orElse: () => TaskPriority.medium,
            )
          : null,
      tags: json['tags'] != null ? List<String>.from(json['tags']) : null,
      assignedTo: json['assigned_to'] as String?,
      dueDate: json['due_date'] != null
          ? DateTime.parse(json['due_date'] as String)
          : null,
    );
  }
}

class UpdateTaskRequest {
  final String? title;
  final String? description;
  final TaskStatus? status;
  final TaskPriority? priority;
  final List<String>? tags;
  final String? assignedTo;
  final DateTime? dueDate;

  UpdateTaskRequest({
    this.title,
    this.description,
    this.status,
    this.priority,
    this.tags,
    this.assignedTo,
    this.dueDate,
  });

  factory UpdateTaskRequest.fromJson(Map<String, dynamic> json) {
    return UpdateTaskRequest(
      title: json['title'] as String?,
      description: json['description'] as String?,
      status: json['status'] != null
          ? TaskStatus.values.firstWhere(
              (e) => e.name == json['status'],
              orElse: () => TaskStatus.pending,
            )
          : null,
      priority: json['priority'] != null
          ? TaskPriority.values.firstWhere(
              (e) => e.name == json['priority'],
              orElse: () => TaskPriority.medium,
            )
          : null,
      tags: json['tags'] != null ? List<String>.from(json['tags']) : null,
      assignedTo: json['assigned_to'] as String?,
      dueDate: json['due_date'] != null
          ? DateTime.parse(json['due_date'] as String)
          : null,
    );
  }
}