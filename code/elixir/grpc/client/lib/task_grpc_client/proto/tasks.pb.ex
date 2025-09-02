defmodule Tasks.V1.Task do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :title, 2, type: :string
  field :description, 3, type: :string
  field :status, 4, type: Tasks.V1.TaskStatus, enum: true
  field :priority, 5, type: Tasks.V1.TaskPriority, enum: true
  field :tags, 6, repeated: true, type: :string
  field :created_by, 7, type: :string, json_name: "createdBy"
  field :assigned_to, 8, type: :string, json_name: "assignedTo"
  field :created_at, 9, type: Google.Protobuf.Timestamp, json_name: "createdAt"
  field :updated_at, 10, type: Google.Protobuf.Timestamp, json_name: "updatedAt"
  field :due_date, 11, type: Google.Protobuf.Timestamp, json_name: "dueDate"
  field :completed_at, 12, type: Google.Protobuf.Timestamp, json_name: "completedAt"
end

defmodule Tasks.V1.TaskStatus do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :TASK_STATUS_UNSPECIFIED, 0
  field :TASK_STATUS_PENDING, 1
  field :TASK_STATUS_IN_PROGRESS, 2
  field :TASK_STATUS_COMPLETED, 3
  field :TASK_STATUS_CANCELLED, 4
  field :TASK_STATUS_ON_HOLD, 5
end

defmodule Tasks.V1.TaskPriority do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :TASK_PRIORITY_UNSPECIFIED, 0
  field :TASK_PRIORITY_LOW, 1
  field :TASK_PRIORITY_MEDIUM, 2
  field :TASK_PRIORITY_HIGH, 3
  field :TASK_PRIORITY_CRITICAL, 4
end

defmodule Tasks.V1.ListTasksRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :page_size, 1, type: :int32, json_name: "pageSize"
  field :page_token, 2, type: :string, json_name: "pageToken"
  field :status, 3, type: Tasks.V1.TaskStatus, enum: true
  field :assigned_to, 4, type: :string, json_name: "assignedTo"
  field :tags, 5, repeated: true, type: :string
  field :sort_order, 6, type: Tasks.V1.SortOrder, json_name: "sortOrder", enum: true
end

defmodule Tasks.V1.ListTasksResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :tasks, 1, repeated: true, type: Tasks.V1.Task
  field :next_page_token, 2, type: :string, json_name: "nextPageToken"
  field :total_count, 3, type: :int32, json_name: "totalCount"
end

defmodule Tasks.V1.GetTaskRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
end

defmodule Tasks.V1.CreateTaskRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :task, 1, type: Tasks.V1.Task
end

defmodule Tasks.V1.UpdateTaskRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :task, 1, type: Tasks.V1.Task
  field :update_mask, 2, repeated: true, type: :string, json_name: "updateMask"
end

defmodule Tasks.V1.DeleteTaskRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
end

defmodule Tasks.V1.WatchTasksRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :task_ids, 1, repeated: true, type: :string, json_name: "taskIds"
  field :watch_all, 2, type: :bool, json_name: "watchAll"
  field :assigned_to, 3, type: :string, json_name: "assignedTo"
end

defmodule Tasks.V1.TaskEvent do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :event_type, 1, type: Tasks.V1.EventType, json_name: "eventType", enum: true
  field :task, 2, type: Tasks.V1.Task
  field :timestamp, 3, type: Google.Protobuf.Timestamp
end

defmodule Tasks.V1.EventType do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :EVENT_TYPE_UNSPECIFIED, 0
  field :EVENT_TYPE_CREATED, 1
  field :EVENT_TYPE_UPDATED, 2
  field :EVENT_TYPE_DELETED, 3
  field :EVENT_TYPE_STATUS_CHANGED, 4
end

defmodule Tasks.V1.SortOrder do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :SORT_ORDER_UNSPECIFIED, 0
  field :SORT_ORDER_CREATED_AT_ASC, 1
  field :SORT_ORDER_CREATED_AT_DESC, 2
  field :SORT_ORDER_DUE_DATE_ASC, 3
  field :SORT_ORDER_DUE_DATE_DESC, 4
  field :SORT_ORDER_PRIORITY_ASC, 5
  field :SORT_ORDER_PRIORITY_DESC, 6
end