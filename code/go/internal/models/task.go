package models

import (
	"time"
)

// TaskStatus represents the current state of a task
type TaskStatus string

const (
	TaskStatusPending    TaskStatus = "pending"
	TaskStatusInProgress TaskStatus = "in_progress"
	TaskStatusCompleted  TaskStatus = "completed"
	TaskStatusCancelled  TaskStatus = "cancelled"
	TaskStatusOnHold     TaskStatus = "on_hold"
)

// TaskPriority represents the priority level of a task
type TaskPriority string

const (
	TaskPriorityLow      TaskPriority = "low"
	TaskPriorityMedium   TaskPriority = "medium"
	TaskPriorityHigh     TaskPriority = "high"
	TaskPriorityCritical TaskPriority = "critical"
)

// SortOrder represents the sort order for task listing
type SortOrder string

const (
	SortOrderCreatedAtAsc  SortOrder = "created_at_asc"
	SortOrderCreatedAtDesc SortOrder = "created_at_desc"
	SortOrderDueDateAsc    SortOrder = "due_date_asc"
	SortOrderDueDateDesc   SortOrder = "due_date_desc"
	SortOrderPriorityAsc   SortOrder = "priority_asc"
	SortOrderPriorityDesc  SortOrder = "priority_desc"
)

// Task represents a task in the system
type Task struct {
	ID          string       `json:"id"`
	Title       string       `json:"title" binding:"required,min=1,max=200"`
	Description string       `json:"description"`
	Status      TaskStatus   `json:"status"`
	Priority    TaskPriority `json:"priority"`
	Tags        []string     `json:"tags"`
	CreatedBy   string       `json:"created_by"`
	AssignedTo  *string      `json:"assigned_to,omitempty"`
	CreatedAt   time.Time    `json:"created_at"`
	UpdatedAt   time.Time    `json:"updated_at"`
	DueDate     *time.Time   `json:"due_date,omitempty"`
	CompletedAt *time.Time   `json:"completed_at,omitempty"`
}

// CreateTaskRequest represents a request to create a new task
type CreateTaskRequest struct {
	Title       string       `json:"title" binding:"required,min=1,max=200"`
	Description string       `json:"description"`
	Priority    TaskPriority `json:"priority"`
	Tags        []string     `json:"tags"`
	AssignedTo  *string      `json:"assigned_to,omitempty"`
	DueDate     *time.Time   `json:"due_date,omitempty"`
}

// UpdateTaskRequest represents a request to update an existing task
type UpdateTaskRequest struct {
	Title       *string       `json:"title,omitempty" binding:"omitempty,min=1,max=200"`
	Description *string       `json:"description,omitempty"`
	Status      *TaskStatus   `json:"status,omitempty"`
	Priority    *TaskPriority `json:"priority,omitempty"`
	Tags        []string      `json:"tags,omitempty"`
	AssignedTo  *string       `json:"assigned_to,omitempty"`
	DueDate     *time.Time    `json:"due_date,omitempty"`
}

// UpdateTaskStatusRequest represents a request to update task status
type UpdateTaskStatusRequest struct {
	Status TaskStatus `json:"status" binding:"required"`
}

// ListTasksQuery represents query parameters for listing tasks
type ListTasksQuery struct {
	PageSize   int        `form:"page_size,default=20" binding:"min=1,max=100"`
	PageToken  string     `form:"page_token"`
	Status     TaskStatus `form:"status"`
	AssignedTo string     `form:"assigned_to"`
	Tags       string     `form:"tags"`
	SortOrder  SortOrder  `form:"sort_order"`
}

// ListTasksResponse represents the response for listing tasks
type ListTasksResponse struct {
	Tasks         []Task `json:"tasks"`
	NextPageToken string `json:"next_page_token,omitempty"`
	TotalCount    int    `json:"total_count"`
}

// ErrorResponse represents an error response
type ErrorResponse struct {
	Error ErrorDetail `json:"error"`
}

// ErrorDetail contains error details
type ErrorDetail struct {
	Code    string      `json:"code"`
	Message string      `json:"message"`
	Details interface{} `json:"details,omitempty"`
}

// Validate validates the task status
func (s TaskStatus) IsValid() bool {
	switch s {
	case TaskStatusPending, TaskStatusInProgress, TaskStatusCompleted,
		TaskStatusCancelled, TaskStatusOnHold:
		return true
	default:
		return false
	}
}

// Validate validates the task priority
func (p TaskPriority) IsValid() bool {
	switch p {
	case TaskPriorityLow, TaskPriorityMedium, TaskPriorityHigh, TaskPriorityCritical:
		return true
	default:
		return false
	}
}

// GetPriorityValue returns a numeric value for priority sorting
func (p TaskPriority) GetValue() int {
	switch p {
	case TaskPriorityLow:
		return 1
	case TaskPriorityMedium:
		return 2
	case TaskPriorityHigh:
		return 3
	case TaskPriorityCritical:
		return 4
	default:
		return 2
	}
}