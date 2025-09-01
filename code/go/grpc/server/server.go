package server

import (
	"context"
	"fmt"
	"io"
	"log"
	"sync"
	"time"

	"github.com/cloudstreet-dev/REST-gRPC-in-33-Languages/code/go/internal/models"
	"github.com/cloudstreet-dev/REST-gRPC-in-33-Languages/code/go/internal/services"
	"github.com/google/uuid"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
	"google.golang.org/protobuf/types/known/emptypb"
	"google.golang.org/protobuf/types/known/timestamppb"
)

// TaskServiceServer implements the gRPC TaskService
type TaskServiceServer struct {
	UnimplementedTaskServiceServer
	taskService *services.TaskService
	watchers    map[string]chan *TaskEvent
	watchersMu  sync.RWMutex
}

// NewTaskServiceServer creates a new gRPC server
func NewTaskServiceServer(taskService *services.TaskService) *TaskServiceServer {
	return &TaskServiceServer{
		taskService: taskService,
		watchers:    make(map[string]chan *TaskEvent),
	}
}

// ListTasks implements server streaming RPC
func (s *TaskServiceServer) ListTasks(req *ListTasksRequest, stream TaskService_ListTasksServer) error {
	// Convert gRPC request to internal model
	query := models.ListTasksQuery{
		PageSize:   int(req.PageSize),
		PageToken:  req.PageToken,
		AssignedTo: req.AssignedTo,
		SortOrder:  convertSortOrder(req.SortOrder),
	}

	if req.Status != TaskStatus_TASK_STATUS_UNSPECIFIED {
		query.Status = convertTaskStatus(req.Status)
	}

	if len(req.Tags) > 0 {
		query.Tags = strings.Join(req.Tags, ",")
	}

	// Get tasks from service
	response, err := s.taskService.ListTasks(query)
	if err != nil {
		return status.Errorf(codes.Internal, "failed to list tasks: %v", err)
	}

	// Stream tasks to client
	for _, task := range response.Tasks {
		protoTask := convertTaskToProto(&task)
		if err := stream.Send(protoTask); err != nil {
			return err
		}
	}

	return nil
}

// GetTask implements unary RPC
func (s *TaskServiceServer) GetTask(ctx context.Context, req *GetTaskRequest) (*Task, error) {
	task, err := s.taskService.GetTask(req.Id)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			return nil, status.Errorf(codes.NotFound, err.Error())
		}
		return nil, status.Errorf(codes.Internal, "failed to get task: %v", err)
	}

	return convertTaskToProto(task), nil
}

// CreateTask implements unary RPC
func (s *TaskServiceServer) CreateTask(ctx context.Context, req *CreateTaskRequest) (*Task, error) {
	if req.Task == nil {
		return nil, status.Error(codes.InvalidArgument, "task is required")
	}

	createReq := models.CreateTaskRequest{
		Title:       req.Task.Title,
		Description: req.Task.Description,
		Priority:    convertTaskPriority(req.Task.Priority),
		Tags:        req.Task.Tags,
	}

	if req.Task.AssignedTo != "" {
		createReq.AssignedTo = &req.Task.AssignedTo
	}

	if req.Task.DueDate != nil {
		dueDate := req.Task.DueDate.AsTime()
		createReq.DueDate = &dueDate
	}

	task, err := s.taskService.CreateTask(createReq)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "failed to create task: %v", err)
	}

	// Notify watchers
	s.notifyWatchers(&TaskEvent{
		EventType: EventType_EVENT_TYPE_CREATED,
		Task:      convertTaskToProto(task),
		Timestamp: timestamppb.Now(),
	})

	return convertTaskToProto(task), nil
}

// UpdateTask implements unary RPC
func (s *TaskServiceServer) UpdateTask(ctx context.Context, req *UpdateTaskRequest) (*Task, error) {
	if req.Task == nil || req.Task.Id == "" {
		return nil, status.Error(codes.InvalidArgument, "task with id is required")
	}

	updateReq := models.UpdateTaskRequest{}

	// Apply updates based on update mask
	for _, field := range req.UpdateMask {
		switch field {
		case "title":
			updateReq.Title = &req.Task.Title
		case "description":
			updateReq.Description = &req.Task.Description
		case "status":
			status := convertTaskStatus(req.Task.Status)
			updateReq.Status = &status
		case "priority":
			priority := convertTaskPriority(req.Task.Priority)
			updateReq.Priority = &priority
		case "tags":
			updateReq.Tags = req.Task.Tags
		case "assigned_to":
			if req.Task.AssignedTo != "" {
				updateReq.AssignedTo = &req.Task.AssignedTo
			}
		case "due_date":
			if req.Task.DueDate != nil {
				dueDate := req.Task.DueDate.AsTime()
				updateReq.DueDate = &dueDate
			}
		}
	}

	task, err := s.taskService.UpdateTask(req.Task.Id, updateReq)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			return nil, status.Errorf(codes.NotFound, err.Error())
		}
		return nil, status.Errorf(codes.Internal, "failed to update task: %v", err)
	}

	// Notify watchers
	s.notifyWatchers(&TaskEvent{
		EventType: EventType_EVENT_TYPE_UPDATED,
		Task:      convertTaskToProto(task),
		Timestamp: timestamppb.Now(),
	})

	return convertTaskToProto(task), nil
}

// DeleteTask implements unary RPC
func (s *TaskServiceServer) DeleteTask(ctx context.Context, req *DeleteTaskRequest) (*emptypb.Empty, error) {
	// Get task before deletion for notification
	task, _ := s.taskService.GetTask(req.Id)

	err := s.taskService.DeleteTask(req.Id)
	if err != nil {
		if strings.Contains(err.Error(), "not found") {
			return nil, status.Errorf(codes.NotFound, err.Error())
		}
		return nil, status.Errorf(codes.Internal, "failed to delete task: %v", err)
	}

	// Notify watchers if task was found
	if task != nil {
		s.notifyWatchers(&TaskEvent{
			EventType: EventType_EVENT_TYPE_DELETED,
			Task:      convertTaskToProto(task),
			Timestamp: timestamppb.Now(),
		})
	}

	return &emptypb.Empty{}, nil
}

// WatchTasks implements bidirectional streaming RPC
func (s *TaskServiceServer) WatchTasks(stream TaskService_WatchTasksServer) error {
	watcherID := uuid.New().String()
	eventChan := make(chan *TaskEvent, 100)

	// Register watcher
	s.watchersMu.Lock()
	s.watchers[watcherID] = eventChan
	s.watchersMu.Unlock()

	// Cleanup on exit
	defer func() {
		s.watchersMu.Lock()
		delete(s.watchers, watcherID)
		s.watchersMu.Unlock()
		close(eventChan)
	}()

	// Handle incoming requests and outgoing events
	errChan := make(chan error, 1)

	// Goroutine to handle incoming requests
	go func() {
		for {
			req, err := stream.Recv()
			if err == io.EOF {
				return
			}
			if err != nil {
				errChan <- err
				return
			}

			// Process watch request
			if req.WatchAll {
				// Send all current tasks
				response, _ := s.taskService.ListTasks(models.ListTasksQuery{})
				for _, task := range response.Tasks {
					event := &TaskEvent{
						EventType: EventType_EVENT_TYPE_CREATED,
						Task:      convertTaskToProto(&task),
						Timestamp: timestamppb.Now(),
					}
					select {
					case eventChan <- event:
					case <-stream.Context().Done():
						return
					}
				}
			} else if len(req.TaskIds) > 0 {
				// Send specific tasks
				for _, id := range req.TaskIds {
					if task, err := s.taskService.GetTask(id); err == nil {
						event := &TaskEvent{
							EventType: EventType_EVENT_TYPE_CREATED,
							Task:      convertTaskToProto(task),
							Timestamp: timestamppb.Now(),
						}
						select {
						case eventChan <- event:
						case <-stream.Context().Done():
							return
						}
					}
				}
			}
		}
	}()

	// Send events to client
	for {
		select {
		case event := <-eventChan:
			if err := stream.Send(event); err != nil {
				return err
			}
		case err := <-errChan:
			return err
		case <-stream.Context().Done():
			return stream.Context().Err()
		}
	}
}

// notifyWatchers sends an event to all registered watchers
func (s *TaskServiceServer) notifyWatchers(event *TaskEvent) {
	s.watchersMu.RLock()
	defer s.watchersMu.RUnlock()

	for _, ch := range s.watchers {
		select {
		case ch <- event:
		default:
			// Channel is full, skip this event
		}
	}
}

// Conversion functions

func convertTaskToProto(task *models.Task) *Task {
	protoTask := &Task{
		Id:          task.ID,
		Title:       task.Title,
		Description: task.Description,
		Status:      convertTaskStatusToProto(task.Status),
		Priority:    convertTaskPriorityToProto(task.Priority),
		Tags:        task.Tags,
		CreatedBy:   task.CreatedBy,
		CreatedAt:   timestamppb.New(task.CreatedAt),
		UpdatedAt:   timestamppb.New(task.UpdatedAt),
	}

	if task.AssignedTo != nil {
		protoTask.AssignedTo = *task.AssignedTo
	}

	if task.DueDate != nil {
		protoTask.DueDate = timestamppb.New(*task.DueDate)
	}

	if task.CompletedAt != nil {
		protoTask.CompletedAt = timestamppb.New(*task.CompletedAt)
	}

	return protoTask
}

func convertTaskStatus(status TaskStatus) models.TaskStatus {
	switch status {
	case TaskStatus_TASK_STATUS_PENDING:
		return models.TaskStatusPending
	case TaskStatus_TASK_STATUS_IN_PROGRESS:
		return models.TaskStatusInProgress
	case TaskStatus_TASK_STATUS_COMPLETED:
		return models.TaskStatusCompleted
	case TaskStatus_TASK_STATUS_CANCELLED:
		return models.TaskStatusCancelled
	case TaskStatus_TASK_STATUS_ON_HOLD:
		return models.TaskStatusOnHold
	default:
		return models.TaskStatusPending
	}
}

func convertTaskStatusToProto(status models.TaskStatus) TaskStatus {
	switch status {
	case models.TaskStatusPending:
		return TaskStatus_TASK_STATUS_PENDING
	case models.TaskStatusInProgress:
		return TaskStatus_TASK_STATUS_IN_PROGRESS
	case models.TaskStatusCompleted:
		return TaskStatus_TASK_STATUS_COMPLETED
	case models.TaskStatusCancelled:
		return TaskStatus_TASK_STATUS_CANCELLED
	case models.TaskStatusOnHold:
		return TaskStatus_TASK_STATUS_ON_HOLD
	default:
		return TaskStatus_TASK_STATUS_UNSPECIFIED
	}
}

func convertTaskPriority(priority TaskPriority) models.TaskPriority {
	switch priority {
	case TaskPriority_TASK_PRIORITY_LOW:
		return models.TaskPriorityLow
	case TaskPriority_TASK_PRIORITY_MEDIUM:
		return models.TaskPriorityMedium
	case TaskPriority_TASK_PRIORITY_HIGH:
		return models.TaskPriorityHigh
	case TaskPriority_TASK_PRIORITY_CRITICAL:
		return models.TaskPriorityCritical
	default:
		return models.TaskPriorityMedium
	}
}

func convertTaskPriorityToProto(priority models.TaskPriority) TaskPriority {
	switch priority {
	case models.TaskPriorityLow:
		return TaskPriority_TASK_PRIORITY_LOW
	case models.TaskPriorityMedium:
		return TaskPriority_TASK_PRIORITY_MEDIUM
	case models.TaskPriorityHigh:
		return TaskPriority_TASK_PRIORITY_HIGH
	case models.TaskPriorityCritical:
		return TaskPriority_TASK_PRIORITY_CRITICAL
	default:
		return TaskPriority_TASK_PRIORITY_UNSPECIFIED
	}
}

func convertSortOrder(order SortOrder) models.SortOrder {
	switch order {
	case SortOrder_SORT_ORDER_CREATED_AT_ASC:
		return models.SortOrderCreatedAtAsc
	case SortOrder_SORT_ORDER_CREATED_AT_DESC:
		return models.SortOrderCreatedAtDesc
	case SortOrder_SORT_ORDER_DUE_DATE_ASC:
		return models.SortOrderDueDateAsc
	case SortOrder_SORT_ORDER_DUE_DATE_DESC:
		return models.SortOrderDueDateDesc
	case SortOrder_SORT_ORDER_PRIORITY_ASC:
		return models.SortOrderPriorityAsc
	case SortOrder_SORT_ORDER_PRIORITY_DESC:
		return models.SortOrderPriorityDesc
	default:
		return ""
	}
}