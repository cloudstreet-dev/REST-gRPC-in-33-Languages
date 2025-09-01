<?php
declare(strict_types=1);

namespace TaskGRPC\Server;

use Tasks\V1\Task;
use Tasks\V1\TaskStatus;
use Tasks\V1\TaskPriority;
use Tasks\V1\ListTasksRequest;
use Tasks\V1\ListTasksResponse;
use Tasks\V1\GetTaskRequest;
use Tasks\V1\CreateTaskRequest;
use Tasks\V1\UpdateTaskRequest;
use Tasks\V1\DeleteTaskRequest;
use Tasks\V1\TaskServiceInterface;
use Google\Protobuf\GPBEmpty;
use Google\Protobuf\Timestamp;
use Grpc\ServerContext;
use Ramsey\Uuid\Uuid;

class TaskServiceImpl implements TaskServiceInterface
{
    private array $tasks = [];
    
    public function __construct()
    {
        $this->initializeSampleData();
    }
    
    public function ListTasks(ListTasksRequest $request, ServerContext $context): ListTasksResponse
    {
        $filteredTasks = $this->filterTasks($request);
        $sortedTasks = $this->sortTasks($filteredTasks, $request->getSortOrder());
        
        $pageSize = $request->getPageSize() ?: 20;
        $pageSize = min(max($pageSize, 1), 100);
        
        $startIndex = 0;
        if ($request->getPageToken()) {
            $startIndex = (int)$request->getPageToken();
        }
        
        $tasksPage = array_slice($sortedTasks, $startIndex, $pageSize);
        $nextPageToken = ($startIndex + $pageSize < count($sortedTasks)) 
            ? (string)($startIndex + $pageSize) 
            : '';
        
        $response = new ListTasksResponse();
        $response->setTasks($tasksPage);
        $response->setNextPageToken($nextPageToken);
        $response->setTotalCount(count($sortedTasks));
        
        return $response;
    }
    
    public function GetTask(GetTaskRequest $request, ServerContext $context): Task
    {
        $id = $request->getId();
        
        if (!isset($this->tasks[$id])) {
            $context->setStatus(\Grpc\Status::notFound("Task with ID {$id} not found"));
            return new Task();
        }
        
        return $this->tasks[$id];
    }
    
    public function CreateTask(CreateTaskRequest $request, ServerContext $context): Task
    {
        $task = $request->getTask();
        
        if (!$task) {
            $context->setStatus(\Grpc\Status::invalidArgument("Task data is required"));
            return new Task();
        }
        
        // Generate ID and set timestamps
        $task->setId(Uuid::uuid4()->toString());
        
        $now = new Timestamp();
        $now->setSeconds(time());
        $task->setCreatedAt($now);
        $task->setUpdatedAt(clone $now);
        
        // Set defaults
        if ($task->getStatus() === TaskStatus::TASK_STATUS_UNSPECIFIED) {
            $task->setStatus(TaskStatus::TASK_STATUS_PENDING);
        }
        
        if ($task->getPriority() === TaskPriority::TASK_PRIORITY_UNSPECIFIED) {
            $task->setPriority(TaskPriority::TASK_PRIORITY_MEDIUM);
        }
        
        if (empty($task->getCreatedBy())) {
            $task->setCreatedBy('system');
        }
        
        // Validate
        if (empty($task->getTitle())) {
            $context->setStatus(\Grpc\Status::invalidArgument("Title is required"));
            return new Task();
        }
        
        if (strlen($task->getTitle()) > 200) {
            $context->setStatus(\Grpc\Status::invalidArgument("Title must be 200 characters or less"));
            return new Task();
        }
        
        $this->tasks[$task->getId()] = $task;
        
        return $task;
    }
    
    public function UpdateTask(UpdateTaskRequest $request, ServerContext $context): Task
    {
        $task = $request->getTask();
        $updateMask = $request->getUpdateMask();
        
        if (!$task || empty($task->getId())) {
            $context->setStatus(\Grpc\Status::invalidArgument("Task ID is required"));
            return new Task();
        }
        
        $id = $task->getId();
        
        if (!isset($this->tasks[$id])) {
            $context->setStatus(\Grpc\Status::notFound("Task with ID {$id} not found"));
            return new Task();
        }
        
        $existingTask = $this->tasks[$id];
        
        // Apply updates based on update mask
        if (empty($updateMask)) {
            // Update all fields if no mask provided
            $task->setUpdatedAt($this->getCurrentTimestamp());
            $this->tasks[$id] = $task;
        } else {
            // Selective update based on mask
            foreach ($updateMask as $field) {
                switch ($field) {
                    case 'title':
                        $existingTask->setTitle($task->getTitle());
                        break;
                    case 'description':
                        $existingTask->setDescription($task->getDescription());
                        break;
                    case 'status':
                        $existingTask->setStatus($task->getStatus());
                        if ($existingTask->getStatus() === TaskStatus::TASK_STATUS_COMPLETED 
                            && !$existingTask->hasCompletedAt()) {
                            $existingTask->setCompletedAt($this->getCurrentTimestamp());
                        }
                        break;
                    case 'priority':
                        $existingTask->setPriority($task->getPriority());
                        break;
                    case 'tags':
                        $existingTask->setTags($task->getTags());
                        break;
                    case 'assigned_to':
                        $existingTask->setAssignedTo($task->getAssignedTo());
                        break;
                    case 'due_date':
                        $existingTask->setDueDate($task->getDueDate());
                        break;
                }
            }
            $existingTask->setUpdatedAt($this->getCurrentTimestamp());
        }
        
        return $this->tasks[$id];
    }
    
    public function DeleteTask(DeleteTaskRequest $request, ServerContext $context): GPBEmpty
    {
        $id = $request->getId();
        
        if (!isset($this->tasks[$id])) {
            $context->setStatus(\Grpc\Status::notFound("Task with ID {$id} not found"));
        } else {
            unset($this->tasks[$id]);
        }
        
        return new GPBEmpty();
    }
    
    private function initializeSampleData(): void
    {
        $sampleTasks = [
            [
                'title' => 'Implement PHP gRPC API',
                'description' => 'Create gRPC API with PHP',
                'priority' => TaskPriority::TASK_PRIORITY_HIGH,
                'tags' => ['php', 'grpc', 'api']
            ],
            [
                'title' => 'Add streaming support',
                'description' => 'Implement server streaming for real-time updates',
                'priority' => TaskPriority::TASK_PRIORITY_MEDIUM,
                'tags' => ['php', 'grpc', 'streaming']
            ]
        ];
        
        foreach ($sampleTasks as $taskData) {
            $task = new Task();
            $task->setId(Uuid::uuid4()->toString());
            $task->setTitle($taskData['title']);
            $task->setDescription($taskData['description']);
            $task->setStatus(TaskStatus::TASK_STATUS_PENDING);
            $task->setPriority($taskData['priority']);
            $task->setTags($taskData['tags']);
            $task->setCreatedBy('system');
            $task->setCreatedAt($this->getCurrentTimestamp());
            $task->setUpdatedAt($this->getCurrentTimestamp());
            
            $this->tasks[$task->getId()] = $task;
        }
    }
    
    private function filterTasks(ListTasksRequest $request): array
    {
        $tasks = array_values($this->tasks);
        
        // Filter by status
        if ($request->getStatus() !== TaskStatus::TASK_STATUS_UNSPECIFIED) {
            $tasks = array_filter($tasks, fn($t) => $t->getStatus() === $request->getStatus());
        }
        
        // Filter by assigned_to
        if (!empty($request->getAssignedTo())) {
            $tasks = array_filter($tasks, fn($t) => $t->getAssignedTo() === $request->getAssignedTo());
        }
        
        // Filter by tags
        $requestTags = $request->getTags();
        if (!empty($requestTags)) {
            $tasks = array_filter($tasks, function($t) use ($requestTags) {
                $taskTags = iterator_to_array($t->getTags());
                foreach ($requestTags as $tag) {
                    if (!in_array($tag, $taskTags)) {
                        return false;
                    }
                }
                return true;
            });
        }
        
        return array_values($tasks);
    }
    
    private function sortTasks(array $tasks, int $sortOrder): array
    {
        switch ($sortOrder) {
            case \Tasks\V1\SortOrder::SORT_ORDER_CREATED_AT_ASC:
                usort($tasks, fn($a, $b) => $a->getCreatedAt()->getSeconds() <=> $b->getCreatedAt()->getSeconds());
                break;
            case \Tasks\V1\SortOrder::SORT_ORDER_CREATED_AT_DESC:
                usort($tasks, fn($a, $b) => $b->getCreatedAt()->getSeconds() <=> $a->getCreatedAt()->getSeconds());
                break;
            case \Tasks\V1\SortOrder::SORT_ORDER_PRIORITY_ASC:
                usort($tasks, fn($a, $b) => $this->getPriorityValue($a->getPriority()) <=> $this->getPriorityValue($b->getPriority()));
                break;
            case \Tasks\V1\SortOrder::SORT_ORDER_PRIORITY_DESC:
                usort($tasks, fn($a, $b) => $this->getPriorityValue($b->getPriority()) <=> $this->getPriorityValue($a->getPriority()));
                break;
        }
        
        return $tasks;
    }
    
    private function getPriorityValue(int $priority): int
    {
        return match($priority) {
            TaskPriority::TASK_PRIORITY_LOW => 1,
            TaskPriority::TASK_PRIORITY_MEDIUM => 2,
            TaskPriority::TASK_PRIORITY_HIGH => 3,
            TaskPriority::TASK_PRIORITY_CRITICAL => 4,
            default => 0
        };
    }
    
    private function getCurrentTimestamp(): Timestamp
    {
        $timestamp = new Timestamp();
        $timestamp->setSeconds(time());
        return $timestamp;
    }
}