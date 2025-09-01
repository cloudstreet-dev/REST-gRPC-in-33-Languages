<?php
declare(strict_types=1);

namespace TaskGRPC\Client;

use Tasks\V1\TaskServiceClient;
use Tasks\V1\Task;
use Tasks\V1\TaskStatus;
use Tasks\V1\TaskPriority;
use Tasks\V1\ListTasksRequest;
use Tasks\V1\GetTaskRequest;
use Tasks\V1\CreateTaskRequest;
use Tasks\V1\UpdateTaskRequest;
use Tasks\V1\DeleteTaskRequest;
use Grpc\ChannelCredentials;
use Google\Protobuf\Timestamp;

class TaskGrpcClient
{
    private TaskServiceClient $client;
    
    public function __construct(string $host = 'localhost:50051')
    {
        $this->client = new TaskServiceClient($host, [
            'credentials' => ChannelCredentials::createInsecure()
        ]);
    }
    
    public function listTasks(array $params = []): array
    {
        $request = new ListTasksRequest();
        
        if (isset($params['page_size'])) {
            $request->setPageSize($params['page_size']);
        }
        
        if (isset($params['page_token'])) {
            $request->setPageToken($params['page_token']);
        }
        
        if (isset($params['status'])) {
            $request->setStatus($params['status']);
        }
        
        if (isset($params['assigned_to'])) {
            $request->setAssignedTo($params['assigned_to']);
        }
        
        if (isset($params['tags'])) {
            $request->setTags($params['tags']);
        }
        
        if (isset($params['sort_order'])) {
            $request->setSortOrder($params['sort_order']);
        }
        
        [$response, $status] = $this->client->ListTasks($request)->wait();
        
        if ($status->code !== \Grpc\STATUS_OK) {
            throw new \Exception("gRPC Error: " . $status->details);
        }
        
        $tasks = [];
        foreach ($response->getTasks() as $task) {
            $tasks[] = $this->taskToArray($task);
        }
        
        return [
            'tasks' => $tasks,
            'next_page_token' => $response->getNextPageToken(),
            'total_count' => $response->getTotalCount()
        ];
    }
    
    public function getTask(string $id): array
    {
        $request = new GetTaskRequest();
        $request->setId($id);
        
        [$response, $status] = $this->client->GetTask($request)->wait();
        
        if ($status->code !== \Grpc\STATUS_OK) {
            throw new \Exception("gRPC Error: " . $status->details);
        }
        
        return $this->taskToArray($response);
    }
    
    public function createTask(array $data): array
    {
        $task = new Task();
        $task->setTitle($data['title']);
        
        if (isset($data['description'])) {
            $task->setDescription($data['description']);
        }
        
        if (isset($data['priority'])) {
            $priority = $this->stringToPriority($data['priority']);
            $task->setPriority($priority);
        }
        
        if (isset($data['tags'])) {
            $task->setTags($data['tags']);
        }
        
        if (isset($data['assigned_to'])) {
            $task->setAssignedTo($data['assigned_to']);
        }
        
        $request = new CreateTaskRequest();
        $request->setTask($task);
        
        [$response, $status] = $this->client->CreateTask($request)->wait();
        
        if ($status->code !== \Grpc\STATUS_OK) {
            throw new \Exception("gRPC Error: " . $status->details);
        }
        
        return $this->taskToArray($response);
    }
    
    public function updateTask(string $id, array $data, array $updateMask = []): array
    {
        $task = new Task();
        $task->setId($id);
        
        if (isset($data['title'])) {
            $task->setTitle($data['title']);
            $updateMask[] = 'title';
        }
        
        if (isset($data['description'])) {
            $task->setDescription($data['description']);
            $updateMask[] = 'description';
        }
        
        if (isset($data['status'])) {
            $status = $this->stringToStatus($data['status']);
            $task->setStatus($status);
            $updateMask[] = 'status';
        }
        
        if (isset($data['priority'])) {
            $priority = $this->stringToPriority($data['priority']);
            $task->setPriority($priority);
            $updateMask[] = 'priority';
        }
        
        if (isset($data['tags'])) {
            $task->setTags($data['tags']);
            $updateMask[] = 'tags';
        }
        
        if (isset($data['assigned_to'])) {
            $task->setAssignedTo($data['assigned_to']);
            $updateMask[] = 'assigned_to';
        }
        
        $request = new UpdateTaskRequest();
        $request->setTask($task);
        $request->setUpdateMask($updateMask);
        
        [$response, $status] = $this->client->UpdateTask($request)->wait();
        
        if ($status->code !== \Grpc\STATUS_OK) {
            throw new \Exception("gRPC Error: " . $status->details);
        }
        
        return $this->taskToArray($response);
    }
    
    public function deleteTask(string $id): bool
    {
        $request = new DeleteTaskRequest();
        $request->setId($id);
        
        [$response, $status] = $this->client->DeleteTask($request)->wait();
        
        if ($status->code !== \Grpc\STATUS_OK) {
            throw new \Exception("gRPC Error: " . $status->details);
        }
        
        return true;
    }
    
    private function taskToArray(Task $task): array
    {
        $data = [
            'id' => $task->getId(),
            'title' => $task->getTitle(),
            'description' => $task->getDescription(),
            'status' => $this->statusToString($task->getStatus()),
            'priority' => $this->priorityToString($task->getPriority()),
            'tags' => iterator_to_array($task->getTags()),
            'created_by' => $task->getCreatedBy(),
            'assigned_to' => $task->getAssignedTo()
        ];
        
        if ($task->hasCreatedAt()) {
            $data['created_at'] = date('c', $task->getCreatedAt()->getSeconds());
        }
        
        if ($task->hasUpdatedAt()) {
            $data['updated_at'] = date('c', $task->getUpdatedAt()->getSeconds());
        }
        
        if ($task->hasDueDate()) {
            $data['due_date'] = date('c', $task->getDueDate()->getSeconds());
        }
        
        if ($task->hasCompletedAt()) {
            $data['completed_at'] = date('c', $task->getCompletedAt()->getSeconds());
        }
        
        return $data;
    }
    
    private function statusToString(int $status): string
    {
        return match($status) {
            TaskStatus::TASK_STATUS_PENDING => 'pending',
            TaskStatus::TASK_STATUS_IN_PROGRESS => 'in_progress',
            TaskStatus::TASK_STATUS_COMPLETED => 'completed',
            TaskStatus::TASK_STATUS_CANCELLED => 'cancelled',
            TaskStatus::TASK_STATUS_ON_HOLD => 'on_hold',
            default => 'pending'
        };
    }
    
    private function stringToStatus(string $status): int
    {
        return match($status) {
            'pending' => TaskStatus::TASK_STATUS_PENDING,
            'in_progress' => TaskStatus::TASK_STATUS_IN_PROGRESS,
            'completed' => TaskStatus::TASK_STATUS_COMPLETED,
            'cancelled' => TaskStatus::TASK_STATUS_CANCELLED,
            'on_hold' => TaskStatus::TASK_STATUS_ON_HOLD,
            default => TaskStatus::TASK_STATUS_PENDING
        };
    }
    
    private function priorityToString(int $priority): string
    {
        return match($priority) {
            TaskPriority::TASK_PRIORITY_LOW => 'low',
            TaskPriority::TASK_PRIORITY_MEDIUM => 'medium',
            TaskPriority::TASK_PRIORITY_HIGH => 'high',
            TaskPriority::TASK_PRIORITY_CRITICAL => 'critical',
            default => 'medium'
        };
    }
    
    private function stringToPriority(string $priority): int
    {
        return match($priority) {
            'low' => TaskPriority::TASK_PRIORITY_LOW,
            'medium' => TaskPriority::TASK_PRIORITY_MEDIUM,
            'high' => TaskPriority::TASK_PRIORITY_HIGH,
            'critical' => TaskPriority::TASK_PRIORITY_CRITICAL,
            default => TaskPriority::TASK_PRIORITY_MEDIUM
        };
    }
}