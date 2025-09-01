<?php
declare(strict_types=1);

namespace TaskAPI\Services;

use TaskAPI\Models\Task;
use TaskAPI\Exceptions\NotFoundException;
use TaskAPI\Exceptions\ValidationException;

class TaskService
{
    private array $tasks = [];
    
    public function __construct()
    {
        $this->initializeSampleData();
    }
    
    public function listTasks(
        int $pageSize = 20,
        ?string $pageToken = null,
        ?string $status = null,
        ?string $assignedTo = null,
        ?string $tags = null,
        ?string $sortOrder = null
    ): array {
        // Filter tasks
        $filteredTasks = array_filter($this->tasks, function(Task $task) use ($status, $assignedTo, $tags) {
            return $task->matchesFilters($status, $assignedTo, $tags);
        });
        
        // Sort tasks
        $sortedTasks = $this->sortTasks(array_values($filteredTasks), $sortOrder);
        
        // Paginate
        $pageSize = min(max($pageSize, 1), 100);
        $startIndex = $pageToken ? (int)$pageToken : 0;
        $endIndex = $startIndex + $pageSize;
        
        $tasksPage = array_slice($sortedTasks, $startIndex, $pageSize);
        $nextPageToken = $endIndex < count($sortedTasks) ? (string)$endIndex : null;
        
        return [
            'tasks' => array_map(fn($task) => $task->jsonSerialize(), $tasksPage),
            'next_page_token' => $nextPageToken,
            'total_count' => count($sortedTasks)
        ];
    }
    
    public function getTask(string $id): Task
    {
        if (!isset($this->tasks[$id])) {
            throw new NotFoundException("Task with ID {$id} not found");
        }
        
        return $this->tasks[$id];
    }
    
    public function createTask(array $data): Task
    {
        $task = Task::fromArray($data);
        
        if (!$task->isValid()) {
            $errors = $task->getValidationErrors();
            throw new ValidationException(implode(', ', $errors));
        }
        
        $this->tasks[$task->getId()] = $task;
        
        return $task;
    }
    
    public function updateTask(string $id, array $data): Task
    {
        if (!isset($this->tasks[$id])) {
            throw new NotFoundException("Task with ID {$id} not found");
        }
        
        $task = $this->tasks[$id];
        $task->update($data);
        
        if (!$task->isValid()) {
            $errors = $task->getValidationErrors();
            throw new ValidationException(implode(', ', $errors));
        }
        
        return $task;
    }
    
    public function updateTaskStatus(string $id, string $status): Task
    {
        return $this->updateTask($id, ['status' => $status]);
    }
    
    public function deleteTask(string $id): void
    {
        if (!isset($this->tasks[$id])) {
            throw new NotFoundException("Task with ID {$id} not found");
        }
        
        unset($this->tasks[$id]);
    }
    
    private function initializeSampleData(): void
    {
        $sampleTasks = [
            [
                'title' => 'Implement PHP REST API',
                'description' => 'Create REST API using Slim Framework',
                'priority' => Task::PRIORITY_HIGH,
                'tags' => ['php', 'rest', 'api']
            ],
            [
                'title' => 'Add gRPC support',
                'description' => 'Implement gRPC server with PHP',
                'priority' => Task::PRIORITY_MEDIUM,
                'tags' => ['php', 'grpc', 'protobuf']
            ],
            [
                'title' => 'Write unit tests',
                'description' => 'Add PHPUnit tests for API endpoints',
                'priority' => Task::PRIORITY_HIGH,
                'tags' => ['php', 'testing', 'phpunit']
            ]
        ];
        
        foreach ($sampleTasks as $taskData) {
            $this->createTask($taskData);
        }
    }
    
    private function sortTasks(array $tasks, ?string $sortOrder): array
    {
        switch ($sortOrder) {
            case 'created_at_asc':
                usort($tasks, fn($a, $b) => strcmp($a->getCreatedAt(), $b->getCreatedAt()));
                break;
            case 'created_at_desc':
                usort($tasks, fn($a, $b) => strcmp($b->getCreatedAt(), $a->getCreatedAt()));
                break;
            case 'due_date_asc':
                usort($tasks, fn($a, $b) => strcmp($a->getDueDate() ?? '9999-12-31', $b->getDueDate() ?? '9999-12-31'));
                break;
            case 'due_date_desc':
                usort($tasks, fn($a, $b) => strcmp($b->getDueDate() ?? '0000-01-01', $a->getDueDate() ?? '0000-01-01'));
                break;
            case 'priority_asc':
                usort($tasks, fn($a, $b) => $a->getPriorityValue() <=> $b->getPriorityValue());
                break;
            case 'priority_desc':
                usort($tasks, fn($a, $b) => $b->getPriorityValue() <=> $a->getPriorityValue());
                break;
        }
        
        return $tasks;
    }
}