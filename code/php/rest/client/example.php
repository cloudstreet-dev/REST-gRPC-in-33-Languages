<?php
declare(strict_types=1);

require_once __DIR__ . '/vendor/autoload.php';

use TaskAPI\Client\TaskAPIClient;
use TaskAPI\Client\APIException;

$client = new TaskAPIClient();

try {
    // Create a task
    echo "Creating task...\n";
    $task = $client->createTask([
        'title' => 'Test PHP Client',
        'description' => 'Testing the PHP REST client',
        'priority' => 'high',
        'tags' => ['test', 'php']
    ]);
    $taskId = $task['id'];
    echo "Created task: {$taskId}\n";
    
    // List tasks
    echo "\nListing tasks...\n";
    $result = $client->listTasks([
        'page_size' => 10,
        'sort_order' => 'priority_desc'
    ]);
    
    foreach ($result['tasks'] as $t) {
        $status = strtoupper($t['status']);
        $priority = strtoupper($t['priority']);
        echo "[{$status}] {$t['title']} (Priority: {$priority})\n";
        
        if (!empty($t['description'])) {
            echo "  Description: {$t['description']}\n";
        }
        
        if (!empty($t['tags'])) {
            echo "  Tags: " . implode(', ', $t['tags']) . "\n";
        }
        
        echo "\n";
    }
    
    // Update task status
    echo "Updating task status...\n";
    $updated = $client->updateTaskStatus($taskId, 'in_progress');
    echo "Task status updated to: {$updated['status']}\n";
    
    // Get single task
    echo "\nFetching task details...\n";
    $fetched = $client->getTask($taskId);
    echo "Task: {$fetched['title']} - Status: {$fetched['status']}\n";
    
    // Delete task
    echo "\nDeleting task...\n";
    if ($client->deleteTask($taskId)) {
        echo "Task deleted successfully\n";
    }
    
} catch (APIException $e) {
    echo "Error: " . $e->getMessage() . "\n";
}