# Chapter 7: PHP - The Web's Native Language Evolves

PHP, originally created by Rasmus Lerdorf in 1994 as "Personal Home Page Tools," has evolved into one of the world's most widely-used programming languages for web development. Standing now for "PHP: Hypertext Preprocessor," this recursive backronym reflects the language's journey from simple scripting to powering over 75% of websites with known server-side programming languages. In this chapter, we'll explore how modern PHP, particularly PHP 8+, excels at building both REST and gRPC APIs, leveraging its mature ecosystem and continuous improvements.

## Why PHP for APIs?

Despite facing criticism over the years, PHP has transformed into a modern, performant language with compelling advantages for API development:

1. **Ubiquitous Deployment**: PHP runs virtually everywhere - from shared hosting to cloud platforms, making deployment straightforward and cost-effective.

2. **Mature Ecosystem**: Composer, PHP's package manager, provides access to over 300,000 packages, with mature frameworks like Laravel, Symfony, and Slim.

3. **Performance Evolution**: PHP 8+ delivers 2-3x performance improvements over PHP 5, with JIT compilation, preloading, and optimized opcache.

4. **Type Safety**: Modern PHP supports strict typing, union types, attributes, and enums, enabling robust, maintainable code.

5. **Developer Productivity**: PHP's pragmatic design, excellent documentation, and vast community resources enable rapid development.

6. **Enterprise Ready**: Companies like Facebook, Wikipedia, WordPress.com, and Slack rely on PHP at massive scale.

## PHP's Modern Renaissance

PHP 8 represents a watershed moment for the language:

- **JIT Compilation**: Just-In-Time compilation provides significant performance gains for CPU-intensive operations
- **Union Types**: Express complex type constraints naturally
- **Attributes**: Native annotations replace docblock annotations
- **Named Arguments**: Improve code readability and API design
- **Match Expressions**: More powerful and concise than switch statements
- **Nullsafe Operator**: Simplify null checking chains
- **Constructor Property Promotion**: Reduce boilerplate in class definitions

## Setting Up Modern PHP

### macOS

```bash
# Using Homebrew
brew install php@8.3
brew link php@8.3

# Install Composer
brew install composer

# Or download directly
curl -sS https://getcomposer.org/installer | php
mv composer.phar /usr/local/bin/composer
```

### Linux

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install php8.3 php8.3-cli php8.3-common php8.3-curl php8.3-mbstring php8.3-xml php8.3-zip

# Install Composer
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer

# For gRPC support
sudo pecl install grpc
sudo pecl install protobuf
```

### Windows

```powershell
# Using Chocolatey
choco install php
choco install composer

# Or download from
# PHP: https://windows.php.net/download/
# Composer: https://getcomposer.org/Composer-Setup.exe
```

### Docker Setup

```dockerfile
FROM php:8.3-cli

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    zip \
    unzip \
    libzip-dev

# Install PHP extensions
RUN docker-php-ext-install zip

# Install Composer
COPY --from=composer:latest /usr/bin/composer /usr/bin/composer

# Install gRPC extension
RUN pecl install grpc && docker-php-ext-enable grpc

WORKDIR /app
```

### Verifying Installation

```bash
php --version
# PHP 8.3.0 (cli) (built: Nov 23 2023 09:53:23) (NTS)

composer --version
# Composer version 2.6.5 2023-10-06 10:11:52

php -m | grep grpc
# grpc
```

## REST API with Slim Framework

Slim is a PHP microframework that helps you quickly write simple yet powerful web applications and APIs. It's perfect for microservices and API-focused applications.

### Project Structure

```
code/php/rest/
├── server/
│   ├── composer.json
│   ├── public/
│   │   └── index.php
│   └── src/
│       ├── Models/
│       │   └── Task.php
│       ├── Services/
│       │   └── TaskService.php
│       └── Exceptions/
│           ├── NotFoundException.php
│           └── ValidationException.php
└── client/
    ├── composer.json
    └── src/
        └── TaskAPIClient.php
```

### Installing Dependencies

```bash
cd code/php/rest/server
composer install
```

### Implementing the Task Model

Modern PHP's type system shines in model definitions:

```php
<?php
declare(strict_types=1);

namespace TaskAPI\Models;

use JsonSerializable;
use Ramsey\Uuid\Uuid;

class Task implements JsonSerializable
{
    public const STATUS_PENDING = 'pending';
    public const STATUS_IN_PROGRESS = 'in_progress';
    public const STATUS_COMPLETED = 'completed';
    public const STATUS_CANCELLED = 'cancelled';
    public const STATUS_ON_HOLD = 'on_hold';
    
    public const PRIORITY_LOW = 'low';
    public const PRIORITY_MEDIUM = 'medium';
    public const PRIORITY_HIGH = 'high';
    public const PRIORITY_CRITICAL = 'critical';
    
    public function __construct(
        private string $id,
        private string $title,
        private string $description = '',
        private string $status = self::STATUS_PENDING,
        private string $priority = self::PRIORITY_MEDIUM,
        private array $tags = [],
        private string $createdBy = 'system',
        private ?string $assignedTo = null,
        private string $createdAt = '',
        private string $updatedAt = '',
        private ?string $dueDate = null,
        private ?string $completedAt = null
    ) {
        $this->id = $id ?: Uuid::uuid4()->toString();
        $this->createdAt = $createdAt ?: date('c');
        $this->updatedAt = $updatedAt ?: date('c');
    }
    
    public static function fromArray(array $data): self
    {
        return new self(
            id: $data['id'] ?? '',
            title: $data['title'],
            description: $data['description'] ?? '',
            status: $data['status'] ?? self::STATUS_PENDING,
            priority: $data['priority'] ?? self::PRIORITY_MEDIUM,
            tags: $data['tags'] ?? [],
            createdBy: $data['created_by'] ?? 'system',
            assignedTo: $data['assigned_to'] ?? null,
            dueDate: $data['due_date'] ?? null
        );
    }
    
    public function isValid(): bool
    {
        if (empty($this->title) || strlen($this->title) > 200) {
            return false;
        }
        
        if (!in_array($this->status, self::getValidStatuses())) {
            return false;
        }
        
        if (!in_array($this->priority, self::getValidPriorities())) {
            return false;
        }
        
        return true;
    }
    
    public function getValidationErrors(): array
    {
        $errors = [];
        
        if (empty($this->title)) {
            $errors[] = 'Title is required';
        } elseif (strlen($this->title) > 200) {
            $errors[] = 'Title must be 200 characters or less';
        }
        
        if (!in_array($this->status, self::getValidStatuses())) {
            $errors[] = "Invalid status: {$this->status}";
        }
        
        if (!in_array($this->priority, self::getValidPriorities())) {
            $errors[] = "Invalid priority: {$this->priority}";
        }
        
        return $errors;
    }
    
    public function update(array $data): void
    {
        if (isset($data['title'])) {
            $this->title = $data['title'];
        }
        
        if (isset($data['description'])) {
            $this->description = $data['description'];
        }
        
        if (isset($data['status'])) {
            $this->status = $data['status'];
            if ($this->status === self::STATUS_COMPLETED && $this->completedAt === null) {
                $this->completedAt = date('c');
            }
        }
        
        if (isset($data['priority'])) {
            $this->priority = $data['priority'];
        }
        
        if (isset($data['tags'])) {
            $this->tags = $data['tags'];
        }
        
        if (array_key_exists('assigned_to', $data)) {
            $this->assignedTo = $data['assigned_to'];
        }
        
        if (array_key_exists('due_date', $data)) {
            $this->dueDate = $data['due_date'];
        }
        
        $this->updatedAt = date('c');
    }
    
    public function matchesFilters(?string $status = null, ?string $assignedTo = null, ?string $tags = null): bool
    {
        if ($status !== null && $this->status !== $status) {
            return false;
        }
        
        if ($assignedTo !== null && $this->assignedTo !== $assignedTo) {
            return false;
        }
        
        if ($tags !== null) {
            $requiredTags = array_map('trim', explode(',', $tags));
            foreach ($requiredTags as $tag) {
                if (!in_array($tag, $this->tags)) {
                    return false;
                }
            }
        }
        
        return true;
    }
    
    public function getPriorityValue(): int
    {
        return match($this->priority) {
            self::PRIORITY_LOW => 1,
            self::PRIORITY_MEDIUM => 2,
            self::PRIORITY_HIGH => 3,
            self::PRIORITY_CRITICAL => 4,
            default => 2
        };
    }
    
    public static function getValidStatuses(): array
    {
        return [
            self::STATUS_PENDING,
            self::STATUS_IN_PROGRESS,
            self::STATUS_COMPLETED,
            self::STATUS_CANCELLED,
            self::STATUS_ON_HOLD
        ];
    }
    
    public static function getValidPriorities(): array
    {
        return [
            self::PRIORITY_LOW,
            self::PRIORITY_MEDIUM,
            self::PRIORITY_HIGH,
            self::PRIORITY_CRITICAL
        ];
    }
    
    public function jsonSerialize(): mixed
    {
        $data = [
            'id' => $this->id,
            'title' => $this->title,
            'description' => $this->description,
            'status' => $this->status,
            'priority' => $this->priority,
            'tags' => $this->tags,
            'created_by' => $this->createdBy,
            'created_at' => $this->createdAt,
            'updated_at' => $this->updatedAt
        ];
        
        if ($this->assignedTo !== null) {
            $data['assigned_to'] = $this->assignedTo;
        }
        
        if ($this->dueDate !== null) {
            $data['due_date'] = $this->dueDate;
        }
        
        if ($this->completedAt !== null) {
            $data['completed_at'] = $this->completedAt;
        }
        
        return $data;
    }
    
    // Getters
    public function getId(): string { return $this->id; }
    public function getTitle(): string { return $this->title; }
    public function getStatus(): string { return $this->status; }
    public function getPriority(): string { return $this->priority; }
    public function getCreatedAt(): string { return $this->createdAt; }
    public function getDueDate(): ?string { return $this->dueDate; }
}
```

### Creating the Service Layer

```php
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
    
    public function deleteTask(string $id): void
    {
        if (!isset($this->tasks[$id])) {
            throw new NotFoundException("Task with ID {$id} not found");
        }
        
        unset($this->tasks[$id]);
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
}
```

### Building the Slim Application

```php
<?php
declare(strict_types=1);

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;
use TaskAPI\Services\TaskService;
use TaskAPI\Exceptions\NotFoundException;
use TaskAPI\Exceptions\ValidationException;

require __DIR__ . '/../vendor/autoload.php';

// Create app
$app = AppFactory::create();

// Add error middleware
$app->addErrorMiddleware(true, true, true);

// Initialize service
$taskService = new TaskService();

// CORS middleware
$app->add(function (Request $request, $handler) {
    $response = $handler->handle($request);
    return $response
        ->withHeader('Access-Control-Allow-Origin', '*')
        ->withHeader('Access-Control-Allow-Headers', 'X-Requested-With, Content-Type, Accept, Origin, Authorization, X-API-Key')
        ->withHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, PATCH, DELETE, OPTIONS')
        ->withHeader('Content-Type', 'application/json');
});

// Handle preflight requests
$app->options('/{routes:.+}', function (Request $request, Response $response) {
    return $response;
});

// Health check
$app->get('/health', function (Request $request, Response $response) {
    $data = [
        'status' => 'healthy',
        'timestamp' => date('c'),
        'service' => 'task-api-php',
        'version' => '1.0.0'
    ];
    
    $response->getBody()->write(json_encode($data));
    return $response;
});

// List tasks
$app->get('/api/v1/tasks', function (Request $request, Response $response) use ($taskService) {
    $params = $request->getQueryParams();
    
    try {
        $result = $taskService->listTasks(
            (int)($params['page_size'] ?? 20),
            $params['page_token'] ?? null,
            $params['status'] ?? null,
            $params['assigned_to'] ?? null,
            $params['tags'] ?? null,
            $params['sort_order'] ?? null
        );
        
        $response->getBody()->write(json_encode($result));
        return $response;
    } catch (Exception $e) {
        $error = ['error' => ['code' => 'INTERNAL_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500);
    }
});

// Get task by ID
$app->get('/api/v1/tasks/{id}', function (Request $request, Response $response, array $args) use ($taskService) {
    try {
        $task = $taskService->getTask($args['id']);
        $response->getBody()->write(json_encode($task));
        return $response;
    } catch (NotFoundException $e) {
        $error = ['error' => ['code' => 'NOT_FOUND', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(404);
    }
});

// Create task
$app->post('/api/v1/tasks', function (Request $request, Response $response) use ($taskService) {
    try {
        $data = json_decode((string)$request->getBody(), true);
        
        if ($data === null) {
            $error = ['error' => ['code' => 'INVALID_JSON', 'message' => 'Invalid JSON in request body']];
            $response->getBody()->write(json_encode($error));
            return $response->withStatus(400);
        }
        
        $task = $taskService->createTask($data);
        
        $response->getBody()->write(json_encode($task));
        return $response
            ->withStatus(201)
            ->withHeader('Location', '/api/v1/tasks/' . $task->getId());
    } catch (ValidationException $e) {
        $error = ['error' => ['code' => 'VALIDATION_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(400);
    }
});

// Update task
$app->put('/api/v1/tasks/{id}', function (Request $request, Response $response, array $args) use ($taskService) {
    try {
        $data = json_decode((string)$request->getBody(), true);
        
        $task = $taskService->updateTask($args['id'], $data);
        
        $response->getBody()->write(json_encode($task));
        return $response;
    } catch (NotFoundException $e) {
        $error = ['error' => ['code' => 'NOT_FOUND', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(404);
    } catch (ValidationException $e) {
        $error = ['error' => ['code' => 'VALIDATION_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(400);
    }
});

// Delete task
$app->delete('/api/v1/tasks/{id}', function (Request $request, Response $response, array $args) use ($taskService) {
    try {
        $taskService->deleteTask($args['id']);
        return $response->withStatus(204);
    } catch (NotFoundException $e) {
        $error = ['error' => ['code' => 'NOT_FOUND', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(404);
    }
});

$app->run();
```

### Running the Server

```bash
# Development server
php -S localhost:8080 -t public

# Or using Composer script
composer start

# Production with PHP-FPM and Nginx
# Configure nginx to proxy to PHP-FPM
```

## PHP REST Client

Building a robust HTTP client using Guzzle:

```php
<?php
declare(strict_types=1);

namespace TaskAPI\Client;

use GuzzleHttp\Client;
use GuzzleHttp\Exception\ClientException;
use GuzzleHttp\Exception\GuzzleException;

class TaskAPIClient
{
    private Client $httpClient;
    private string $baseUrl;
    private array $headers;
    
    public function __construct(string $baseUrl = 'http://localhost:8080/api/v1')
    {
        $this->baseUrl = rtrim($baseUrl, '/');
        $this->headers = [
            'Content-Type' => 'application/json',
            'Accept' => 'application/json'
        ];
        
        $this->httpClient = new Client([
            'base_uri' => $this->baseUrl,
            'timeout' => 30.0,
            'headers' => $this->headers
        ]);
    }
    
    public function setAuthToken(string $token): void
    {
        $this->headers['Authorization'] = "Bearer {$token}";
        $this->updateHttpClient();
    }
    
    public function setApiKey(string $apiKey): void
    {
        $this->headers['X-API-Key'] = $apiKey;
        $this->updateHttpClient();
    }
    
    public function listTasks(array $params = []): array
    {
        try {
            $response = $this->httpClient->get('/tasks', [
                'query' => $params
            ]);
            
            return json_decode($response->getBody()->getContents(), true);
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        }
    }
    
    public function getTask(string $id): array
    {
        try {
            $response = $this->httpClient->get("/tasks/{$id}");
            return json_decode($response->getBody()->getContents(), true);
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        }
    }
    
    public function createTask(array $data): array
    {
        try {
            $response = $this->httpClient->post('/tasks', [
                'json' => $data
            ]);
            
            return json_decode($response->getBody()->getContents(), true);
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        }
    }
    
    public function updateTask(string $id, array $data): array
    {
        try {
            $response = $this->httpClient->put("/tasks/{$id}", [
                'json' => $data
            ]);
            
            return json_decode($response->getBody()->getContents(), true);
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        }
    }
    
    public function deleteTask(string $id): bool
    {
        try {
            $response = $this->httpClient->delete("/tasks/{$id}");
            return $response->getStatusCode() === 204;
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        }
    }
    
    private function handleClientException(ClientException $e): APIException
    {
        $response = $e->getResponse();
        $statusCode = $response->getStatusCode();
        $body = $response->getBody()->getContents();
        
        try {
            $error = json_decode($body, true);
            $message = $error['error']['message'] ?? 'Unknown error';
        } catch (\Exception $parseError) {
            $message = "HTTP {$statusCode} error";
        }
        
        return match($statusCode) {
            400 => new ValidationException("Validation failed: {$message}"),
            401 => new AuthenticationException("Authentication failed"),
            403 => new AuthorizationException("Access denied"),
            404 => new NotFoundException("Resource not found: {$message}"),
            500 => new ServerException("Server error: {$message}"),
            default => new APIException("Unexpected response: {$statusCode}")
        };
    }
}
```

### Using the Client

```php
$client = new TaskAPIClient();

// Create a task
$task = $client->createTask([
    'title' => 'Learn PHP 8 features',
    'description' => 'Explore new features in PHP 8',
    'priority' => 'high',
    'tags' => ['php', 'learning', 'php8']
]);

echo "Created task: {$task['id']}\n";

// List tasks with filtering
$result = $client->listTasks([
    'status' => 'pending',
    'sort_order' => 'priority_desc',
    'page_size' => 10
]);

foreach ($result['tasks'] as $task) {
    $status = strtoupper($task['status']);
    $priority = strtoupper($task['priority']);
    echo "[{$status}] {$task['title']} (Priority: {$priority})\n";
}

// Update task
$updated = $client->updateTask($task['id'], [
    'status' => 'in_progress',
    'assigned_to' => 'php-developer'
]);

// Delete task
$client->deleteTask($task['id']);
```

## gRPC in PHP

PHP has mature gRPC support through the official gRPC PHP extension and Google's Protocol Buffer implementation.

### Installing gRPC Extension

```bash
# Install via PECL
pecl install grpc
pecl install protobuf

# Add to php.ini
echo "extension=grpc.so" >> /usr/local/etc/php/php.ini
echo "extension=protobuf.so" >> /usr/local/etc/php/php.ini

# Verify installation
php -m | grep -E "grpc|protobuf"
```

### Generating PHP Code from Proto Files

```bash
# Install protoc compiler
brew install protobuf  # macOS
apt-get install protobuf-compiler  # Linux

# Install gRPC PHP plugin
git clone -b v1.57.0 https://github.com/grpc/grpc
cd grpc
make grpc_php_plugin

# Generate PHP code
protoc \
  --proto_path=../../shared/protos \
  --php_out=./generated \
  --grpc_out=./generated \
  --plugin=protoc-gen-grpc=/usr/local/bin/grpc_php_plugin \
  ../../shared/protos/tasks.proto
```

### gRPC Server Implementation

PHP gRPC servers typically use a hybrid approach with a Go or C++ proxy:

```php
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
    
    private function getCurrentTimestamp(): Timestamp
    {
        $timestamp = new Timestamp();
        $timestamp->setSeconds(time());
        return $timestamp;
    }
}
```

### gRPC Client Implementation

```php
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
            $request->setStatus($this->stringToStatus($params['status']));
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
        return [
            'id' => $task->getId(),
            'title' => $task->getTitle(),
            'description' => $task->getDescription(),
            'status' => $this->statusToString($task->getStatus()),
            'priority' => $this->priorityToString($task->getPriority()),
            'tags' => iterator_to_array($task->getTags()),
            'created_by' => $task->getCreatedBy(),
            'assigned_to' => $task->getAssignedTo(),
            'created_at' => $task->hasCreatedAt() ? date('c', $task->getCreatedAt()->getSeconds()) : null,
            'updated_at' => $task->hasUpdatedAt() ? date('c', $task->getUpdatedAt()->getSeconds()) : null,
            'due_date' => $task->hasDueDate() ? date('c', $task->getDueDate()->getSeconds()) : null,
            'completed_at' => $task->hasCompletedAt() ? date('c', $task->getCompletedAt()->getSeconds()) : null
        ];
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
```

## Testing PHP APIs

### PHPUnit for REST API Testing

```php
<?php
declare(strict_types=1);

namespace TaskAPI\Tests;

use PHPUnit\Framework\TestCase;
use TaskAPI\Client\TaskAPIClient;

class TaskAPITest extends TestCase
{
    private TaskAPIClient $client;
    
    protected function setUp(): void
    {
        $this->client = new TaskAPIClient('http://localhost:8080/api/v1');
    }
    
    public function testCreateTask(): void
    {
        $task = $this->client->createTask([
            'title' => 'Test Task',
            'description' => 'Test Description',
            'priority' => 'high'
        ]);
        
        $this->assertArrayHasKey('id', $task);
        $this->assertEquals('Test Task', $task['title']);
        $this->assertEquals('pending', $task['status']);
        $this->assertEquals('high', $task['priority']);
    }
    
    public function testListTasks(): void
    {
        $result = $this->client->listTasks([
            'page_size' => 10,
            'status' => 'pending'
        ]);
        
        $this->assertArrayHasKey('tasks', $result);
        $this->assertArrayHasKey('total_count', $result);
        
        foreach ($result['tasks'] as $task) {
            $this->assertEquals('pending', $task['status']);
        }
    }
    
    public function testUpdateTask(): void
    {
        // Create a task first
        $task = $this->client->createTask([
            'title' => 'Update Test'
        ]);
        
        // Update it
        $updated = $this->client->updateTask($task['id'], [
            'status' => 'in_progress',
            'priority' => 'critical'
        ]);
        
        $this->assertEquals('in_progress', $updated['status']);
        $this->assertEquals('critical', $updated['priority']);
    }
    
    public function testDeleteTask(): void
    {
        // Create a task first
        $task = $this->client->createTask([
            'title' => 'Delete Test'
        ]);
        
        // Delete it
        $result = $this->client->deleteTask($task['id']);
        $this->assertTrue($result);
        
        // Verify it's gone
        $this->expectException(NotFoundException::class);
        $this->client->getTask($task['id']);
    }
}
```

### Integration Testing

```php
<?php
declare(strict_types=1);

namespace TaskAPI\Tests\Integration;

use PHPUnit\Framework\TestCase;
use GuzzleHttp\Client;

class APIIntegrationTest extends TestCase
{
    private Client $httpClient;
    
    protected function setUp(): void
    {
        $this->httpClient = new Client([
            'base_uri' => 'http://localhost:8080',
            'http_errors' => false
        ]);
    }
    
    public function testHealthEndpoint(): void
    {
        $response = $this->httpClient->get('/health');
        
        $this->assertEquals(200, $response->getStatusCode());
        
        $body = json_decode($response->getBody()->getContents(), true);
        $this->assertEquals('healthy', $body['status']);
        $this->assertEquals('task-api-php', $body['service']);
    }
    
    public function testCORSHeaders(): void
    {
        $response = $this->httpClient->options('/api/v1/tasks');
        
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertTrue($response->hasHeader('Access-Control-Allow-Origin'));
        $this->assertTrue($response->hasHeader('Access-Control-Allow-Methods'));
    }
    
    public function testRateLimiting(): void
    {
        // Make many requests quickly
        $responses = [];
        for ($i = 0; $i < 100; $i++) {
            $responses[] = $this->httpClient->get('/api/v1/tasks');
        }
        
        // Check if rate limiting kicked in
        $rateLimited = array_filter($responses, fn($r) => $r->getStatusCode() === 429);
        
        // Depending on your rate limiting configuration
        $this->assertGreaterThan(0, count($rateLimited));
    }
}
```

## Performance Optimization

### OPcache Configuration

```ini
; php.ini optimizations
opcache.enable=1
opcache.memory_consumption=256
opcache.interned_strings_buffer=16
opcache.max_accelerated_files=10000
opcache.revalidate_freq=2
opcache.fast_shutdown=1
opcache.enable_cli=1

; JIT compilation (PHP 8+)
opcache.jit_buffer_size=100M
opcache.jit=tracing
```

### Preloading (PHP 7.4+)

```php
// preload.php
<?php
declare(strict_types=1);

// Preload framework files
require_once __DIR__ . '/vendor/autoload.php';

// Preload application files
$files = glob(__DIR__ . '/src/**/*.php');
foreach ($files as $file) {
    opcache_compile_file($file);
}
```

### Async Processing with ReactPHP

```php
use React\EventLoop\Loop;
use React\Http\HttpServer;
use React\Http\Message\Response;
use React\Http\Middleware\StreamingRequestMiddleware;

$loop = Loop::get();

$server = new HttpServer(
    $loop,
    new StreamingRequestMiddleware(),
    function (ServerRequestInterface $request) use ($taskService) {
        $path = $request->getUri()->getPath();
        $method = $request->getMethod();
        
        if ($path === '/api/v1/tasks' && $method === 'GET') {
            $tasks = $taskService->listTasks();
            return Response::json($tasks);
        }
        
        return Response::plaintext("Not Found")->withStatus(404);
    }
);

$socket = new \React\Socket\SocketServer('127.0.0.1:8080', [], $loop);
$server->listen($socket);

echo "Server running at http://127.0.0.1:8080\n";
$loop->run();
```

## Production Deployment

### PHP-FPM Configuration

```ini
; www.conf
[www]
pm = dynamic
pm.max_children = 50
pm.start_servers = 5
pm.min_spare_servers = 5
pm.max_spare_servers = 35
pm.max_requests = 500

; Enable status page
pm.status_path = /status

; Slow log
slowlog = /var/log/php-fpm/www-slow.log
request_slowlog_timeout = 5s
```

### Nginx Configuration

```nginx
server {
    listen 80;
    server_name api.example.com;
    root /var/www/api/public;
    index index.php;

    location / {
        try_files $uri /index.php$is_args$args;
    }

    location ~ ^/index\.php(/|$) {
        fastcgi_pass unix:/var/run/php/php8.3-fpm.sock;
        fastcgi_split_path_info ^(.+\.php)(/.*)$;
        include fastcgi_params;
        fastcgi_param SCRIPT_FILENAME $realpath_root$fastcgi_script_name;
        fastcgi_param DOCUMENT_ROOT $realpath_root;
        internal;
    }

    location ~ \.php$ {
        return 404;
    }

    # Cache static assets
    location ~* \.(jpg|jpeg|png|gif|ico|css|js)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
}
```

### Docker Deployment

```dockerfile
FROM php:8.3-fpm-alpine

# Install system dependencies
RUN apk add --no-cache \
    git \
    zip \
    unzip \
    icu-dev

# Install PHP extensions
RUN docker-php-ext-install \
    pdo_mysql \
    opcache \
    intl

# Install Composer
COPY --from=composer:latest /usr/bin/composer /usr/bin/composer

# Configure OPcache
RUN echo "opcache.enable=1" >> /usr/local/etc/php/conf.d/opcache.ini \
    && echo "opcache.memory_consumption=256" >> /usr/local/etc/php/conf.d/opcache.ini \
    && echo "opcache.max_accelerated_files=10000" >> /usr/local/etc/php/conf.d/opcache.ini

WORKDIR /var/www

# Copy application
COPY . .

# Install dependencies
RUN composer install --no-dev --optimize-autoloader

# Set permissions
RUN chown -R www-data:www-data /var/www

USER www-data

EXPOSE 9000
```

## PHP-Specific Best Practices

### 1. Use Strict Types

```php
<?php
declare(strict_types=1);

// This enforces strict type checking
function calculateTotal(float $price, int $quantity): float
{
    return $price * $quantity;
}

// This will throw a TypeError
// calculateTotal("10.50", "2");
```

### 2. Leverage Type Declarations

```php
class TaskRepository
{
    public function find(string $id): ?Task
    {
        // Return Task or null
    }
    
    public function findAll(): array
    {
        // Return array of Tasks
    }
    
    public function save(Task $task): void
    {
        // No return value
    }
}
```

### 3. Use Dependency Injection

```php
class TaskController
{
    public function __construct(
        private readonly TaskService $taskService,
        private readonly LoggerInterface $logger,
        private readonly CacheInterface $cache
    ) {}
    
    public function index(Request $request): Response
    {
        $cacheKey = 'tasks_' . md5($request->getUri());
        
        if ($cached = $this->cache->get($cacheKey)) {
            return new Response($cached);
        }
        
        $tasks = $this->taskService->listTasks();
        $this->cache->set($cacheKey, $tasks, 300); // Cache for 5 minutes
        
        return new Response($tasks);
    }
}
```

### 4. Implement Middleware

```php
class RateLimitMiddleware implements MiddlewareInterface
{
    public function __construct(
        private RateLimiter $limiter
    ) {}
    
    public function process(Request $request, RequestHandler $handler): Response
    {
        $clientId = $this->getClientId($request);
        
        if (!$this->limiter->allow($clientId)) {
            return new Response(429, [], json_encode([
                'error' => 'Rate limit exceeded'
            ]));
        }
        
        $response = $handler->handle($request);
        
        $response = $response
            ->withHeader('X-RateLimit-Limit', $this->limiter->getLimit())
            ->withHeader('X-RateLimit-Remaining', $this->limiter->getRemaining($clientId))
            ->withHeader('X-RateLimit-Reset', $this->limiter->getResetTime($clientId));
        
        return $response;
    }
    
    private function getClientId(Request $request): string
    {
        return $request->getHeaderLine('X-API-Key') 
            ?: $request->getServerParams()['REMOTE_ADDR'];
    }
}
```

## Conclusion

PHP's evolution from a simple scripting language to a modern, performant platform for web applications and APIs is remarkable. The language that powers WordPress, Facebook, and Wikipedia continues to innovate while maintaining its core strength: making web development accessible and productive.

Modern PHP, particularly version 8+, offers:
- **Type Safety**: Catching errors at development time rather than runtime
- **Performance**: JIT compilation and optimizations rival compiled languages for web workloads
- **Ecosystem**: Mature frameworks and libraries for every need
- **Deployment**: Runs everywhere from $5/month shared hosting to enterprise cloud infrastructure
- **Developer Experience**: Excellent tooling, documentation, and community support

Key takeaways from PHP API development:

1. **Modern PHP is Fast**: With OPcache, JIT, and proper configuration, PHP 8+ delivers excellent performance
2. **Type System**: Strict types, union types, and property types enable robust, maintainable code
3. **Ecosystem Maturity**: Composer and frameworks like Slim, Laravel, and Symfony provide production-ready foundations
4. **gRPC Support**: PHP has mature gRPC support for high-performance RPC communication
5. **Universal Deployment**: PHP's ubiquity means your API can run anywhere

As we continue our polyglot journey, PHP demonstrates that a language can evolve dramatically while maintaining backward compatibility and ease of use. Whether you're building a simple REST API or a complex microservices architecture, PHP provides the tools and performance to deliver robust solutions at any scale.