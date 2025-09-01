<?php
declare(strict_types=1);

use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use Slim\Factory\AppFactory;
use Slim\Exception\HttpNotFoundException;
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
    } catch (Exception $e) {
        $error = ['error' => ['code' => 'INTERNAL_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500);
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
    } catch (Exception $e) {
        $error = ['error' => ['code' => 'INTERNAL_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500);
    }
});

// Update task
$app->put('/api/v1/tasks/{id}', function (Request $request, Response $response, array $args) use ($taskService) {
    try {
        $data = json_decode((string)$request->getBody(), true);
        
        if ($data === null) {
            $error = ['error' => ['code' => 'INVALID_JSON', 'message' => 'Invalid JSON in request body']];
            $response->getBody()->write(json_encode($error));
            return $response->withStatus(400);
        }
        
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
    } catch (Exception $e) {
        $error = ['error' => ['code' => 'INTERNAL_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500);
    }
});

// Update task status
$app->patch('/api/v1/tasks/{id}/status', function (Request $request, Response $response, array $args) use ($taskService) {
    try {
        $data = json_decode((string)$request->getBody(), true);
        
        if ($data === null || !isset($data['status'])) {
            $error = ['error' => ['code' => 'VALIDATION_ERROR', 'message' => 'Status is required']];
            $response->getBody()->write(json_encode($error));
            return $response->withStatus(400);
        }
        
        $task = $taskService->updateTaskStatus($args['id'], $data['status']);
        
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
    } catch (Exception $e) {
        $error = ['error' => ['code' => 'INTERNAL_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500);
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
    } catch (Exception $e) {
        $error = ['error' => ['code' => 'INTERNAL_ERROR', 'message' => $e->getMessage()]];
        $response->getBody()->write(json_encode($error));
        return $response->withStatus(500);
    }
});

$app->run();