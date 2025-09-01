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
        } catch (GuzzleException $e) {
            throw new APIException("Request failed: " . $e->getMessage());
        }
    }
    
    public function getTask(string $id): array
    {
        try {
            $response = $this->httpClient->get("/tasks/{$id}");
            return json_decode($response->getBody()->getContents(), true);
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        } catch (GuzzleException $e) {
            throw new APIException("Request failed: " . $e->getMessage());
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
        } catch (GuzzleException $e) {
            throw new APIException("Request failed: " . $e->getMessage());
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
        } catch (GuzzleException $e) {
            throw new APIException("Request failed: " . $e->getMessage());
        }
    }
    
    public function updateTaskStatus(string $id, string $status): array
    {
        try {
            $response = $this->httpClient->patch("/tasks/{$id}/status", [
                'json' => ['status' => $status]
            ]);
            
            return json_decode($response->getBody()->getContents(), true);
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        } catch (GuzzleException $e) {
            throw new APIException("Request failed: " . $e->getMessage());
        }
    }
    
    public function deleteTask(string $id): bool
    {
        try {
            $response = $this->httpClient->delete("/tasks/{$id}");
            return $response->getStatusCode() === 204;
        } catch (ClientException $e) {
            throw $this->handleClientException($e);
        } catch (GuzzleException $e) {
            throw new APIException("Request failed: " . $e->getMessage());
        }
    }
    
    private function updateHttpClient(): void
    {
        $this->httpClient = new Client([
            'base_uri' => $this->baseUrl,
            'timeout' => 30.0,
            'headers' => $this->headers
        ]);
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

class APIException extends \Exception {}
class ValidationException extends APIException {}
class NotFoundException extends APIException {}
class AuthenticationException extends APIException {}
class AuthorizationException extends APIException {}
class ServerException extends APIException {}