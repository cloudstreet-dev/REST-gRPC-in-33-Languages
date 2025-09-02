#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <ctype.h>
#include "task.h"
#include "task_repository.h"

#define PORT 8080
#define BUFFER_SIZE 8192
#define MAX_HEADERS 32

static TaskRepository repository;
static volatile int keep_running = 1;

typedef struct {
    char method[16];
    char path[256];
    char version[16];
    char headers[MAX_HEADERS][256];
    int header_count;
    char* body;
    int body_length;
} HttpRequest;

typedef struct {
    int status_code;
    char* body;
    char content_type[128];
} HttpResponse;

void signal_handler(int sig) {
    if (sig == SIGINT) {
        printf("\n[INFO] Shutting down server...\n");
        keep_running = 0;
    }
}

void parse_request(const char* raw_request, HttpRequest* request) {
    memset(request, 0, sizeof(HttpRequest));
    
    // Parse request line
    const char* line_end = strstr(raw_request, "\r\n");
    if (!line_end) return;
    
    char request_line[512];
    int line_len = line_end - raw_request;
    strncpy(request_line, raw_request, line_len);
    request_line[line_len] = '\0';
    
    sscanf(request_line, "%s %s %s", request->method, request->path, request->version);
    
    // Parse headers
    const char* header_start = line_end + 2;
    while (*header_start && request->header_count < MAX_HEADERS) {
        line_end = strstr(header_start, "\r\n");
        if (!line_end) break;
        
        if (line_end == header_start) {
            // Empty line - body starts after this
            request->body = (char*)(line_end + 2);
            break;
        }
        
        line_len = line_end - header_start;
        strncpy(request->headers[request->header_count], header_start, line_len);
        request->headers[request->header_count][line_len] = '\0';
        request->header_count++;
        
        header_start = line_end + 2;
    }
    
    // Get content length if present
    for (int i = 0; i < request->header_count; i++) {
        if (strncasecmp(request->headers[i], "Content-Length:", 15) == 0) {
            request->body_length = atoi(request->headers[i] + 15);
            break;
        }
    }
}

void send_response(int client_socket, HttpResponse* response) {
    char header[1024];
    const char* status_text;
    
    switch (response->status_code) {
        case 200: status_text = "OK"; break;
        case 201: status_text = "Created"; break;
        case 204: status_text = "No Content"; break;
        case 400: status_text = "Bad Request"; break;
        case 404: status_text = "Not Found"; break;
        case 405: status_text = "Method Not Allowed"; break;
        case 500: status_text = "Internal Server Error"; break;
        default: status_text = "Unknown";
    }
    
    int body_len = response->body ? strlen(response->body) : 0;
    
    snprintf(header, sizeof(header),
        "HTTP/1.1 %d %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %d\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE, OPTIONS\r\n"
        "Access-Control-Allow-Headers: Content-Type\r\n"
        "\r\n",
        response->status_code, status_text,
        response->content_type,
        body_len
    );
    
    send(client_socket, header, strlen(header), 0);
    if (response->body && body_len > 0) {
        send(client_socket, response->body, body_len, 0);
    }
}

void handle_list_tasks(HttpRequest* request, HttpResponse* response) {
    // Extract query parameters from path
    char* query = strchr(request->path, '?');
    char* status = NULL;
    char* assigned_to = NULL;
    
    if (query) {
        *query = '\0';
        query++;
        
        // Simple query parameter parsing
        char* param = strtok(query, "&");
        while (param) {
            char* value = strchr(param, '=');
            if (value) {
                *value = '\0';
                value++;
                
                if (strcmp(param, "status") == 0) {
                    status = value;
                } else if (strcmp(param, "assigned_to") == 0) {
                    assigned_to = value;
                }
            }
            param = strtok(NULL, "&");
        }
    }
    
    Task tasks[100];
    int count = repository_list_tasks(&repository, tasks, 100, status, assigned_to, NULL, 0);
    
    // Build JSON response
    response->body = malloc(BUFFER_SIZE * 4);
    strcpy(response->body, "{\"tasks\":[");
    
    for (int i = 0; i < count; i++) {
        if (i > 0) strcat(response->body, ",");
        char* task_json = task_to_json(&tasks[i]);
        strcat(response->body, task_json);
        free(task_json);
    }
    
    char count_str[128];
    snprintf(count_str, sizeof(count_str), "],\"total_count\":%d,\"page_size\":100}", count);
    strcat(response->body, count_str);
    
    response->status_code = 200;
    strcpy(response->content_type, "application/json");
}

void handle_get_task(const char* id, HttpResponse* response) {
    Task* task = repository_get_task(&repository, id);
    
    if (task) {
        response->body = task_to_json(task);
        response->status_code = 200;
        strcpy(response->content_type, "application/json");
    } else {
        response->body = strdup("{\"error\":\"Task not found\"}");
        response->status_code = 404;
        strcpy(response->content_type, "application/json");
    }
}

void handle_create_task(HttpRequest* request, HttpResponse* response) {
    if (!request->body || request->body_length == 0) {
        response->body = strdup("{\"error\":\"Request body is required\"}");
        response->status_code = 400;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    Task new_task;
    if (!task_from_json(&new_task, request->body)) {
        response->body = strdup("{\"error\":\"Invalid JSON or missing title\"}");
        response->status_code = 400;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    if (repository_create_task(&repository, &new_task)) {
        response->body = task_to_json(&new_task);
        response->status_code = 201;
        strcpy(response->content_type, "application/json");
    } else {
        response->body = strdup("{\"error\":\"Failed to create task\"}");
        response->status_code = 500;
        strcpy(response->content_type, "application/json");
    }
}

void handle_update_task(const char* id, HttpRequest* request, HttpResponse* response) {
    if (!request->body || request->body_length == 0) {
        response->body = strdup("{\"error\":\"Request body is required\"}");
        response->status_code = 400;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    Task* existing = repository_get_task(&repository, id);
    if (!existing) {
        response->body = strdup("{\"error\":\"Task not found\"}");
        response->status_code = 404;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    Task update_task;
    if (!task_from_json(&update_task, request->body)) {
        response->body = strdup("{\"error\":\"Invalid JSON\"}");
        response->status_code = 400;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    strcpy(update_task.id, id);
    
    if (repository_update_task(&repository, id, &update_task)) {
        Task* updated = repository_get_task(&repository, id);
        response->body = task_to_json(updated);
        response->status_code = 200;
        strcpy(response->content_type, "application/json");
    } else {
        response->body = strdup("{\"error\":\"Failed to update task\"}");
        response->status_code = 500;
        strcpy(response->content_type, "application/json");
    }
}

void handle_update_status(const char* id, HttpRequest* request, HttpResponse* response) {
    // Extract status from query parameter
    char* query = strchr(request->path, '?');
    char* status = NULL;
    
    if (query) {
        query++;
        char* param = strstr(query, "status=");
        if (param) {
            status = param + 7;
            char* end = strchr(status, '&');
            if (end) *end = '\0';
        }
    }
    
    if (!status) {
        response->body = strdup("{\"error\":\"Status parameter is required\"}");
        response->status_code = 400;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    Task* existing = repository_get_task(&repository, id);
    if (!existing) {
        response->body = strdup("{\"error\":\"Task not found\"}");
        response->status_code = 404;
        strcpy(response->content_type, "application/json");
        return;
    }
    
    Task update_task = *existing;
    update_task.status = task_string_to_status(status);
    
    if (repository_update_task(&repository, id, &update_task)) {
        Task* updated = repository_get_task(&repository, id);
        response->body = task_to_json(updated);
        response->status_code = 200;
        strcpy(response->content_type, "application/json");
    } else {
        response->body = strdup("{\"error\":\"Failed to update status\"}");
        response->status_code = 500;
        strcpy(response->content_type, "application/json");
    }
}

void handle_delete_task(const char* id, HttpResponse* response) {
    if (repository_delete_task(&repository, id)) {
        response->body = NULL;
        response->status_code = 204;
        strcpy(response->content_type, "text/plain");
    } else {
        response->body = strdup("{\"error\":\"Task not found\"}");
        response->status_code = 404;
        strcpy(response->content_type, "application/json");
    }
}

void handle_health(HttpResponse* response) {
    char health[256];
    snprintf(health, sizeof(health),
        "{\"status\":\"healthy\",\"service\":\"c-task-api\",\"task_count\":%d}",
        repository_get_count(&repository));
    
    response->body = strdup(health);
    response->status_code = 200;
    strcpy(response->content_type, "application/json");
}

void* handle_client(void* arg) {
    int client_socket = *(int*)arg;
    free(arg);
    
    char buffer[BUFFER_SIZE];
    memset(buffer, 0, BUFFER_SIZE);
    
    int bytes_read = recv(client_socket, buffer, BUFFER_SIZE - 1, 0);
    if (bytes_read <= 0) {
        close(client_socket);
        return NULL;
    }
    
    HttpRequest request;
    HttpResponse response = {0};
    parse_request(buffer, &request);
    
    printf("[%s] %s\n", request.method, request.path);
    
    // Route handling
    if (strcmp(request.method, "OPTIONS") == 0) {
        response.status_code = 204;
        strcpy(response.content_type, "text/plain");
    } else if (strcmp(request.path, "/health") == 0 && strcmp(request.method, "GET") == 0) {
        handle_health(&response);
    } else if (strncmp(request.path, "/api/tasks", 10) == 0) {
        char* path_suffix = request.path + 10;
        
        if (*path_suffix == '\0' || *path_suffix == '?') {
            // /api/tasks or /api/tasks?...
            if (strcmp(request.method, "GET") == 0) {
                handle_list_tasks(&request, &response);
            } else if (strcmp(request.method, "POST") == 0) {
                handle_create_task(&request, &response);
            } else {
                response.body = strdup("{\"error\":\"Method not allowed\"}");
                response.status_code = 405;
                strcpy(response.content_type, "application/json");
            }
        } else if (*path_suffix == '/') {
            path_suffix++;
            char* slash = strchr(path_suffix, '/');
            
            if (slash) {
                // /api/tasks/{id}/...
                *slash = '\0';
                char* id = path_suffix;
                char* action = slash + 1;
                
                if (strncmp(action, "status", 6) == 0 && strcmp(request.method, "PATCH") == 0) {
                    handle_update_status(id, &request, &response);
                } else {
                    response.body = strdup("{\"error\":\"Not found\"}");
                    response.status_code = 404;
                    strcpy(response.content_type, "application/json");
                }
            } else {
                // /api/tasks/{id}
                char* id = path_suffix;
                
                if (strcmp(request.method, "GET") == 0) {
                    handle_get_task(id, &response);
                } else if (strcmp(request.method, "PUT") == 0) {
                    handle_update_task(id, &request, &response);
                } else if (strcmp(request.method, "DELETE") == 0) {
                    handle_delete_task(id, &response);
                } else {
                    response.body = strdup("{\"error\":\"Method not allowed\"}");
                    response.status_code = 405;
                    strcpy(response.content_type, "application/json");
                }
            }
        }
    } else {
        response.body = strdup("{\"error\":\"Not found\"}");
        response.status_code = 404;
        strcpy(response.content_type, "application/json");
    }
    
    send_response(client_socket, &response);
    
    if (response.body) {
        free(response.body);
    }
    
    close(client_socket);
    return NULL;
}

int main(int argc, char* argv[]) {
    int port = PORT;
    if (argc > 1) {
        port = atoi(argv[1]);
        if (port <= 0) port = PORT;
    }
    
    printf("╔════════════════════════════════════════════════╗\n");
    printf("║          C Task Management REST API            ║\n");
    printf("║         Built with minimal dependencies        ║\n");
    printf("╚════════════════════════════════════════════════╝\n\n");
    
    // Initialize repository
    repository_init(&repository);
    
    // Set up signal handler
    signal(SIGINT, signal_handler);
    
    // Create socket
    int server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket < 0) {
        perror("Socket creation failed");
        return 1;
    }
    
    // Allow socket reuse
    int opt = 1;
    if (setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        perror("Setsockopt failed");
        return 1;
    }
    
    // Bind socket
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(port);
    
    if (bind(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Bind failed");
        return 1;
    }
    
    // Listen for connections
    if (listen(server_socket, 10) < 0) {
        perror("Listen failed");
        return 1;
    }
    
    printf("[INFO] C Task REST Server started on port %d\n", port);
    printf("[INFO] Visit http://localhost:%d/api/tasks\n\n", port);
    
    printf("Available endpoints:\n");
    printf("  GET    /api/tasks          - List all tasks\n");
    printf("  GET    /api/tasks/{id}     - Get a specific task\n");
    printf("  POST   /api/tasks          - Create a new task\n");
    printf("  PUT    /api/tasks/{id}     - Update a task\n");
    printf("  PATCH  /api/tasks/{id}/status - Update task status\n");
    printf("  DELETE /api/tasks/{id}     - Delete a task\n");
    printf("  GET    /health             - Health check\n\n");
    
    printf("Sample requests:\n");
    printf("  curl http://localhost:%d/api/tasks\n", port);
    printf("  curl -X POST http://localhost:%d/api/tasks \\\n", port);
    printf("    -H \"Content-Type: application/json\" \\\n");
    printf("    -d '{\"title\":\"New Task\",\"priority\":\"high\"}'\n\n");
    
    printf("[INFO] Press Ctrl+C to stop the server\n\n");
    
    // Accept connections
    while (keep_running) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        
        int* client_socket = malloc(sizeof(int));
        *client_socket = accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
        
        if (*client_socket < 0) {
            if (keep_running) {
                perror("Accept failed");
            }
            free(client_socket);
            continue;
        }
        
        // Handle client in a new thread
        pthread_t thread;
        pthread_create(&thread, NULL, handle_client, client_socket);
        pthread_detach(thread);
    }
    
    // Cleanup
    close(server_socket);
    repository_destroy(&repository);
    
    printf("[INFO] Server stopped\n");
    return 0;
}