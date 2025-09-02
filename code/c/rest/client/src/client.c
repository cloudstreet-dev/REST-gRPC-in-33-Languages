#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define BUFFER_SIZE 8192
#define DEFAULT_HOST "localhost"
#define DEFAULT_PORT 8080

typedef struct {
    char* body;
    int status_code;
    int content_length;
} HttpResponse;

int parse_url(const char* url, char* host, int* port, char* path) {
    // Default values
    strcpy(host, DEFAULT_HOST);
    *port = DEFAULT_PORT;
    strcpy(path, "/");
    
    // Skip http:// if present
    const char* url_start = url;
    if (strncmp(url, "http://", 7) == 0) {
        url_start += 7;
    }
    
    // Find path separator
    const char* path_start = strchr(url_start, '/');
    if (path_start) {
        // Copy host:port part
        int host_len = path_start - url_start;
        char host_port[256];
        strncpy(host_port, url_start, host_len);
        host_port[host_len] = '\0';
        
        // Check for port
        char* colon = strchr(host_port, ':');
        if (colon) {
            *colon = '\0';
            strcpy(host, host_port);
            *port = atoi(colon + 1);
        } else {
            strcpy(host, host_port);
        }
        
        // Copy path
        strcpy(path, path_start);
    } else {
        // No path, just host:port
        char* colon = strchr(url_start, ':');
        if (colon) {
            int host_len = colon - url_start;
            strncpy(host, url_start, host_len);
            host[host_len] = '\0';
            *port = atoi(colon + 1);
        } else {
            strcpy(host, url_start);
        }
    }
    
    return 0;
}

int connect_to_server(const char* host, int port) {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Socket creation failed");
        return -1;
    }
    
    struct hostent* server = gethostbyname(host);
    if (!server) {
        fprintf(stderr, "Error: No such host: %s\n", host);
        close(sock);
        return -1;
    }
    
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    memcpy(&server_addr.sin_addr.s_addr, server->h_addr, server->h_length);
    server_addr.sin_port = htons(port);
    
    if (connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Connection failed");
        close(sock);
        return -1;
    }
    
    return sock;
}

HttpResponse* http_request(const char* method, const char* url, const char* body) {
    char host[256];
    char path[512];
    int port;
    
    parse_url(url, host, &port, path);
    
    int sock = connect_to_server(host, port);
    if (sock < 0) {
        return NULL;
    }
    
    // Build request
    char request[BUFFER_SIZE];
    int body_len = body ? strlen(body) : 0;
    
    if (body_len > 0) {
        snprintf(request, sizeof(request),
            "%s %s HTTP/1.1\r\n"
            "Host: %s:%d\r\n"
            "Content-Type: application/json\r\n"
            "Content-Length: %d\r\n"
            "Connection: close\r\n"
            "\r\n"
            "%s",
            method, path, host, port, body_len, body
        );
    } else {
        snprintf(request, sizeof(request),
            "%s %s HTTP/1.1\r\n"
            "Host: %s:%d\r\n"
            "Connection: close\r\n"
            "\r\n",
            method, path, host, port
        );
    }
    
    // Send request
    if (send(sock, request, strlen(request), 0) < 0) {
        perror("Send failed");
        close(sock);
        return NULL;
    }
    
    // Read response
    char buffer[BUFFER_SIZE];
    memset(buffer, 0, sizeof(buffer));
    int total_read = 0;
    int bytes_read;
    
    while ((bytes_read = recv(sock, buffer + total_read, sizeof(buffer) - total_read - 1, 0)) > 0) {
        total_read += bytes_read;
    }
    
    close(sock);
    
    // Parse response
    HttpResponse* response = malloc(sizeof(HttpResponse));
    response->body = NULL;
    response->status_code = 0;
    response->content_length = 0;
    
    // Get status code
    if (sscanf(buffer, "HTTP/1.1 %d", &response->status_code) != 1) {
        free(response);
        return NULL;
    }
    
    // Find body start
    char* body_start = strstr(buffer, "\r\n\r\n");
    if (body_start) {
        body_start += 4;
        response->body = strdup(body_start);
    }
    
    return response;
}

void print_json_pretty(const char* json) {
    if (!json) return;
    
    int indent = 0;
    int in_string = 0;
    
    for (const char* p = json; *p; p++) {
        if (*p == '"' && (p == json || *(p-1) != '\\')) {
            in_string = !in_string;
        }
        
        if (!in_string) {
            if (*p == '{' || *p == '[') {
                putchar(*p);
                putchar('\n');
                indent += 2;
                for (int i = 0; i < indent; i++) putchar(' ');
            } else if (*p == '}' || *p == ']') {
                putchar('\n');
                indent -= 2;
                for (int i = 0; i < indent; i++) putchar(' ');
                putchar(*p);
            } else if (*p == ',') {
                putchar(*p);
                putchar('\n');
                for (int i = 0; i < indent; i++) putchar(' ');
            } else if (*p != ' ' && *p != '\n' && *p != '\r' && *p != '\t') {
                putchar(*p);
            }
        } else {
            putchar(*p);
        }
    }
    putchar('\n');
}

void print_help() {
    printf("╔════════════════════════════════════════════════╗\n");
    printf("║         C Task Management CLI                  ║\n");
    printf("╚════════════════════════════════════════════════╝\n\n");
    
    printf("Usage: client <command> [options]\n\n");
    
    printf("Commands:\n");
    printf("  list                    List all tasks\n");
    printf("  get <task-id>          Get a specific task\n");
    printf("  create <json>          Create a new task\n");
    printf("  update <task-id> <json> Update a task\n");
    printf("  delete <task-id>       Delete a task\n");
    printf("  health                 Check server health\n");
    printf("  demo                   Run a demonstration\n");
    printf("  help                   Show this help\n\n");
    
    printf("Environment:\n");
    printf("  TASK_API_URL  Base URL (default: http://localhost:8080)\n\n");
    
    printf("Examples:\n");
    printf("  client list\n");
    printf("  client get 123e4567-e89b-12d3-a456-426614174000\n");
    printf("  client create '{\"title\":\"New Task\",\"priority\":\"high\"}'\n");
    printf("  client delete 123e4567-e89b-12d3-a456-426614174000\n");
}

void run_demo() {
    printf("╔════════════════════════════════════════════════╗\n");
    printf("║          Task Management API Demo              ║\n");
    printf("╚════════════════════════════════════════════════╝\n\n");
    
    const char* base_url = getenv("TASK_API_URL");
    if (!base_url) base_url = "http://localhost:8080";
    
    char url[512];
    
    // Step 1: List tasks
    printf("1. Listing all tasks...\n");
    snprintf(url, sizeof(url), "%s/api/tasks", base_url);
    HttpResponse* response = http_request("GET", url, NULL);
    if (response) {
        printf("   Status: %d\n", response->status_code);
        if (response->body && response->status_code == 200) {
            printf("   Response:\n");
            print_json_pretty(response->body);
        }
        free(response->body);
        free(response);
    }
    printf("\n");
    
    // Step 2: Create a task
    printf("2. Creating a new task...\n");
    const char* new_task = "{\"title\":\"Demo Task from C Client\","
                          "\"description\":\"This task was created using the C REST client\","
                          "\"priority\":\"high\","
                          "\"tags\":[\"demo\",\"c\",\"api-test\"],"
                          "\"assigned_to\":\"demo-user\"}";
    
    response = http_request("POST", url, new_task);
    char task_id[64] = "";
    if (response) {
        printf("   Status: %d\n", response->status_code);
        if (response->body && response->status_code == 201) {
            printf("   Created task:\n");
            print_json_pretty(response->body);
            
            // Extract task ID (simple extraction)
            char* id_start = strstr(response->body, "\"id\":\"");
            if (id_start) {
                id_start += 6;
                char* id_end = strchr(id_start, '"');
                if (id_end) {
                    int len = id_end - id_start;
                    if (len < 64) {
                        strncpy(task_id, id_start, len);
                        task_id[len] = '\0';
                    }
                }
            }
        }
        free(response->body);
        free(response);
    }
    printf("\n");
    
    if (strlen(task_id) > 0) {
        // Step 3: Get the created task
        printf("3. Retrieving the created task...\n");
        snprintf(url, sizeof(url), "%s/api/tasks/%s", base_url, task_id);
        response = http_request("GET", url, NULL);
        if (response) {
            printf("   Status: %d\n", response->status_code);
            if (response->body && response->status_code == 200) {
                printf("   Task details:\n");
                print_json_pretty(response->body);
            }
            free(response->body);
            free(response);
        }
        printf("\n");
        
        // Step 4: Update the task
        printf("4. Updating the task...\n");
        const char* update = "{\"title\":\"Updated Demo Task\","
                           "\"description\":\"This task has been updated\","
                           "\"status\":\"in_progress\","
                           "\"priority\":\"urgent\"}";
        
        response = http_request("PUT", url, update);
        if (response) {
            printf("   Status: %d\n", response->status_code);
            if (response->body && response->status_code == 200) {
                printf("   Updated task:\n");
                print_json_pretty(response->body);
            }
            free(response->body);
            free(response);
        }
        printf("\n");
        
        // Step 5: Update status
        printf("5. Updating task status to completed...\n");
        snprintf(url, sizeof(url), "%s/api/tasks/%s/status?status=completed", base_url, task_id);
        response = http_request("PATCH", url, NULL);
        if (response) {
            printf("   Status: %d\n", response->status_code);
            if (response->body && response->status_code == 200) {
                printf("   Task with updated status:\n");
                print_json_pretty(response->body);
            }
            free(response->body);
            free(response);
        }
        printf("\n");
        
        // Step 6: Delete the task
        printf("6. Deleting the demo task...\n");
        snprintf(url, sizeof(url), "%s/api/tasks/%s", base_url, task_id);
        response = http_request("DELETE", url, NULL);
        if (response) {
            printf("   Status: %d\n", response->status_code);
            if (response->status_code == 204) {
                printf("   Task deleted successfully\n");
            }
            free(response->body);
            free(response);
        }
        printf("\n");
        
        // Step 7: Verify deletion
        printf("7. Verifying task deletion...\n");
        response = http_request("GET", url, NULL);
        if (response) {
            printf("   Status: %d\n", response->status_code);
            if (response->status_code == 404) {
                printf("   Task not found (as expected)\n");
            }
            free(response->body);
            free(response);
        }
    }
    
    printf("\n╔════════════════════════════════════════════════╗\n");
    printf("║              Demo Complete!                    ║\n");
    printf("╚════════════════════════════════════════════════╝\n");
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_help();
        return 1;
    }
    
    const char* base_url = getenv("TASK_API_URL");
    if (!base_url) base_url = "http://localhost:8080";
    
    char url[512];
    const char* command = argv[1];
    
    if (strcmp(command, "help") == 0) {
        print_help();
        return 0;
    }
    
    if (strcmp(command, "demo") == 0) {
        run_demo();
        return 0;
    }
    
    if (strcmp(command, "health") == 0) {
        snprintf(url, sizeof(url), "%s/health", base_url);
        HttpResponse* response = http_request("GET", url, NULL);
        if (response) {
            printf("Status: %d\n", response->status_code);
            if (response->body) {
                print_json_pretty(response->body);
                free(response->body);
            }
            free(response);
        }
        return 0;
    }
    
    if (strcmp(command, "list") == 0) {
        snprintf(url, sizeof(url), "%s/api/tasks", base_url);
        HttpResponse* response = http_request("GET", url, NULL);
        if (response) {
            printf("Status: %d\n", response->status_code);
            if (response->body) {
                print_json_pretty(response->body);
                free(response->body);
            }
            free(response);
        }
        return 0;
    }
    
    if (strcmp(command, "get") == 0 && argc >= 3) {
        snprintf(url, sizeof(url), "%s/api/tasks/%s", base_url, argv[2]);
        HttpResponse* response = http_request("GET", url, NULL);
        if (response) {
            printf("Status: %d\n", response->status_code);
            if (response->body) {
                print_json_pretty(response->body);
                free(response->body);
            }
            free(response);
        }
        return 0;
    }
    
    if (strcmp(command, "create") == 0 && argc >= 3) {
        snprintf(url, sizeof(url), "%s/api/tasks", base_url);
        HttpResponse* response = http_request("POST", url, argv[2]);
        if (response) {
            printf("Status: %d\n", response->status_code);
            if (response->body) {
                print_json_pretty(response->body);
                free(response->body);
            }
            free(response);
        }
        return 0;
    }
    
    if (strcmp(command, "update") == 0 && argc >= 4) {
        snprintf(url, sizeof(url), "%s/api/tasks/%s", base_url, argv[2]);
        HttpResponse* response = http_request("PUT", url, argv[3]);
        if (response) {
            printf("Status: %d\n", response->status_code);
            if (response->body) {
                print_json_pretty(response->body);
                free(response->body);
            }
            free(response);
        }
        return 0;
    }
    
    if (strcmp(command, "delete") == 0 && argc >= 3) {
        snprintf(url, sizeof(url), "%s/api/tasks/%s", base_url, argv[2]);
        HttpResponse* response = http_request("DELETE", url, NULL);
        if (response) {
            printf("Status: %d\n", response->status_code);
            if (response->status_code == 204) {
                printf("Task deleted successfully\n");
            } else if (response->body) {
                print_json_pretty(response->body);
                free(response->body);
            }
            free(response);
        }
        return 0;
    }
    
    fprintf(stderr, "Unknown command: %s\n", command);
    print_help();
    return 1;
}