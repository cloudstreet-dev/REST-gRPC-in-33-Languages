#pragma once

#include "models.h"
#include <curl/curl.h>
#include <memory>
#include <functional>

namespace task_api {

class HttpResponse {
public:
    long status_code;
    std::string body;
    
    HttpResponse(long code, std::string body) : status_code(code), body(std::move(body)) {}
    
    bool is_success() const { return status_code >= 200 && status_code < 300; }
};

class TaskApiClient {
public:
    explicit TaskApiClient(const std::string& base_url);
    ~TaskApiClient();
    
    // Disable copy constructor and assignment
    TaskApiClient(const TaskApiClient&) = delete;
    TaskApiClient& operator=(const TaskApiClient&) = delete;
    
    // Move constructor and assignment
    TaskApiClient(TaskApiClient&&) = default;
    TaskApiClient& operator=(TaskApiClient&&) = default;
    
    // API methods
    std::expected<ListTasksResponse, std::string> list_tasks(
        std::optional<TaskStatus> status = std::nullopt,
        std::optional<std::string> assigned_to = std::nullopt,
        std::optional<std::vector<std::string>> tags = std::nullopt,
        int page_size = 20,
        std::optional<std::string> page_token = std::nullopt,
        const std::string& sort_by = "created_at",
        const std::string& sort_order = "desc"
    );
    
    std::expected<Task, std::string> get_task(const std::string& id);
    
    std::expected<Task, std::string> create_task(const CreateTaskRequest& request);
    
    std::expected<Task, std::string> update_task(const std::string& id, const UpdateTaskRequest& request);
    
    std::expected<Task, std::string> update_task_status(const std::string& id, TaskStatus status);
    
    std::expected<bool, std::string> delete_task(const std::string& id);
    
private:
    std::string base_url_;
    CURL* curl_;
    
    // HTTP helper methods
    HttpResponse perform_request(const std::string& url, const std::string& method, 
                               const std::string& body = "", 
                               const std::vector<std::string>& headers = {});
    
    std::string build_query_string(const std::map<std::string, std::string>& params);
    
    static size_t write_callback(void* contents, size_t size, size_t nmemb, std::string* output);
};

// Utility functions for client usage
void print_task(const Task& task);
void print_error(const std::string& error);
std::string format_datetime(const std::chrono::system_clock::time_point& tp);

} // namespace task_api