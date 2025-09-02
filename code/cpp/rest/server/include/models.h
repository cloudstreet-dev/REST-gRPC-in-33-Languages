#pragma once

#include <string>
#include <vector>
#include <optional>
#include <chrono>
#include <nlohmann/json.hpp>

namespace task_api {

enum class TaskStatus {
    Pending,
    InProgress, 
    Completed
};

enum class TaskPriority {
    Low,
    Medium,
    High,
    Critical
};

// Task model
struct Task {
    std::string id;
    std::string title;
    std::optional<std::string> description;
    TaskStatus status = TaskStatus::Pending;
    TaskPriority priority = TaskPriority::Medium;
    std::vector<std::string> tags;
    std::optional<std::string> assigned_to;
    std::string created_by = "system";
    std::chrono::system_clock::time_point created_at;
    std::chrono::system_clock::time_point updated_at;
    std::optional<std::chrono::system_clock::time_point> due_date;
    
    // Default constructor
    Task() : created_at(std::chrono::system_clock::now()), 
             updated_at(std::chrono::system_clock::now()) {}
};

// Request/Response DTOs
struct CreateTaskRequest {
    std::string title;
    std::optional<std::string> description;
    std::optional<TaskPriority> priority;
    std::optional<std::vector<std::string>> tags;
    std::optional<std::string> assigned_to;
    std::optional<std::string> created_by;
    std::optional<std::chrono::system_clock::time_point> due_date;
};

struct UpdateTaskRequest {
    std::optional<std::string> title;
    std::optional<std::string> description;
    std::optional<TaskStatus> status;
    std::optional<TaskPriority> priority;
    std::optional<std::vector<std::string>> tags;
    std::optional<std::string> assigned_to;
    std::optional<std::chrono::system_clock::time_point> due_date;
};

struct ListTasksResponse {
    std::vector<Task> tasks;
    std::optional<std::string> next_page_token;
    int total_count = 0;
};

struct TaskFilters {
    std::optional<TaskStatus> status;
    std::optional<std::string> assigned_to;
    std::optional<std::vector<std::string>> tags;
    int page_size = 20;
    std::optional<std::string> page_token;
    std::string sort_by = "created_at";
    std::string sort_order = "desc";
};

struct ErrorResponse {
    std::string message;
    std::optional<nlohmann::json> errors;
};

// JSON conversion functions
void to_json(nlohmann::json& j, const TaskStatus& status);
void from_json(const nlohmann::json& j, TaskStatus& status);

void to_json(nlohmann::json& j, const TaskPriority& priority);
void from_json(const nlohmann::json& j, TaskPriority& priority);

void to_json(nlohmann::json& j, const Task& task);
void from_json(const nlohmann::json& j, Task& task);

void to_json(nlohmann::json& j, const CreateTaskRequest& request);
void from_json(const nlohmann::json& j, CreateTaskRequest& request);

void to_json(nlohmann::json& j, const UpdateTaskRequest& request);
void from_json(const nlohmann::json& j, UpdateTaskRequest& request);

void to_json(nlohmann::json& j, const ListTasksResponse& response);

void to_json(nlohmann::json& j, const ErrorResponse& error);

// Utility functions
std::string to_iso_string(const std::chrono::system_clock::time_point& tp);
std::chrono::system_clock::time_point from_iso_string(const std::string& str);

std::string generate_uuid();
std::string trim(const std::string& str);
bool validate_title(const std::string& title);

} // namespace task_api