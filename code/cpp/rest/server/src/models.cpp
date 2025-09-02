#include "models.h"
#include <sstream>
#include <iomanip>
#include <random>
#include <algorithm>
#include <cctype>

namespace task_api {

// JSON conversion for TaskStatus
void to_json(nlohmann::json& j, const TaskStatus& status) {
    switch (status) {
        case TaskStatus::Pending: j = "pending"; break;
        case TaskStatus::InProgress: j = "in_progress"; break;
        case TaskStatus::Completed: j = "completed"; break;
    }
}

void from_json(const nlohmann::json& j, TaskStatus& status) {
    std::string str = j.get<std::string>();
    if (str == "pending") status = TaskStatus::Pending;
    else if (str == "in_progress") status = TaskStatus::InProgress;
    else if (str == "completed") status = TaskStatus::Completed;
    else throw std::invalid_argument("Invalid TaskStatus: " + str);
}

// JSON conversion for TaskPriority
void to_json(nlohmann::json& j, const TaskPriority& priority) {
    switch (priority) {
        case TaskPriority::Low: j = "low"; break;
        case TaskPriority::Medium: j = "medium"; break;
        case TaskPriority::High: j = "high"; break;
        case TaskPriority::Critical: j = "critical"; break;
    }
}

void from_json(const nlohmann::json& j, TaskPriority& priority) {
    std::string str = j.get<std::string>();
    if (str == "low") priority = TaskPriority::Low;
    else if (str == "medium") priority = TaskPriority::Medium;
    else if (str == "high") priority = TaskPriority::High;
    else if (str == "critical") priority = TaskPriority::Critical;
    else throw std::invalid_argument("Invalid TaskPriority: " + str);
}

// JSON conversion for Task
void to_json(nlohmann::json& j, const Task& task) {
    j = nlohmann::json{
        {"id", task.id},
        {"title", task.title},
        {"status", task.status},
        {"priority", task.priority},
        {"tags", task.tags},
        {"created_by", task.created_by},
        {"created_at", to_iso_string(task.created_at)},
        {"updated_at", to_iso_string(task.updated_at)}
    };
    
    if (task.description) {
        j["description"] = *task.description;
    }
    
    if (task.assigned_to) {
        j["assigned_to"] = *task.assigned_to;
    }
    
    if (task.due_date) {
        j["due_date"] = to_iso_string(*task.due_date);
    }
}

void from_json(const nlohmann::json& j, Task& task) {
    j.at("id").get_to(task.id);
    j.at("title").get_to(task.title);
    j.at("status").get_to(task.status);
    j.at("priority").get_to(task.priority);
    j.at("tags").get_to(task.tags);
    j.at("created_by").get_to(task.created_by);
    
    task.created_at = from_iso_string(j.at("created_at").get<std::string>());
    task.updated_at = from_iso_string(j.at("updated_at").get<std::string>());
    
    if (j.contains("description") && !j["description"].is_null()) {
        task.description = j["description"].get<std::string>();
    }
    
    if (j.contains("assigned_to") && !j["assigned_to"].is_null()) {
        task.assigned_to = j["assigned_to"].get<std::string>();
    }
    
    if (j.contains("due_date") && !j["due_date"].is_null()) {
        task.due_date = from_iso_string(j["due_date"].get<std::string>());
    }
}

// JSON conversion for CreateTaskRequest
void from_json(const nlohmann::json& j, CreateTaskRequest& request) {
    j.at("title").get_to(request.title);
    
    if (j.contains("description") && !j["description"].is_null()) {
        request.description = j["description"].get<std::string>();
    }
    
    if (j.contains("priority") && !j["priority"].is_null()) {
        TaskPriority priority;
        j["priority"].get_to(priority);
        request.priority = priority;
    }
    
    if (j.contains("tags") && !j["tags"].is_null()) {
        request.tags = j["tags"].get<std::vector<std::string>>();
    }
    
    if (j.contains("assigned_to") && !j["assigned_to"].is_null()) {
        request.assigned_to = j["assigned_to"].get<std::string>();
    }
    
    if (j.contains("created_by") && !j["created_by"].is_null()) {
        request.created_by = j["created_by"].get<std::string>();
    }
    
    if (j.contains("due_date") && !j["due_date"].is_null()) {
        request.due_date = from_iso_string(j["due_date"].get<std::string>());
    }
}

// JSON conversion for UpdateTaskRequest
void from_json(const nlohmann::json& j, UpdateTaskRequest& request) {
    if (j.contains("title") && !j["title"].is_null()) {
        request.title = j["title"].get<std::string>();
    }
    
    if (j.contains("description") && !j["description"].is_null()) {
        request.description = j["description"].get<std::string>();
    }
    
    if (j.contains("status") && !j["status"].is_null()) {
        TaskStatus status;
        j["status"].get_to(status);
        request.status = status;
    }
    
    if (j.contains("priority") && !j["priority"].is_null()) {
        TaskPriority priority;
        j["priority"].get_to(priority);
        request.priority = priority;
    }
    
    if (j.contains("tags") && !j["tags"].is_null()) {
        request.tags = j["tags"].get<std::vector<std::string>>();
    }
    
    if (j.contains("assigned_to") && !j["assigned_to"].is_null()) {
        request.assigned_to = j["assigned_to"].get<std::string>();
    }
    
    if (j.contains("due_date") && !j["due_date"].is_null()) {
        request.due_date = from_iso_string(j["due_date"].get<std::string>());
    }
}

// JSON conversion for ListTasksResponse
void to_json(nlohmann::json& j, const ListTasksResponse& response) {
    j = nlohmann::json{
        {"tasks", response.tasks},
        {"total_count", response.total_count}
    };
    
    if (response.next_page_token) {
        j["next_page_token"] = *response.next_page_token;
    }
}

// JSON conversion for ErrorResponse
void to_json(nlohmann::json& j, const ErrorResponse& error) {
    j = nlohmann::json{{"message", error.message}};
    
    if (error.errors) {
        j["errors"] = *error.errors;
    }
}

// Utility functions
std::string to_iso_string(const std::chrono::system_clock::time_point& tp) {
    auto time_t = std::chrono::system_clock::to_time_t(tp);
    std::stringstream ss;
    ss << std::put_time(std::gmtime(&time_t), "%Y-%m-%dT%H:%M:%SZ");
    return ss.str();
}

std::chrono::system_clock::time_point from_iso_string(const std::string& str) {
    std::tm tm = {};
    std::istringstream ss(str);
    ss >> std::get_time(&tm, "%Y-%m-%dT%H:%M:%SZ");
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

std::string generate_uuid() {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dis(0, 15);
    static std::uniform_int_distribution<> dis2(8, 11);
    
    std::stringstream ss;
    ss << std::hex;
    
    for (int i = 0; i < 8; i++) {
        ss << dis(gen);
    }
    ss << "-";
    
    for (int i = 0; i < 4; i++) {
        ss << dis(gen);
    }
    ss << "-4";
    
    for (int i = 0; i < 3; i++) {
        ss << dis(gen);
    }
    ss << "-";
    
    ss << dis2(gen);
    for (int i = 0; i < 3; i++) {
        ss << dis(gen);
    }
    ss << "-";
    
    for (int i = 0; i < 12; i++) {
        ss << dis(gen);
    }
    
    return ss.str();
}

std::string trim(const std::string& str) {
    size_t start = str.find_first_not_of(" \t\n\r\f\v");
    if (start == std::string::npos) {
        return "";
    }
    
    size_t end = str.find_last_not_of(" \t\n\r\f\v");
    return str.substr(start, end - start + 1);
}

bool validate_title(const std::string& title) {
    std::string trimmed = trim(title);
    return !trimmed.empty() && trimmed.length() <= 200;
}

} // namespace task_api