#include "api_client.h"
#include <nlohmann/json.hpp>
#include <sstream>
#include <iomanip>
#include <stdexcept>

namespace task_api {

TaskApiClient::TaskApiClient(const std::string& base_url) : base_url_(base_url) {
    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl_ = curl_easy_init();
    if (!curl_) {
        throw std::runtime_error("Failed to initialize CURL");
    }
    
    // Set common options
    curl_easy_setopt(curl_, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl_, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl_, CURLOPT_WRITEFUNCTION, write_callback);
}

TaskApiClient::~TaskApiClient() {
    if (curl_) {
        curl_easy_cleanup(curl_);
    }
    curl_global_cleanup();
}

std::expected<ListTasksResponse, std::string> TaskApiClient::list_tasks(
    std::optional<TaskStatus> status,
    std::optional<std::string> assigned_to,
    std::optional<std::vector<std::string>> tags,
    int page_size,
    std::optional<std::string> page_token,
    const std::string& sort_by,
    const std::string& sort_order
) {
    std::map<std::string, std::string> params;
    
    if (status) {
        params["status"] = task_status_to_string(*status);
    }
    if (assigned_to) {
        params["assigned_to"] = *assigned_to;
    }
    if (tags && !tags->empty()) {
        std::ostringstream oss;
        for (size_t i = 0; i < tags->size(); ++i) {
            if (i > 0) oss << ",";
            oss << (*tags)[i];
        }
        params["tags"] = oss.str();
    }
    if (page_size != 20) {
        params["page_size"] = std::to_string(page_size);
    }
    if (page_token) {
        params["page_token"] = *page_token;
    }
    if (sort_by != "created_at") {
        params["sort_by"] = sort_by;
    }
    if (sort_order != "desc") {
        params["sort_order"] = sort_order;
    }
    
    std::string url = base_url_ + "/api/tasks";
    if (!params.empty()) {
        url += "?" + build_query_string(params);
    }
    
    auto response = perform_request(url, "GET");
    
    if (!response.is_success()) {
        return std::unexpected("HTTP " + std::to_string(response.status_code) + ": " + response.body);
    }
    
    try {
        auto json = nlohmann::json::parse(response.body);
        ListTasksResponse result;
        
        result.total_count = json["total_count"];
        result.page_token = json.value("next_page_token", "");
        
        for (const auto& task_json : json["tasks"]) {
            Task task;
            task.id = task_json["id"];
            task.title = task_json["title"];
            task.description = task_json.value("description", "");
            task.status = string_to_task_status(task_json["status"]);
            task.priority = string_to_task_priority(task_json["priority"]);
            
            if (task_json.contains("tags") && task_json["tags"].is_array()) {
                for (const auto& tag : task_json["tags"]) {
                    task.tags.push_back(tag);
                }
            }
            
            task.assigned_to = task_json.value("assigned_to", "");
            task.created_at = std::chrono::system_clock::now(); // TODO: Parse datetime
            task.updated_at = std::chrono::system_clock::now(); // TODO: Parse datetime
            
            result.tasks.push_back(task);
        }
        
        return result;
    } catch (const std::exception& e) {
        return std::unexpected("JSON parsing error: " + std::string(e.what()));
    }
}

std::expected<Task, std::string> TaskApiClient::get_task(const std::string& id) {
    std::string url = base_url_ + "/api/tasks/" + id;
    auto response = perform_request(url, "GET");
    
    if (response.status_code == 404) {
        return std::unexpected("Task not found");
    }
    
    if (!response.is_success()) {
        return std::unexpected("HTTP " + std::to_string(response.status_code) + ": " + response.body);
    }
    
    try {
        auto json = nlohmann::json::parse(response.body);
        Task task;
        
        task.id = json["id"];
        task.title = json["title"];
        task.description = json.value("description", "");
        task.status = string_to_task_status(json["status"]);
        task.priority = string_to_task_priority(json["priority"]);
        
        if (json.contains("tags") && json["tags"].is_array()) {
            for (const auto& tag : json["tags"]) {
                task.tags.push_back(tag);
            }
        }
        
        task.assigned_to = json.value("assigned_to", "");
        task.created_at = std::chrono::system_clock::now(); // TODO: Parse datetime
        task.updated_at = std::chrono::system_clock::now(); // TODO: Parse datetime
        
        return task;
    } catch (const std::exception& e) {
        return std::unexpected("JSON parsing error: " + std::string(e.what()));
    }
}

std::expected<Task, std::string> TaskApiClient::create_task(const CreateTaskRequest& request) {
    nlohmann::json json_body;
    json_body["title"] = request.title;
    
    if (!request.description.empty()) {
        json_body["description"] = request.description;
    }
    
    json_body["priority"] = task_priority_to_string(request.priority);
    
    if (!request.tags.empty()) {
        json_body["tags"] = request.tags;
    }
    
    if (!request.assigned_to.empty()) {
        json_body["assigned_to"] = request.assigned_to;
    }
    
    std::string url = base_url_ + "/api/tasks";
    std::string body = json_body.dump();
    
    auto response = perform_request(url, "POST", body, {"Content-Type: application/json"});
    
    if (!response.is_success()) {
        return std::unexpected("HTTP " + std::to_string(response.status_code) + ": " + response.body);
    }
    
    try {
        auto json = nlohmann::json::parse(response.body);
        Task task;
        
        task.id = json["id"];
        task.title = json["title"];
        task.description = json.value("description", "");
        task.status = string_to_task_status(json["status"]);
        task.priority = string_to_task_priority(json["priority"]);
        
        if (json.contains("tags") && json["tags"].is_array()) {
            for (const auto& tag : json["tags"]) {
                task.tags.push_back(tag);
            }
        }
        
        task.assigned_to = json.value("assigned_to", "");
        task.created_at = std::chrono::system_clock::now(); // TODO: Parse datetime
        task.updated_at = std::chrono::system_clock::now(); // TODO: Parse datetime
        
        return task;
    } catch (const std::exception& e) {
        return std::unexpected("JSON parsing error: " + std::string(e.what()));
    }
}

std::expected<Task, std::string> TaskApiClient::update_task(const std::string& id, const UpdateTaskRequest& request) {
    nlohmann::json json_body;
    
    if (request.title) {
        json_body["title"] = *request.title;
    }
    if (request.description) {
        json_body["description"] = *request.description;
    }
    if (request.status) {
        json_body["status"] = task_status_to_string(*request.status);
    }
    if (request.priority) {
        json_body["priority"] = task_priority_to_string(*request.priority);
    }
    if (request.tags) {
        json_body["tags"] = *request.tags;
    }
    if (request.assigned_to) {
        json_body["assigned_to"] = *request.assigned_to;
    }
    
    std::string url = base_url_ + "/api/tasks/" + id;
    std::string body = json_body.dump();
    
    auto response = perform_request(url, "PUT", body, {"Content-Type: application/json"});
    
    if (response.status_code == 404) {
        return std::unexpected("Task not found");
    }
    
    if (!response.is_success()) {
        return std::unexpected("HTTP " + std::to_string(response.status_code) + ": " + response.body);
    }
    
    try {
        auto json = nlohmann::json::parse(response.body);
        Task task;
        
        task.id = json["id"];
        task.title = json["title"];
        task.description = json.value("description", "");
        task.status = string_to_task_status(json["status"]);
        task.priority = string_to_task_priority(json["priority"]);
        
        if (json.contains("tags") && json["tags"].is_array()) {
            for (const auto& tag : json["tags"]) {
                task.tags.push_back(tag);
            }
        }
        
        task.assigned_to = json.value("assigned_to", "");
        task.created_at = std::chrono::system_clock::now(); // TODO: Parse datetime
        task.updated_at = std::chrono::system_clock::now(); // TODO: Parse datetime
        
        return task;
    } catch (const std::exception& e) {
        return std::unexpected("JSON parsing error: " + std::string(e.what()));
    }
}

std::expected<Task, std::string> TaskApiClient::update_task_status(const std::string& id, TaskStatus status) {
    UpdateTaskRequest request;
    request.status = status;
    return update_task(id, request);
}

std::expected<bool, std::string> TaskApiClient::delete_task(const std::string& id) {
    std::string url = base_url_ + "/api/tasks/" + id;
    auto response = perform_request(url, "DELETE");
    
    if (response.status_code == 404) {
        return std::unexpected("Task not found");
    }
    
    return response.is_success();
}

HttpResponse TaskApiClient::perform_request(const std::string& url, const std::string& method, 
                                          const std::string& body, const std::vector<std::string>& headers) {
    std::string response_body;
    
    curl_easy_setopt(curl_, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl_, CURLOPT_WRITEDATA, &response_body);
    
    // Set HTTP method
    if (method == "GET") {
        curl_easy_setopt(curl_, CURLOPT_HTTPGET, 1L);
    } else if (method == "POST") {
        curl_easy_setopt(curl_, CURLOPT_POST, 1L);
        curl_easy_setopt(curl_, CURLOPT_POSTFIELDS, body.c_str());
    } else if (method == "PUT") {
        curl_easy_setopt(curl_, CURLOPT_CUSTOMREQUEST, "PUT");
        curl_easy_setopt(curl_, CURLOPT_POSTFIELDS, body.c_str());
    } else if (method == "DELETE") {
        curl_easy_setopt(curl_, CURLOPT_CUSTOMREQUEST, "DELETE");
    }
    
    // Set headers
    struct curl_slist* header_list = nullptr;
    for (const auto& header : headers) {
        header_list = curl_slist_append(header_list, header.c_str());
    }
    if (header_list) {
        curl_easy_setopt(curl_, CURLOPT_HTTPHEADER, header_list);
    }
    
    // Perform request
    CURLcode res = curl_easy_perform(curl_);
    
    // Clean up headers
    if (header_list) {
        curl_slist_free_all(header_list);
    }
    
    long status_code = 0;
    if (res == CURLE_OK) {
        curl_easy_getinfo(curl_, CURLINFO_RESPONSE_CODE, &status_code);
    } else {
        response_body = "CURL error: " + std::string(curl_easy_strerror(res));
        status_code = 0;
    }
    
    return HttpResponse(status_code, response_body);
}

std::string TaskApiClient::build_query_string(const std::map<std::string, std::string>& params) {
    std::ostringstream oss;
    bool first = true;
    
    for (const auto& [key, value] : params) {
        if (!first) {
            oss << "&";
        }
        
        // URL encode key and value
        char* encoded_key = curl_easy_escape(curl_, key.c_str(), key.length());
        char* encoded_value = curl_easy_escape(curl_, value.c_str(), value.length());
        
        oss << encoded_key << "=" << encoded_value;
        
        curl_free(encoded_key);
        curl_free(encoded_value);
        
        first = false;
    }
    
    return oss.str();
}

size_t TaskApiClient::write_callback(void* contents, size_t size, size_t nmemb, std::string* output) {
    size_t total_size = size * nmemb;
    output->append(static_cast<char*>(contents), total_size);
    return total_size;
}

// Utility functions
void print_task(const Task& task) {
    std::cout << "Task ID: " << task.id << std::endl;
    std::cout << "Title: " << task.title << std::endl;
    
    if (!task.description.empty()) {
        std::cout << "Description: " << task.description << std::endl;
    }
    
    std::cout << "Status: " << task_status_to_string(task.status) << std::endl;
    std::cout << "Priority: " << task_priority_to_string(task.priority) << std::endl;
    
    if (!task.tags.empty()) {
        std::cout << "Tags: ";
        for (size_t i = 0; i < task.tags.size(); ++i) {
            if (i > 0) std::cout << ", ";
            std::cout << task.tags[i];
        }
        std::cout << std::endl;
    }
    
    if (!task.assigned_to.empty()) {
        std::cout << "Assigned to: " << task.assigned_to << std::endl;
    }
    
    std::cout << "Created: " << format_datetime(task.created_at) << std::endl;
    std::cout << "Updated: " << format_datetime(task.updated_at) << std::endl;
    std::cout << std::endl;
}

void print_error(const std::string& error) {
    std::cerr << "Error: " << error << std::endl;
}

std::string format_datetime(const std::chrono::system_clock::time_point& tp) {
    auto time_t = std::chrono::system_clock::to_time_t(tp);
    std::stringstream ss;
    ss << std::put_time(std::localtime(&time_t), "%Y-%m-%d %H:%M:%S");
    return ss.str();
}

} // namespace task_api