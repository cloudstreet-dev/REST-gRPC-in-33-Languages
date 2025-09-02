#include "task_service.h"
#include <algorithm>
#include <chrono>
#include <sstream>
#include <iomanip>

TaskServiceImpl::TaskServiceImpl() : next_id_(1) {
    // Create a few sample tasks
    auto now = std::chrono::system_clock::now();
    auto timestamp = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch()).count();
    
    // Sample task 1
    auto task1 = std::make_unique<tasks::Task>();
    task1->set_id("1");
    task1->set_title("Implement gRPC server");
    task1->set_description("Create a gRPC server for the task management API");
    task1->set_status(tasks::TaskStatus::IN_PROGRESS);
    task1->set_priority(tasks::TaskPriority::HIGH);
    task1->add_tags("grpc");
    task1->add_tags("server");
    task1->add_tags("cpp");
    task1->set_assigned_to("dev-team");
    task1->mutable_created_at()->set_seconds(timestamp - 3600);
    task1->mutable_updated_at()->set_seconds(timestamp - 1800);
    
    // Sample task 2
    auto task2 = std::make_unique<tasks::Task>();
    task2->set_id("2");
    task2->set_title("Write unit tests");
    task2->set_description("Add comprehensive unit tests for the task service");
    task2->set_status(tasks::TaskStatus::PENDING);
    task2->set_priority(tasks::TaskPriority::MEDIUM);
    task2->add_tags("testing");
    task2->add_tags("unit-tests");
    task2->set_assigned_to("qa-team");
    task2->mutable_created_at()->set_seconds(timestamp - 7200);
    task2->mutable_updated_at()->set_seconds(timestamp - 7200);
    
    tasks_["1"] = std::move(task1);
    tasks_["2"] = std::move(task2);
    next_id_ = 3;
}

grpc::Status TaskServiceImpl::ListTasks(grpc::ServerContext* context, 
                                       const tasks::ListTasksRequest* request,
                                       tasks::ListTasksResponse* response) {
    std::lock_guard<std::mutex> lock(tasks_mutex_);
    
    std::vector<const tasks::Task*> filtered_tasks;
    
    // Filter tasks based on request criteria
    for (const auto& [id, task] : tasks_) {
        if (matches_filter(*task, *request)) {
            filtered_tasks.push_back(task.get());
        }
    }
    
    // Sort tasks (simplified - by ID for now)
    std::sort(filtered_tasks.begin(), filtered_tasks.end(),
              [](const tasks::Task* a, const tasks::Task* b) {
                  return a->id() < b->id();
              });
    
    // Apply pagination
    int page_size = request->page_size() > 0 ? std::min(request->page_size(), 100) : 20;
    int start_idx = 0;
    
    if (!request->page_token().empty()) {
        try {
            start_idx = std::stoi(request->page_token());
        } catch (const std::exception&) {
            return grpc::Status(grpc::StatusCode::INVALID_ARGUMENT, "Invalid page token");
        }
    }
    
    int end_idx = std::min(start_idx + page_size, static_cast<int>(filtered_tasks.size()));
    
    // Add tasks to response
    for (int i = start_idx; i < end_idx; ++i) {
        *response->add_tasks() = *filtered_tasks[i];
    }
    
    response->set_total_count(filtered_tasks.size());
    
    // Set next page token if there are more results
    if (end_idx < static_cast<int>(filtered_tasks.size())) {
        response->set_next_page_token(std::to_string(end_idx));
    }
    
    return grpc::Status::OK;
}

grpc::Status TaskServiceImpl::GetTask(grpc::ServerContext* context, 
                                     const tasks::GetTaskRequest* request,
                                     tasks::Task* response) {
    std::lock_guard<std::mutex> lock(tasks_mutex_);
    
    auto it = tasks_.find(request->id());
    if (it == tasks_.end()) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND, "Task not found");
    }
    
    *response = *it->second;
    return grpc::Status::OK;
}

grpc::Status TaskServiceImpl::CreateTask(grpc::ServerContext* context, 
                                        const tasks::CreateTaskRequest* request,
                                        tasks::Task* response) {
    if (request->title().empty()) {
        return grpc::Status(grpc::StatusCode::INVALID_ARGUMENT, "Title is required");
    }
    
    std::lock_guard<std::mutex> lock(tasks_mutex_);
    
    auto task = std::make_unique<tasks::Task>();
    task->set_id(generate_id());
    task->set_title(request->title());
    
    if (!request->description().empty()) {
        task->set_description(request->description());
    }
    
    task->set_status(tasks::TaskStatus::PENDING);
    
    if (request->has_priority()) {
        task->set_priority(request->priority());
    } else {
        task->set_priority(tasks::TaskPriority::MEDIUM);
    }
    
    // Copy tags
    for (const auto& tag : request->tags()) {
        task->add_tags(tag);
    }
    
    if (!request->assigned_to().empty()) {
        task->set_assigned_to(request->assigned_to());
    }
    
    // Set timestamps
    auto now = std::chrono::system_clock::now();
    auto timestamp = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch()).count();
    task->mutable_created_at()->set_seconds(timestamp);
    task->mutable_updated_at()->set_seconds(timestamp);
    
    std::string task_id = task->id();
    *response = *task;
    
    tasks_[task_id] = std::move(task);
    
    return grpc::Status::OK;
}

grpc::Status TaskServiceImpl::UpdateTask(grpc::ServerContext* context, 
                                        const tasks::UpdateTaskRequest* request,
                                        tasks::Task* response) {
    std::lock_guard<std::mutex> lock(tasks_mutex_);
    
    auto it = tasks_.find(request->id());
    if (it == tasks_.end()) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND, "Task not found");
    }
    
    auto& task = *it->second;
    
    // Update fields based on field mask (simplified - update all provided fields)
    if (!request->title().empty()) {
        task.set_title(request->title());
    }
    
    if (!request->description().empty()) {
        task.set_description(request->description());
    }
    
    if (request->has_status()) {
        task.set_status(request->status());
    }
    
    if (request->has_priority()) {
        task.set_priority(request->priority());
    }
    
    if (!request->tags().empty()) {
        task.clear_tags();
        for (const auto& tag : request->tags()) {
            task.add_tags(tag);
        }
    }
    
    if (!request->assigned_to().empty()) {
        task.set_assigned_to(request->assigned_to());
    }
    
    // Update timestamp
    auto now = std::chrono::system_clock::now();
    auto timestamp = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch()).count();
    task.mutable_updated_at()->set_seconds(timestamp);
    
    *response = task;
    return grpc::Status::OK;
}

grpc::Status TaskServiceImpl::DeleteTask(grpc::ServerContext* context, 
                                        const tasks::DeleteTaskRequest* request,
                                        tasks::DeleteTaskResponse* response) {
    std::lock_guard<std::mutex> lock(tasks_mutex_);
    
    auto it = tasks_.find(request->id());
    if (it == tasks_.end()) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND, "Task not found");
    }
    
    tasks_.erase(it);
    response->set_success(true);
    
    return grpc::Status::OK;
}

grpc::Status TaskServiceImpl::UpdateTaskStatus(grpc::ServerContext* context, 
                                              const tasks::UpdateTaskStatusRequest* request,
                                              tasks::Task* response) {
    std::lock_guard<std::mutex> lock(tasks_mutex_);
    
    auto it = tasks_.find(request->id());
    if (it == tasks_.end()) {
        return grpc::Status(grpc::StatusCode::NOT_FOUND, "Task not found");
    }
    
    auto& task = *it->second;
    task.set_status(request->status());
    
    // Update timestamp
    auto now = std::chrono::system_clock::now();
    auto timestamp = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch()).count();
    task.mutable_updated_at()->set_seconds(timestamp);
    
    *response = task;
    return grpc::Status::OK;
}

std::string TaskServiceImpl::generate_id() {
    return std::to_string(next_id_++);
}

std::unique_ptr<tasks::Task> TaskServiceImpl::create_task_copy(const tasks::Task& task) {
    auto copy = std::make_unique<tasks::Task>();
    *copy = task;
    return copy;
}

bool TaskServiceImpl::matches_filter(const tasks::Task& task, const tasks::ListTasksRequest& request) {
    // Filter by status
    if (request.has_status() && task.status() != request.status()) {
        return false;
    }
    
    // Filter by assigned_to
    if (!request.assigned_to().empty() && task.assigned_to() != request.assigned_to()) {
        return false;
    }
    
    // Filter by tags (task must have all requested tags)
    for (const auto& requested_tag : request.tags()) {
        bool tag_found = false;
        for (const auto& task_tag : task.tags()) {
            if (task_tag == requested_tag) {
                tag_found = true;
                break;
            }
        }
        if (!tag_found) {
            return false;
        }
    }
    
    return true;
}