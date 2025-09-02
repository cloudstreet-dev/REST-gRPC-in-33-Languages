#pragma once

#include <grpcpp/grpcpp.h>
#include "tasks.grpc.pb.h"
#include <unordered_map>
#include <mutex>
#include <string>
#include <memory>

class TaskServiceImpl final : public tasks::TaskService::Service {
public:
    TaskServiceImpl();
    
    grpc::Status ListTasks(grpc::ServerContext* context, 
                          const tasks::ListTasksRequest* request,
                          tasks::ListTasksResponse* response) override;
    
    grpc::Status GetTask(grpc::ServerContext* context, 
                        const tasks::GetTaskRequest* request,
                        tasks::Task* response) override;
    
    grpc::Status CreateTask(grpc::ServerContext* context, 
                           const tasks::CreateTaskRequest* request,
                           tasks::Task* response) override;
    
    grpc::Status UpdateTask(grpc::ServerContext* context, 
                           const tasks::UpdateTaskRequest* request,
                           tasks::Task* response) override;
    
    grpc::Status DeleteTask(grpc::ServerContext* context, 
                           const tasks::DeleteTaskRequest* request,
                           tasks::DeleteTaskResponse* response) override;
    
    grpc::Status UpdateTaskStatus(grpc::ServerContext* context, 
                                 const tasks::UpdateTaskStatusRequest* request,
                                 tasks::Task* response) override;

private:
    std::unordered_map<std::string, std::unique_ptr<tasks::Task>> tasks_;
    std::mutex tasks_mutex_;
    std::atomic<int> next_id_;
    
    std::string generate_id();
    std::unique_ptr<tasks::Task> create_task_copy(const tasks::Task& task);
    bool matches_filter(const tasks::Task& task, const tasks::ListTasksRequest& request);
};