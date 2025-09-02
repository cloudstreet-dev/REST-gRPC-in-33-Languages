#pragma once

#include <grpcpp/grpcpp.h>
#include "tasks.grpc.pb.h"
#include <memory>
#include <string>
#include <expected>

class TaskGrpcClient {
public:
    explicit TaskGrpcClient(const std::string& server_address);
    ~TaskGrpcClient() = default;
    
    // Disable copy constructor and assignment
    TaskGrpcClient(const TaskGrpcClient&) = delete;
    TaskGrpcClient& operator=(const TaskGrpcClient&) = delete;
    
    // Move constructor and assignment
    TaskGrpcClient(TaskGrpcClient&&) = default;
    TaskGrpcClient& operator=(TaskGrpcClient&&) = default;
    
    // gRPC methods
    std::expected<tasks::ListTasksResponse, std::string> list_tasks(
        const tasks::ListTasksRequest& request = {});
    
    std::expected<tasks::Task, std::string> get_task(const std::string& id);
    
    std::expected<tasks::Task, std::string> create_task(
        const tasks::CreateTaskRequest& request);
    
    std::expected<tasks::Task, std::string> update_task(
        const tasks::UpdateTaskRequest& request);
    
    std::expected<bool, std::string> delete_task(const std::string& id);
    
    std::expected<tasks::Task, std::string> update_task_status(
        const std::string& id, tasks::TaskStatus status);

private:
    std::unique_ptr<tasks::TaskService::Stub> stub_;
    
    std::string format_grpc_status(const grpc::Status& status);
};