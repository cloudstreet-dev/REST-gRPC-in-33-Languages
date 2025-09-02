#include "task_client.h"
#include <grpcpp/create_channel.h>

TaskGrpcClient::TaskGrpcClient(const std::string& server_address) {
    auto channel = grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials());
    stub_ = tasks::TaskService::NewStub(channel);
}

std::expected<tasks::ListTasksResponse, std::string> TaskGrpcClient::list_tasks(
    const tasks::ListTasksRequest& request) {
    
    tasks::ListTasksResponse response;
    grpc::ClientContext context;
    
    grpc::Status status = stub_->ListTasks(&context, request, &response);
    
    if (status.ok()) {
        return response;
    } else {
        return std::unexpected(format_grpc_status(status));
    }
}

std::expected<tasks::Task, std::string> TaskGrpcClient::get_task(const std::string& id) {
    tasks::GetTaskRequest request;
    request.set_id(id);
    
    tasks::Task response;
    grpc::ClientContext context;
    
    grpc::Status status = stub_->GetTask(&context, request, &response);
    
    if (status.ok()) {
        return response;
    } else {
        return std::unexpected(format_grpc_status(status));
    }
}

std::expected<tasks::Task, std::string> TaskGrpcClient::create_task(
    const tasks::CreateTaskRequest& request) {
    
    tasks::Task response;
    grpc::ClientContext context;
    
    grpc::Status status = stub_->CreateTask(&context, request, &response);
    
    if (status.ok()) {
        return response;
    } else {
        return std::unexpected(format_grpc_status(status));
    }
}

std::expected<tasks::Task, std::string> TaskGrpcClient::update_task(
    const tasks::UpdateTaskRequest& request) {
    
    tasks::Task response;
    grpc::ClientContext context;
    
    grpc::Status status = stub_->UpdateTask(&context, request, &response);
    
    if (status.ok()) {
        return response;
    } else {
        return std::unexpected(format_grpc_status(status));
    }
}

std::expected<bool, std::string> TaskGrpcClient::delete_task(const std::string& id) {
    tasks::DeleteTaskRequest request;
    request.set_id(id);
    
    tasks::DeleteTaskResponse response;
    grpc::ClientContext context;
    
    grpc::Status status = stub_->DeleteTask(&context, request, &response);
    
    if (status.ok()) {
        return response.success();
    } else {
        return std::unexpected(format_grpc_status(status));
    }
}

std::expected<tasks::Task, std::string> TaskGrpcClient::update_task_status(
    const std::string& id, tasks::TaskStatus status) {
    
    tasks::UpdateTaskStatusRequest request;
    request.set_id(id);
    request.set_status(status);
    
    tasks::Task response;
    grpc::ClientContext context;
    
    grpc::Status grpc_status = stub_->UpdateTaskStatus(&context, request, &response);
    
    if (grpc_status.ok()) {
        return response;
    } else {
        return std::unexpected(format_grpc_status(grpc_status));
    }
}

std::string TaskGrpcClient::format_grpc_status(const grpc::Status& status) {
    return "gRPC error [" + std::to_string(static_cast<int>(status.error_code())) + 
           "]: " + status.error_message();
}