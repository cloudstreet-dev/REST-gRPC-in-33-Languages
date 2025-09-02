#include <iostream>
#include <memory>
#include <string>
#include <signal.h>
#include <grpcpp/grpcpp.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>
#include <grpcpp/health_check_service_interface.h>
#include "task_service.h"

using grpc::Server;
using grpc::ServerBuilder;

std::unique_ptr<Server> server;

void signal_handler(int signal) {
    std::cout << "\nShutting down server..." << std::endl;
    if (server) {
        server->Shutdown();
    }
}

int main() {
    std::cout << "C++ gRPC Task Server" << std::endl;
    std::cout << "===================" << std::endl;
    
    // Set up signal handlers for graceful shutdown
    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);
    
    std::string server_address("0.0.0.0:50051");
    TaskServiceImpl service;

    grpc::EnableDefaultHealthCheckService(true);
    grpc::reflection::InitProtoReflectionServerBuilderPlugin();
    
    ServerBuilder builder;
    
    // Listen on the given address without any authentication mechanism
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    
    // Register service
    builder.RegisterService(&service);

    // Finally assemble the server
    server = builder.BuildAndStart();
    
    if (!server) {
        std::cerr << "Failed to start server" << std::endl;
        return 1;
    }
    
    std::cout << "Server listening on " << server_address << std::endl;
    std::cout << "gRPC Task Management Service is ready!" << std::endl;
    std::cout << std::endl;
    std::cout << "Available RPCs:" << std::endl;
    std::cout << "- ListTasks: List all tasks with filtering and pagination" << std::endl;
    std::cout << "- GetTask: Get a specific task by ID" << std::endl;
    std::cout << "- CreateTask: Create a new task" << std::endl;
    std::cout << "- UpdateTask: Update an existing task" << std::endl;
    std::cout << "- DeleteTask: Delete a task" << std::endl;
    std::cout << "- UpdateTaskStatus: Update only the status of a task" << std::endl;
    std::cout << std::endl;
    std::cout << "Use Ctrl+C to shutdown gracefully" << std::endl;

    // Wait for the server to shutdown
    server->Wait();
    
    std::cout << "Server stopped." << std::endl;
    return 0;
}