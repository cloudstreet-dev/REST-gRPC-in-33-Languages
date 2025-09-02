#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <sstream>
#include <chrono>
#include "task_client.h"

void print_usage(const char* program_name) {
    std::cout << "Usage: " << program_name << " [server_address] [command] [args...]" << std::endl;
    std::cout << "Server address defaults to localhost:50051" << std::endl;
    std::cout << "Commands:" << std::endl;
    std::cout << "  demo                    - Run a comprehensive demo" << std::endl;
    std::cout << "  list [status] [assigned_to] - List tasks with optional filters" << std::endl;
    std::cout << "  get <id>                - Get a specific task" << std::endl;
    std::cout << "  create <title> [desc]   - Create a new task" << std::endl;
    std::cout << "  update <id> <title> [desc] - Update a task" << std::endl;
    std::cout << "  status <id> <status>    - Update task status" << std::endl;
    std::cout << "  delete <id>             - Delete a task" << std::endl;
}

void print_task(const tasks::Task& task) {
    std::cout << "Task ID: " << task.id() << std::endl;
    std::cout << "Title: " << task.title() << std::endl;
    
    if (!task.description().empty()) {
        std::cout << "Description: " << task.description() << std::endl;
    }
    
    std::cout << "Status: ";
    switch (task.status()) {
        case tasks::TaskStatus::PENDING:
            std::cout << "PENDING";
            break;
        case tasks::TaskStatus::IN_PROGRESS:
            std::cout << "IN_PROGRESS";
            break;
        case tasks::TaskStatus::COMPLETED:
            std::cout << "COMPLETED";
            break;
        case tasks::TaskStatus::CANCELLED:
            std::cout << "CANCELLED";
            break;
        default:
            std::cout << "UNKNOWN";
            break;
    }
    std::cout << std::endl;
    
    std::cout << "Priority: ";
    switch (task.priority()) {
        case tasks::TaskPriority::LOW:
            std::cout << "LOW";
            break;
        case tasks::TaskPriority::MEDIUM:
            std::cout << "MEDIUM";
            break;
        case tasks::TaskPriority::HIGH:
            std::cout << "HIGH";
            break;
        case tasks::TaskPriority::URGENT:
            std::cout << "URGENT";
            break;
        default:
            std::cout << "UNKNOWN";
            break;
    }
    std::cout << std::endl;
    
    if (!task.tags().empty()) {
        std::cout << "Tags: ";
        for (int i = 0; i < task.tags().size(); ++i) {
            if (i > 0) std::cout << ", ";
            std::cout << task.tags(i);
        }
        std::cout << std::endl;
    }
    
    if (!task.assigned_to().empty()) {
        std::cout << "Assigned to: " << task.assigned_to() << std::endl;
    }
    
    if (task.has_created_at()) {
        std::cout << "Created: " << std::ctime(reinterpret_cast<const time_t*>(&task.created_at().seconds()));
    }
    if (task.has_updated_at()) {
        std::cout << "Updated: " << std::ctime(reinterpret_cast<const time_t*>(&task.updated_at().seconds()));
    }
    
    std::cout << std::endl;
}

void print_error(const std::string& error) {
    std::cerr << "Error: " << error << std::endl;
}

tasks::TaskStatus parse_status(const std::string& status_str) {
    std::string upper_status = status_str;
    std::transform(upper_status.begin(), upper_status.end(), upper_status.begin(), ::toupper);
    
    if (upper_status == "PENDING") return tasks::TaskStatus::PENDING;
    if (upper_status == "IN_PROGRESS" || upper_status == "IN-PROGRESS") return tasks::TaskStatus::IN_PROGRESS;
    if (upper_status == "COMPLETED") return tasks::TaskStatus::COMPLETED;
    if (upper_status == "CANCELLED") return tasks::TaskStatus::CANCELLED;
    
    return tasks::TaskStatus::PENDING; // default
}

int main(int argc, char* argv[]) {
    std::cout << "C++ gRPC Task Client" << std::endl;
    std::cout << "====================" << std::endl;
    
    // Parse arguments
    std::string server_address = "localhost:50051";
    int cmd_start = 1;
    
    if (argc > 1 && std::string(argv[1]).find(':') != std::string::npos) {
        server_address = argv[1];
        cmd_start = 2;
    }
    
    if (argc < cmd_start + 1) {
        print_usage(argv[0]);
        return 1;
    }
    
    std::string command = argv[cmd_start];
    
    try {
        TaskGrpcClient client(server_address);
        std::cout << "Connected to gRPC server at " << server_address << std::endl << std::endl;
        
        if (command == "demo") {
            std::cout << "C++ gRPC Client Demo" << std::endl;
            std::cout << "Running comprehensive gRPC operations..." << std::endl;
            
            // List existing tasks
            std::cout << "\n1. Listing existing tasks..." << std::endl;
            tasks::ListTasksRequest list_request;
            auto list_result = client.list_tasks(list_request);
            if (list_result) {
                const auto& response = *list_result;
                std::cout << "Found " << response.total_count() << " existing tasks:" << std::endl;
                for (const auto& task : response.tasks()) {
                    std::cout << "- [";
                    switch (task.status()) {
                        case tasks::TaskStatus::PENDING: std::cout << "PENDING"; break;
                        case tasks::TaskStatus::IN_PROGRESS: std::cout << "IN_PROGRESS"; break;
                        case tasks::TaskStatus::COMPLETED: std::cout << "COMPLETED"; break;
                        case tasks::TaskStatus::CANCELLED: std::cout << "CANCELLED"; break;
                    }
                    std::cout << "] " << task.title() << " (ID: " << task.id() << ")" << std::endl;
                }
            } else {
                std::cout << "Error listing tasks: " << list_result.error() << std::endl;
            }
            
            // Create a new task
            std::cout << "\n2. Creating a new task..." << std::endl;
            tasks::CreateTaskRequest create_request;
            create_request.set_title("C++ gRPC Client Demo Task");
            create_request.set_description("Testing the C++ gRPC client implementation");
            create_request.set_priority(tasks::TaskPriority::HIGH);
            create_request.add_tags("demo");
            create_request.add_tags("cpp");
            create_request.add_tags("grpc");
            create_request.set_assigned_to("dev-team");
            
            auto create_result = client.create_task(create_request);
            if (create_result) {
                const auto& task = *create_result;
                std::cout << "Created task successfully!" << std::endl;
                print_task(task);
                
                // Get the created task
                std::cout << "3. Retrieving the created task..." << std::endl;
                auto get_result = client.get_task(task.id());
                if (get_result) {
                    std::cout << "Retrieved task:" << std::endl;
                    print_task(*get_result);
                } else {
                    std::cout << "Error getting task: " << get_result.error() << std::endl;
                }
                
                // Update task status
                std::cout << "4. Updating task status to IN_PROGRESS..." << std::endl;
                auto status_result = client.update_task_status(task.id(), tasks::TaskStatus::IN_PROGRESS);
                if (status_result) {
                    std::cout << "Status updated successfully!" << std::endl;
                    print_task(*status_result);
                } else {
                    std::cout << "Error updating status: " << status_result.error() << std::endl;
                }
                
                // Update task with more fields
                std::cout << "5. Updating task description..." << std::endl;
                tasks::UpdateTaskRequest update_request;
                update_request.set_id(task.id());
                update_request.set_title("Updated C++ gRPC Demo Task");
                update_request.set_description("Updated description from C++ gRPC client");
                update_request.set_priority(tasks::TaskPriority::MEDIUM);
                
                auto update_result = client.update_task(update_request);
                if (update_result) {
                    std::cout << "Task updated successfully!" << std::endl;
                    print_task(*update_result);
                } else {
                    std::cout << "Error updating task: " << update_result.error() << std::endl;
                }
                
                // List tasks with filter
                std::cout << "6. Listing tasks with status filter (IN_PROGRESS)..." << std::endl;
                tasks::ListTasksRequest filtered_request;
                filtered_request.set_status(tasks::TaskStatus::IN_PROGRESS);
                auto filtered_result = client.list_tasks(filtered_request);
                if (filtered_result) {
                    std::cout << "Found " << filtered_result->total_count() 
                              << " tasks in progress:" << std::endl;
                    for (const auto& task : filtered_result->tasks()) {
                        std::cout << "- " << task.title() << " (ID: " << task.id() << ")" << std::endl;
                    }
                } else {
                    std::cout << "Error filtering tasks: " << filtered_result.error() << std::endl;
                }
                
                // Complete the task
                std::cout << "7. Completing the demo task..." << std::endl;
                auto complete_result = client.update_task_status(task.id(), tasks::TaskStatus::COMPLETED);
                if (complete_result) {
                    std::cout << "Task completed!" << std::endl;
                    print_task(*complete_result);
                } else {
                    std::cout << "Error completing task: " << complete_result.error() << std::endl;
                }
                
                // Finally delete the task
                std::cout << "8. Cleaning up - deleting demo task..." << std::endl;
                auto delete_result = client.delete_task(task.id());
                if (delete_result && *delete_result) {
                    std::cout << "Demo task deleted successfully!" << std::endl;
                } else {
                    std::cout << "Error deleting task: " 
                              << (delete_result ? "Delete failed" : delete_result.error()) << std::endl;
                }
                
            } else {
                std::cout << "Error creating task: " << create_result.error() << std::endl;
            }
            
        } else if (command == "list") {
            tasks::ListTasksRequest request;
            
            if (argc > cmd_start + 1) {
                request.set_status(parse_status(argv[cmd_start + 1]));
            }
            if (argc > cmd_start + 2) {
                request.set_assigned_to(argv[cmd_start + 2]);
            }
            
            auto result = client.list_tasks(request);
            if (result) {
                std::cout << "\nFound " << result->total_count() << " tasks:" << std::endl;
                for (const auto& task : result->tasks()) {
                    std::cout << "- " << task.id() << ": " << task.title() << " [";
                    switch (task.status()) {
                        case tasks::TaskStatus::PENDING: std::cout << "PENDING"; break;
                        case tasks::TaskStatus::IN_PROGRESS: std::cout << "IN_PROGRESS"; break;
                        case tasks::TaskStatus::COMPLETED: std::cout << "COMPLETED"; break;
                        case tasks::TaskStatus::CANCELLED: std::cout << "CANCELLED"; break;
                    }
                    std::cout << "]" << std::endl;
                }
            } else {
                print_error(result.error());
            }
            
        } else if (command == "get") {
            if (argc < cmd_start + 2) {
                std::cout << "Usage: " << argv[0] << " get <id>" << std::endl;
                return 1;
            }
            
            auto result = client.get_task(argv[cmd_start + 1]);
            if (result) {
                print_task(*result);
            } else {
                print_error(result.error());
            }
            
        } else if (command == "create") {
            if (argc < cmd_start + 2) {
                std::cout << "Usage: " << argv[0] << " create <title> [description]" << std::endl;
                return 1;
            }
            
            tasks::CreateTaskRequest request;
            request.set_title(argv[cmd_start + 1]);
            if (argc > cmd_start + 2) {
                request.set_description(argv[cmd_start + 2]);
            }
            
            auto result = client.create_task(request);
            if (result) {
                std::cout << "Task created successfully!" << std::endl;
                print_task(*result);
            } else {
                print_error(result.error());
            }
            
        } else if (command == "status") {
            if (argc < cmd_start + 3) {
                std::cout << "Usage: " << argv[0] << " status <id> <status>" << std::endl;
                std::cout << "Status options: PENDING, IN_PROGRESS, COMPLETED, CANCELLED" << std::endl;
                return 1;
            }
            
            tasks::TaskStatus status = parse_status(argv[cmd_start + 2]);
            auto result = client.update_task_status(argv[cmd_start + 1], status);
            if (result) {
                std::cout << "Task status updated successfully!" << std::endl;
                print_task(*result);
            } else {
                print_error(result.error());
            }
            
        } else if (command == "delete") {
            if (argc < cmd_start + 2) {
                std::cout << "Usage: " << argv[0] << " delete <id>" << std::endl;
                return 1;
            }
            
            auto result = client.delete_task(argv[cmd_start + 1]);
            if (result && *result) {
                std::cout << "Task deleted successfully!" << std::endl;
            } else {
                print_error(result ? "Delete failed" : result.error());
            }
            
        } else {
            std::cout << "Unknown command: " << command << std::endl;
            print_usage(argv[0]);
            return 1;
        }
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}