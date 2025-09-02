#include <iostream>
#include <string>
#include <vector>
#include "api_client.h"

using namespace task_api;

void print_usage(const char* program_name) {
    std::cout << "Usage: " << program_name << " [command] [args...]" << std::endl;
    std::cout << "Commands:" << std::endl;
    std::cout << "  demo                    - Run a comprehensive demo" << std::endl;
    std::cout << "  list [status]           - List tasks (optionally filter by status)" << std::endl;
    std::cout << "  get <id>                - Get a specific task" << std::endl;
    std::cout << "  create <title> [desc]   - Create a new task" << std::endl;
    std::cout << "  update <id> <field> <value> - Update a task field" << std::endl;
    std::cout << "  status <id> <status>    - Update task status" << std::endl;
    std::cout << "  delete <id>             - Delete a task" << std::endl;
}

int main(int argc, char* argv[]) {
    std::cout << "C++ Task REST Client" << std::endl;
    std::cout << "====================" << std::endl;
    
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }
    
    std::string command = argv[1];
    
    try {
        TaskApiClient client("http://localhost:8080");
        
        if (command == "demo") {
            std::cout << "\nC++ REST Client Demo" << std::endl;
            std::cout << "Running comprehensive API operations..." << std::endl;
            
            // List existing tasks
            std::cout << "\n1. Listing existing tasks..." << std::endl;
            auto list_result = client.list_tasks();
            if (list_result) {
                const auto& tasks = list_result->tasks;
                std::cout << "Found " << list_result->total_count << " existing tasks:" << std::endl;
                for (const auto& task : tasks) {
                    std::cout << "- [" << task_status_to_string(task.status) << "] " 
                              << task.title << " (ID: " << task.id << ")" << std::endl;
                }
            } else {
                std::cout << "Error listing tasks: " << list_result.error() << std::endl;
            }
            
            // Create a new task
            std::cout << "\n2. Creating a new task..." << std::endl;
            CreateTaskRequest create_req;
            create_req.title = "C++ REST Client Demo Task";
            create_req.description = "Testing the C++ REST client implementation";
            create_req.priority = TaskPriority::High;
            create_req.tags = {"demo", "cpp", "rest"};
            create_req.assigned_to = "dev-team";
            
            auto create_result = client.create_task(create_req);
            if (create_result) {
                const auto& task = *create_result;
                std::cout << "Created task successfully!" << std::endl;
                print_task(task);
                
                // Get the created task
                std::cout << "3. Retrieving the created task..." << std::endl;
                auto get_result = client.get_task(task.id);
                if (get_result) {
                    std::cout << "Retrieved task:" << std::endl;
                    print_task(*get_result);
                } else {
                    std::cout << "Error getting task: " << get_result.error() << std::endl;
                }
                
                // Update task status
                std::cout << "4. Updating task status to In Progress..." << std::endl;
                auto status_result = client.update_task_status(task.id, TaskStatus::InProgress);
                if (status_result) {
                    std::cout << "Status updated successfully!" << std::endl;
                    print_task(*status_result);
                } else {
                    std::cout << "Error updating status: " << status_result.error() << std::endl;
                }
                
                // Update task with more fields
                std::cout << "5. Updating task description..." << std::endl;
                UpdateTaskRequest update_req;
                update_req.description = "Updated description from C++ client";
                update_req.priority = TaskPriority::Medium;
                
                auto update_result = client.update_task(task.id, update_req);
                if (update_result) {
                    std::cout << "Task updated successfully!" << std::endl;
                    print_task(*update_result);
                } else {
                    std::cout << "Error updating task: " << update_result.error() << std::endl;
                }
                
                // List tasks with filter
                std::cout << "6. Listing tasks with status filter (in_progress)..." << std::endl;
                auto filtered_result = client.list_tasks(TaskStatus::InProgress);
                if (filtered_result) {
                    std::cout << "Found " << filtered_result->total_count 
                              << " tasks in progress:" << std::endl;
                    for (const auto& task : filtered_result->tasks) {
                        std::cout << "- " << task.title << " (ID: " << task.id << ")" << std::endl;
                    }
                } else {
                    std::cout << "Error filtering tasks: " << filtered_result.error() << std::endl;
                }
                
                // Complete the task
                std::cout << "7. Completing the demo task..." << std::endl;
                auto complete_result = client.update_task_status(task.id, TaskStatus::Completed);
                if (complete_result) {
                    std::cout << "Task completed!" << std::endl;
                    print_task(*complete_result);
                } else {
                    std::cout << "Error completing task: " << complete_result.error() << std::endl;
                }
                
                // Finally delete the task
                std::cout << "8. Cleaning up - deleting demo task..." << std::endl;
                auto delete_result = client.delete_task(task.id);
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
            std::optional<TaskStatus> status_filter;
            if (argc > 2) {
                status_filter = string_to_task_status(argv[2]);
            }
            
            auto result = client.list_tasks(status_filter);
            if (result) {
                std::cout << "\nFound " << result->total_count << " tasks:" << std::endl;
                for (const auto& task : result->tasks) {
                    std::cout << "- " << task.id << ": " << task.title 
                              << " [" << task_status_to_string(task.status) << "]" << std::endl;
                }
            } else {
                print_error(result.error());
            }
            
        } else if (command == "get") {
            if (argc < 3) {
                std::cout << "Usage: " << argv[0] << " get <id>" << std::endl;
                return 1;
            }
            
            auto result = client.get_task(argv[2]);
            if (result) {
                print_task(*result);
            } else {
                print_error(result.error());
            }
            
        } else if (command == "create") {
            if (argc < 3) {
                std::cout << "Usage: " << argv[0] << " create <title> [description]" << std::endl;
                return 1;
            }
            
            CreateTaskRequest request;
            request.title = argv[2];
            if (argc > 3) {
                request.description = argv[3];
            }
            
            auto result = client.create_task(request);
            if (result) {
                std::cout << "Task created successfully!" << std::endl;
                print_task(*result);
            } else {
                print_error(result.error());
            }
            
        } else if (command == "status") {
            if (argc < 4) {
                std::cout << "Usage: " << argv[0] << " status <id> <status>" << std::endl;
                std::cout << "Status options: pending, in_progress, completed, cancelled" << std::endl;
                return 1;
            }
            
            TaskStatus status = string_to_task_status(argv[3]);
            auto result = client.update_task_status(argv[2], status);
            if (result) {
                std::cout << "Task status updated successfully!" << std::endl;
                print_task(*result);
            } else {
                print_error(result.error());
            }
            
        } else if (command == "delete") {
            if (argc < 3) {
                std::cout << "Usage: " << argv[0] << " delete <id>" << std::endl;
                return 1;
            }
            
            auto result = client.delete_task(argv[2]);
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