#include "handlers.h"
#include "database.h"
#include <crow.h>
#include <iostream>
#include <memory>

using namespace task_api;

int main() {
    try {
        std::cout << "C++ Task REST Server" << std::endl;
        std::cout << "====================" << std::endl;
        
        // Initialize database
        auto db = std::make_shared<TaskDatabase>("tasks.db");
        if (!db->initialize()) {
            std::cerr << "Failed to initialize database" << std::endl;
            return 1;
        }
        
        // Create sample data
        if (!db->create_sample_data()) {
            std::cerr << "Failed to create sample data" << std::endl;
            return 1;
        }
        
        std::cout << "Database initialized successfully" << std::endl;
        
        // Create Crow app with CORS middleware
        crow::App<crow::CORSHandler> app;
        
        // Initialize handlers
        TaskHandlers handlers(db);
        handlers.register_routes(app);
        
        // Add health check endpoint
        CROW_ROUTE(app, "/health")
        ([]() {
            return crow::response(200, "OK");
        });
        
        // Add root endpoint with API info
        CROW_ROUTE(app, "/")
        ([]() {
            nlohmann::json info = {
                {"name", "C++ Task Management API"},
                {"version", "1.0.0"},
                {"description", "A REST API for task management built with C++ and Crow"},
                {"endpoints", {
                    {"GET /api/tasks", "List all tasks with optional filtering"},
                    {"GET /api/tasks/{id}", "Get a specific task"},
                    {"POST /api/tasks", "Create a new task"},
                    {"PUT /api/tasks/{id}", "Update a task"},
                    {"PATCH /api/tasks/{id}/status", "Update task status"},
                    {"DELETE /api/tasks/{id}", "Delete a task"}
                }}
            };
            
            crow::response res(200);
            res.set_header("Content-Type", "application/json");
            res.write(info.dump(2));
            return res;
        });
        
        std::cout << "Starting server on http://localhost:8080" << std::endl;
        std::cout << "API endpoints available at http://localhost:8080/api/tasks" << std::endl;
        std::cout << "Health check available at http://localhost:8080/health" << std::endl;
        std::cout << "Press Ctrl+C to stop the server" << std::endl;
        
        // Start the server
        app.port(8080)
           .multithreaded()
           .run();
        
    } catch (const std::exception& e) {
        std::cerr << "Server error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}