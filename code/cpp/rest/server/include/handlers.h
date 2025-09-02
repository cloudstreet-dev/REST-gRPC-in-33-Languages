#pragma once

#include "database.h"
#include <crow.h>
#include <memory>

namespace task_api {

class TaskHandlers {
public:
    explicit TaskHandlers(std::shared_ptr<TaskDatabase> db);
    
    // Register all routes
    void register_routes(crow::App<>& app);
    
private:
    std::shared_ptr<TaskDatabase> db_;
    
    // Route handlers
    crow::response list_tasks(const crow::request& req);
    crow::response get_task(const crow::request& req, const std::string& id);
    crow::response create_task(const crow::request& req);
    crow::response update_task(const crow::request& req, const std::string& id);
    crow::response update_task_status(const crow::request& req, const std::string& id);
    crow::response delete_task(const crow::request& req, const std::string& id);
    
    // Helper functions
    TaskFilters parse_query_params(const crow::request& req);
    crow::response make_error_response(int status_code, const std::string& message);
    crow::response make_json_response(const nlohmann::json& json, int status_code = 200);
};

} // namespace task_api