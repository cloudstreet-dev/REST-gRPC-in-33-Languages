#include "handlers.h"
#include <iostream>
#include <sstream>
#include <algorithm>

namespace task_api {

TaskHandlers::TaskHandlers(std::shared_ptr<TaskDatabase> db) : db_(db) {}

void TaskHandlers::register_routes(crow::App<>& app) {
    // Enable CORS
    app.get_middleware<crow::CORSHandler>().global()
        .headers("Content-Type", "Authorization")
        .methods("GET"_method, "POST"_method, "PUT"_method, "DELETE"_method, "PATCH"_method, "OPTIONS"_method)
        .prefix("/api");
    
    // GET /api/tasks - List all tasks with filtering
    CROW_ROUTE(app, "/api/tasks").methods("GET"_method)
    ([this](const crow::request& req) {
        return list_tasks(req);
    });
    
    // GET /api/tasks/<id> - Get specific task
    CROW_ROUTE(app, "/api/tasks/<string>").methods("GET"_method)
    ([this](const crow::request& req, const std::string& id) {
        return get_task(req, id);
    });
    
    // POST /api/tasks - Create new task
    CROW_ROUTE(app, "/api/tasks").methods("POST"_method)
    ([this](const crow::request& req) {
        return create_task(req);
    });
    
    // PUT /api/tasks/<id> - Update task
    CROW_ROUTE(app, "/api/tasks/<string>").methods("PUT"_method)
    ([this](const crow::request& req, const std::string& id) {
        return update_task(req, id);
    });
    
    // PATCH /api/tasks/<id>/status - Update task status
    CROW_ROUTE(app, "/api/tasks/<string>/status").methods("PATCH"_method)
    ([this](const crow::request& req, const std::string& id) {
        return update_task_status(req, id);
    });
    
    // DELETE /api/tasks/<id> - Delete task
    CROW_ROUTE(app, "/api/tasks/<string>").methods("DELETE"_method)
    ([this](const crow::request& req, const std::string& id) {
        return delete_task(req, id);
    });
}

crow::response TaskHandlers::list_tasks(const crow::request& req) {
    try {
        TaskFilters filters = parse_query_params(req);
        
        // Validate page size
        if (filters.page_size > 100) {
            filters.page_size = 100;
        }
        
        // Get tasks and total count
        auto tasks = db_->list_tasks(filters);
        int total_count = db_->count_tasks(filters);
        
        // Build response
        ListTasksResponse response;
        response.tasks = std::move(tasks);
        response.total_count = total_count;
        
        // Calculate next page token
        int offset = 0;
        if (filters.page_token && !filters.page_token->empty()) {
            try {
                offset = std::stoi(*filters.page_token);
            } catch (...) {
                offset = 0;
            }
        }
        
        if (offset + filters.page_size < total_count) {
            response.next_page_token = std::to_string(offset + filters.page_size);
        }
        
        nlohmann::json json = response;
        return make_json_response(json);
        
    } catch (const std::exception& e) {
        std::cerr << "Error in list_tasks: " << e.what() << std::endl;
        return make_error_response(500, "Internal server error");
    }
}

crow::response TaskHandlers::get_task(const crow::request& req, const std::string& id) {
    try {
        auto task = db_->get_task(id);
        if (!task) {
            return make_error_response(404, "Task with ID '" + id + "' not found");
        }
        
        nlohmann::json json = *task;
        return make_json_response(json);
        
    } catch (const std::exception& e) {
        std::cerr << "Error in get_task: " << e.what() << std::endl;
        return make_error_response(500, "Internal server error");
    }
}

crow::response TaskHandlers::create_task(const crow::request& req) {
    try {
        // Parse request body
        nlohmann::json json_req = nlohmann::json::parse(req.body);
        CreateTaskRequest request = json_req;
        
        // Validate request
        if (!validate_title(request.title)) {
            return make_error_response(400, "Title is required and must be 200 characters or less");
        }
        
        // Create task
        Task task;
        task.id = generate_uuid();
        task.title = trim(request.title);
        task.description = request.description;
        task.priority = request.priority.value_or(TaskPriority::Medium);
        task.tags = request.tags.value_or(std::vector<std::string>{});
        task.assigned_to = request.assigned_to;
        task.created_by = request.created_by.value_or("system");
        task.due_date = request.due_date;
        
        // Save to database
        if (!db_->create_task(task)) {
            return make_error_response(500, "Failed to create task");
        }
        
        nlohmann::json json = task;
        return make_json_response(json, 201);
        
    } catch (const nlohmann::json::parse_error& e) {
        return make_error_response(400, "Invalid JSON in request body");
    } catch (const nlohmann::json::type_error& e) {
        return make_error_response(400, "Invalid request format: " + std::string(e.what()));
    } catch (const std::exception& e) {
        std::cerr << "Error in create_task: " << e.what() << std::endl;
        return make_error_response(500, "Internal server error");
    }
}

crow::response TaskHandlers::update_task(const crow::request& req, const std::string& id) {
    try {
        // Check if task exists
        auto existing_task = db_->get_task(id);
        if (!existing_task) {
            return make_error_response(404, "Task with ID '" + id + "' not found");
        }
        
        // Parse request body
        nlohmann::json json_req = nlohmann::json::parse(req.body);
        UpdateTaskRequest request = json_req;
        
        // Validate title if provided
        if (request.title && !validate_title(*request.title)) {
            return make_error_response(400, "Title must be non-empty and 200 characters or less");
        }
        
        // Update task
        if (!db_->update_task(id, request)) {
            return make_error_response(500, "Failed to update task");
        }
        
        // Get updated task
        auto updated_task = db_->get_task(id);
        if (!updated_task) {
            return make_error_response(500, "Failed to retrieve updated task");
        }
        
        nlohmann::json json = *updated_task;
        return make_json_response(json);
        
    } catch (const nlohmann::json::parse_error& e) {
        return make_error_response(400, "Invalid JSON in request body");
    } catch (const nlohmann::json::type_error& e) {
        return make_error_response(400, "Invalid request format: " + std::string(e.what()));
    } catch (const std::exception& e) {
        std::cerr << "Error in update_task: " << e.what() << std::endl;
        return make_error_response(500, "Internal server error");
    }
}

crow::response TaskHandlers::update_task_status(const crow::request& req, const std::string& id) {
    try {
        // Check if task exists
        auto existing_task = db_->get_task(id);
        if (!existing_task) {
            return make_error_response(404, "Task with ID '" + id + "' not found");
        }
        
        // Parse request body
        nlohmann::json json_req = nlohmann::json::parse(req.body);
        TaskStatus status = json_req;
        
        // Create update request with only status
        UpdateTaskRequest request;
        request.status = status;
        
        // Update task
        if (!db_->update_task(id, request)) {
            return make_error_response(500, "Failed to update task status");
        }
        
        // Get updated task
        auto updated_task = db_->get_task(id);
        if (!updated_task) {
            return make_error_response(500, "Failed to retrieve updated task");
        }
        
        nlohmann::json json = *updated_task;
        return make_json_response(json);
        
    } catch (const nlohmann::json::parse_error& e) {
        return make_error_response(400, "Invalid JSON in request body");
    } catch (const nlohmann::json::type_error& e) {
        return make_error_response(400, "Invalid status value: " + std::string(e.what()));
    } catch (const std::exception& e) {
        std::cerr << "Error in update_task_status: " << e.what() << std::endl;
        return make_error_response(500, "Internal server error");
    }
}

crow::response TaskHandlers::delete_task(const crow::request& req, const std::string& id) {
    try {
        if (!db_->delete_task(id)) {
            return make_error_response(404, "Task with ID '" + id + "' not found");
        }
        
        return crow::response(204);
        
    } catch (const std::exception& e) {
        std::cerr << "Error in delete_task: " << e.what() << std::endl;
        return make_error_response(500, "Internal server error");
    }
}

TaskFilters TaskHandlers::parse_query_params(const crow::request& req) {
    TaskFilters filters;
    
    // Parse status filter
    auto status_param = req.url_params.get("status");
    if (status_param) {
        std::string status_str = status_param;
        if (status_str == "pending") filters.status = TaskStatus::Pending;
        else if (status_str == "in_progress") filters.status = TaskStatus::InProgress;
        else if (status_str == "completed") filters.status = TaskStatus::Completed;
    }
    
    // Parse assigned_to filter
    auto assigned_to_param = req.url_params.get("assigned_to");
    if (assigned_to_param) {
        filters.assigned_to = assigned_to_param;
    }
    
    // Parse tags filter (comma-separated)
    auto tags_param = req.url_params.get("tags");
    if (tags_param) {
        std::string tags_str = tags_param;
        std::vector<std::string> tags;
        std::stringstream ss(tags_str);
        std::string tag;
        while (std::getline(ss, tag, ',')) {
            tag = trim(tag);
            if (!tag.empty()) {
                tags.push_back(tag);
            }
        }
        if (!tags.empty()) {
            filters.tags = tags;
        }
    }
    
    // Parse page_size
    auto page_size_param = req.url_params.get("page_size");
    if (page_size_param) {
        try {
            filters.page_size = std::stoi(page_size_param);
        } catch (...) {
            filters.page_size = 20;
        }
    }
    
    // Parse page_token
    auto page_token_param = req.url_params.get("page_token");
    if (page_token_param) {
        filters.page_token = page_token_param;
    }
    
    // Parse sort_by
    auto sort_by_param = req.url_params.get("sort_by");
    if (sort_by_param) {
        filters.sort_by = sort_by_param;
    }
    
    // Parse sort_order
    auto sort_order_param = req.url_params.get("sort_order");
    if (sort_order_param) {
        filters.sort_order = sort_order_param;
    }
    
    return filters;
}

crow::response TaskHandlers::make_error_response(int status_code, const std::string& message) {
    ErrorResponse error;
    error.message = message;
    
    nlohmann::json json = error;
    return make_json_response(json, status_code);
}

crow::response TaskHandlers::make_json_response(const nlohmann::json& json, int status_code) {
    crow::response res(status_code);
    res.set_header("Content-Type", "application/json");
    res.write(json.dump());
    return res;
}

} // namespace task_api