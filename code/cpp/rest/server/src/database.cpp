#include "database.h"
#include <iostream>
#include <sstream>
#include <algorithm>

namespace task_api {

TaskDatabase::TaskDatabase(const std::string& db_path) : db_(nullptr), db_path_(db_path) {}

TaskDatabase::~TaskDatabase() {
    if (db_) {
        sqlite3_close(db_);
    }
}

bool TaskDatabase::initialize() {
    int rc = sqlite3_open(db_path_.c_str(), &db_);
    if (rc != SQLITE_OK) {
        std::cerr << "Cannot open database: " << sqlite3_errmsg(db_) << std::endl;
        return false;
    }
    
    const char* create_table_sql = R"(
        CREATE TABLE IF NOT EXISTS tasks (
            id TEXT PRIMARY KEY,
            title TEXT NOT NULL,
            description TEXT,
            status TEXT NOT NULL DEFAULT 'pending',
            priority TEXT NOT NULL DEFAULT 'medium',
            tags TEXT DEFAULT '[]',
            assigned_to TEXT,
            created_by TEXT NOT NULL DEFAULT 'system',
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            due_date DATETIME
        );
        
        CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status);
        CREATE INDEX IF NOT EXISTS idx_tasks_assigned_to ON tasks(assigned_to);
        CREATE INDEX IF NOT EXISTS idx_tasks_created_at ON tasks(created_at);
        CREATE INDEX IF NOT EXISTS idx_tasks_priority ON tasks(priority);
    )";
    
    return execute_sql(create_table_sql);
}

bool TaskDatabase::create_sample_data() {
    // Check if data already exists
    const char* count_sql = "SELECT COUNT(*) FROM tasks";
    sqlite3_stmt* stmt;
    
    int rc = sqlite3_prepare_v2(db_, count_sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        std::cerr << "Failed to prepare count statement: " << sqlite3_errmsg(db_) << std::endl;
        return false;
    }
    
    rc = sqlite3_step(stmt);
    int count = 0;
    if (rc == SQLITE_ROW) {
        count = sqlite3_column_int(stmt, 0);
    }
    sqlite3_finalize(stmt);
    
    if (count > 0) {
        return true; // Data already exists
    }
    
    // Create sample tasks
    std::vector<Task> sample_tasks;
    
    Task task1;
    task1.id = generate_uuid();
    task1.title = "Complete C++ project documentation";
    task1.description = "Write comprehensive documentation for the C++ REST API";
    task1.status = TaskStatus::InProgress;
    task1.priority = TaskPriority::High;
    task1.tags = {"documentation", "cpp", "api"};
    task1.assigned_to = "dev-team";
    task1.created_by = "system";
    sample_tasks.push_back(task1);
    
    Task task2;
    task2.id = generate_uuid();
    task2.title = "Review memory management patterns";
    task2.description = "Review and approve memory management patterns in C++ codebase";
    task2.status = TaskStatus::Pending;
    task2.priority = TaskPriority::Medium;
    task2.tags = {"review", "memory", "patterns"};
    task2.assigned_to = "senior-dev";
    task2.created_by = "system";
    sample_tasks.push_back(task2);
    
    Task task3;
    task3.id = generate_uuid();
    task3.title = "Optimize performance critical paths";
    task3.description = "Profile and optimize performance critical code paths";
    task3.status = TaskStatus::Pending;
    task3.priority = TaskPriority::Critical;
    task3.tags = {"performance", "optimization", "profiling"};
    task3.assigned_to = "performance-team";
    task3.created_by = "system";
    sample_tasks.push_back(task3);
    
    // Insert sample tasks
    for (const auto& task : sample_tasks) {
        if (!create_task(task)) {
            return false;
        }
    }
    
    return true;
}

std::optional<Task> TaskDatabase::get_task(const std::string& id) {
    const char* sql = "SELECT * FROM tasks WHERE id = ?";
    sqlite3_stmt* stmt;
    
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return std::nullopt;
    }
    
    sqlite3_bind_text(stmt, 1, id.c_str(), -1, SQLITE_STATIC);
    
    rc = sqlite3_step(stmt);
    if (rc == SQLITE_ROW) {
        Task task = task_from_statement(stmt);
        sqlite3_finalize(stmt);
        return task;
    }
    
    sqlite3_finalize(stmt);
    return std::nullopt;
}

std::vector<Task> TaskDatabase::list_tasks(const TaskFilters& filters) {
    std::vector<Task> tasks;
    
    std::string sql = "SELECT * FROM tasks";
    std::string where_clause = build_where_clause(filters);
    if (!where_clause.empty()) {
        sql += " WHERE " + where_clause;
    }
    
    // Add sorting
    sql += " ORDER BY ";
    if (filters.sort_by == "title") {
        sql += "title";
    } else if (filters.sort_by == "status") {
        sql += "status";
    } else if (filters.sort_by == "priority") {
        sql += "priority";
    } else if (filters.sort_by == "updated_at") {
        sql += "updated_at";
    } else {
        sql += "created_at";
    }
    
    if (filters.sort_order == "asc") {
        sql += " ASC";
    } else {
        sql += " DESC";
    }
    
    // Add pagination
    int offset = 0;
    if (filters.page_token && !filters.page_token->empty()) {
        try {
            offset = std::stoi(*filters.page_token);
        } catch (...) {
            offset = 0;
        }
    }
    
    sql += " LIMIT ? OFFSET ?";
    
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql.c_str(), -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return tasks;
    }
    
    int param_index = 1;
    bind_filter_params(stmt, filters, param_index);
    sqlite3_bind_int(stmt, param_index++, filters.page_size);
    sqlite3_bind_int(stmt, param_index++, offset);
    
    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        tasks.push_back(task_from_statement(stmt));
    }
    
    sqlite3_finalize(stmt);
    return tasks;
}

int TaskDatabase::count_tasks(const TaskFilters& filters) {
    std::string sql = "SELECT COUNT(*) FROM tasks";
    std::string where_clause = build_where_clause(filters);
    if (!where_clause.empty()) {
        sql += " WHERE " + where_clause;
    }
    
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql.c_str(), -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return 0;
    }
    
    int param_index = 1;
    bind_filter_params(stmt, filters, param_index);
    
    rc = sqlite3_step(stmt);
    int count = 0;
    if (rc == SQLITE_ROW) {
        count = sqlite3_column_int(stmt, 0);
    }
    
    sqlite3_finalize(stmt);
    return count;
}

bool TaskDatabase::create_task(const Task& task) {
    const char* sql = R"(
        INSERT INTO tasks (id, title, description, status, priority, tags, 
                          assigned_to, created_by, created_at, updated_at, due_date)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    )";
    
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return false;
    }
    
    sqlite3_bind_text(stmt, 1, task.id.c_str(), -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, task.title.c_str(), -1, SQLITE_STATIC);
    
    if (task.description) {
        sqlite3_bind_text(stmt, 3, task.description->c_str(), -1, SQLITE_STATIC);
    } else {
        sqlite3_bind_null(stmt, 3);
    }
    
    // Convert enums to strings
    std::string status_str;
    switch (task.status) {
        case TaskStatus::Pending: status_str = "pending"; break;
        case TaskStatus::InProgress: status_str = "in_progress"; break;
        case TaskStatus::Completed: status_str = "completed"; break;
    }
    sqlite3_bind_text(stmt, 4, status_str.c_str(), -1, SQLITE_TRANSIENT);
    
    std::string priority_str;
    switch (task.priority) {
        case TaskPriority::Low: priority_str = "low"; break;
        case TaskPriority::Medium: priority_str = "medium"; break;
        case TaskPriority::High: priority_str = "high"; break;
        case TaskPriority::Critical: priority_str = "critical"; break;
    }
    sqlite3_bind_text(stmt, 5, priority_str.c_str(), -1, SQLITE_TRANSIENT);
    
    std::string tags_json = tags_to_json(task.tags);
    sqlite3_bind_text(stmt, 6, tags_json.c_str(), -1, SQLITE_TRANSIENT);
    
    if (task.assigned_to) {
        sqlite3_bind_text(stmt, 7, task.assigned_to->c_str(), -1, SQLITE_STATIC);
    } else {
        sqlite3_bind_null(stmt, 7);
    }
    
    sqlite3_bind_text(stmt, 8, task.created_by.c_str(), -1, SQLITE_STATIC);
    
    std::string created_at_str = to_iso_string(task.created_at);
    std::string updated_at_str = to_iso_string(task.updated_at);
    sqlite3_bind_text(stmt, 9, created_at_str.c_str(), -1, SQLITE_TRANSIENT);
    sqlite3_bind_text(stmt, 10, updated_at_str.c_str(), -1, SQLITE_TRANSIENT);
    
    if (task.due_date) {
        std::string due_date_str = to_iso_string(*task.due_date);
        sqlite3_bind_text(stmt, 11, due_date_str.c_str(), -1, SQLITE_TRANSIENT);
    } else {
        sqlite3_bind_null(stmt, 11);
    }
    
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    
    return rc == SQLITE_DONE;
}

bool TaskDatabase::update_task(const std::string& id, const UpdateTaskRequest& request) {
    // Build dynamic update query
    std::vector<std::string> set_clauses;
    std::vector<std::string> params;
    
    if (request.title) {
        set_clauses.push_back("title = ?");
        params.push_back(*request.title);
    }
    
    if (request.description) {
        set_clauses.push_back("description = ?");
        params.push_back(*request.description);
    }
    
    if (request.status) {
        set_clauses.push_back("status = ?");
        switch (*request.status) {
            case TaskStatus::Pending: params.push_back("pending"); break;
            case TaskStatus::InProgress: params.push_back("in_progress"); break;
            case TaskStatus::Completed: params.push_back("completed"); break;
        }
    }
    
    if (request.priority) {
        set_clauses.push_back("priority = ?");
        switch (*request.priority) {
            case TaskPriority::Low: params.push_back("low"); break;
            case TaskPriority::Medium: params.push_back("medium"); break;
            case TaskPriority::High: params.push_back("high"); break;
            case TaskPriority::Critical: params.push_back("critical"); break;
        }
    }
    
    if (request.tags) {
        set_clauses.push_back("tags = ?");
        params.push_back(tags_to_json(*request.tags));
    }
    
    if (request.assigned_to) {
        set_clauses.push_back("assigned_to = ?");
        params.push_back(*request.assigned_to);
    }
    
    if (request.due_date) {
        set_clauses.push_back("due_date = ?");
        params.push_back(to_iso_string(*request.due_date));
    }
    
    if (set_clauses.empty()) {
        return true; // No updates requested
    }
    
    set_clauses.push_back("updated_at = ?");
    params.push_back(to_iso_string(std::chrono::system_clock::now()));
    
    std::string sql = "UPDATE tasks SET ";
    for (size_t i = 0; i < set_clauses.size(); ++i) {
        if (i > 0) sql += ", ";
        sql += set_clauses[i];
    }
    sql += " WHERE id = ?";
    params.push_back(id);
    
    sqlite3_stmt* stmt;
    int rc = sqlite3_prepare_v2(db_, sql.c_str(), -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return false;
    }
    
    for (size_t i = 0; i < params.size(); ++i) {
        sqlite3_bind_text(stmt, static_cast<int>(i + 1), params[i].c_str(), -1, SQLITE_TRANSIENT);
    }
    
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    
    return rc == SQLITE_DONE;
}

bool TaskDatabase::delete_task(const std::string& id) {
    const char* sql = "DELETE FROM tasks WHERE id = ?";
    sqlite3_stmt* stmt;
    
    int rc = sqlite3_prepare_v2(db_, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        return false;
    }
    
    sqlite3_bind_text(stmt, 1, id.c_str(), -1, SQLITE_STATIC);
    
    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);
    
    return rc == SQLITE_DONE && sqlite3_changes(db_) > 0;
}

// Private helper methods
bool TaskDatabase::execute_sql(const std::string& sql) {
    char* error_msg = nullptr;
    int rc = sqlite3_exec(db_, sql.c_str(), nullptr, nullptr, &error_msg);
    
    if (rc != SQLITE_OK) {
        std::cerr << "SQL error: " << error_msg << std::endl;
        sqlite3_free(error_msg);
        return false;
    }
    
    return true;
}

Task TaskDatabase::task_from_statement(sqlite3_stmt* stmt) {
    Task task;
    
    task.id = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
    task.title = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
    
    const char* desc = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 2));
    if (desc) {
        task.description = std::string(desc);
    }
    
    // Convert status string to enum
    std::string status_str = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 3));
    if (status_str == "pending") task.status = TaskStatus::Pending;
    else if (status_str == "in_progress") task.status = TaskStatus::InProgress;
    else if (status_str == "completed") task.status = TaskStatus::Completed;
    
    // Convert priority string to enum
    std::string priority_str = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 4));
    if (priority_str == "low") task.priority = TaskPriority::Low;
    else if (priority_str == "medium") task.priority = TaskPriority::Medium;
    else if (priority_str == "high") task.priority = TaskPriority::High;
    else if (priority_str == "critical") task.priority = TaskPriority::Critical;
    
    // Parse tags JSON
    const char* tags_json = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 5));
    if (tags_json) {
        task.tags = json_to_tags(tags_json);
    }
    
    const char* assigned_to = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 6));
    if (assigned_to) {
        task.assigned_to = std::string(assigned_to);
    }
    
    task.created_by = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 7));
    
    task.created_at = from_iso_string(reinterpret_cast<const char*>(sqlite3_column_text(stmt, 8)));
    task.updated_at = from_iso_string(reinterpret_cast<const char*>(sqlite3_column_text(stmt, 9)));
    
    const char* due_date = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 10));
    if (due_date) {
        task.due_date = from_iso_string(due_date);
    }
    
    return task;
}

std::string TaskDatabase::build_where_clause(const TaskFilters& filters) {
    std::vector<std::string> conditions;
    
    if (filters.status) {
        conditions.push_back("status = ?");
    }
    
    if (filters.assigned_to && !filters.assigned_to->empty()) {
        conditions.push_back("assigned_to = ?");
    }
    
    if (filters.tags && !filters.tags->empty()) {
        for (size_t i = 0; i < filters.tags->size(); ++i) {
            conditions.push_back("tags LIKE ?");
        }
    }
    
    if (conditions.empty()) {
        return "";
    }
    
    std::string result = conditions[0];
    for (size_t i = 1; i < conditions.size(); ++i) {
        result += " AND " + conditions[i];
    }
    
    return result;
}

void TaskDatabase::bind_filter_params(sqlite3_stmt* stmt, const TaskFilters& filters, int& param_index) {
    if (filters.status) {
        std::string status_str;
        switch (*filters.status) {
            case TaskStatus::Pending: status_str = "pending"; break;
            case TaskStatus::InProgress: status_str = "in_progress"; break;
            case TaskStatus::Completed: status_str = "completed"; break;
        }
        sqlite3_bind_text(stmt, param_index++, status_str.c_str(), -1, SQLITE_TRANSIENT);
    }
    
    if (filters.assigned_to && !filters.assigned_to->empty()) {
        sqlite3_bind_text(stmt, param_index++, filters.assigned_to->c_str(), -1, SQLITE_STATIC);
    }
    
    if (filters.tags && !filters.tags->empty()) {
        for (const auto& tag : *filters.tags) {
            std::string pattern = "%" + tag + "%";
            sqlite3_bind_text(stmt, param_index++, pattern.c_str(), -1, SQLITE_TRANSIENT);
        }
    }
}

std::string TaskDatabase::tags_to_json(const std::vector<std::string>& tags) {
    nlohmann::json j = tags;
    return j.dump();
}

std::vector<std::string> TaskDatabase::json_to_tags(const std::string& json_str) {
    try {
        nlohmann::json j = nlohmann::json::parse(json_str);
        return j.get<std::vector<std::string>>();
    } catch (...) {
        return {};
    }
}

} // namespace task_api