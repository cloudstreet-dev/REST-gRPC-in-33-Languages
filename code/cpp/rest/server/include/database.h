#pragma once

#include "models.h"
#include <sqlite3.h>
#include <memory>
#include <vector>
#include <optional>

namespace task_api {

class TaskDatabase {
public:
    TaskDatabase(const std::string& db_path = "tasks.db");
    ~TaskDatabase();
    
    // Disable copy constructor and assignment
    TaskDatabase(const TaskDatabase&) = delete;
    TaskDatabase& operator=(const TaskDatabase&) = delete;
    
    // Move constructor and assignment
    TaskDatabase(TaskDatabase&&) = default;
    TaskDatabase& operator=(TaskDatabase&&) = default;
    
    bool initialize();
    bool create_sample_data();
    
    // CRUD operations
    std::optional<Task> get_task(const std::string& id);
    std::vector<Task> list_tasks(const TaskFilters& filters);
    int count_tasks(const TaskFilters& filters);
    bool create_task(const Task& task);
    bool update_task(const std::string& id, const UpdateTaskRequest& request);
    bool delete_task(const std::string& id);
    
private:
    sqlite3* db_;
    std::string db_path_;
    
    bool execute_sql(const std::string& sql);
    Task task_from_statement(sqlite3_stmt* stmt);
    std::string build_where_clause(const TaskFilters& filters);
    void bind_filter_params(sqlite3_stmt* stmt, const TaskFilters& filters, int& param_index);
    std::string tags_to_json(const std::vector<std::string>& tags);
    std::vector<std::string> json_to_tags(const std::string& json_str);
};

} // namespace task_api