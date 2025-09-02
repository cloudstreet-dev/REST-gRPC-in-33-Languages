#include "task_repository.h"
#include <string.h>
#include <stdlib.h>

void repository_init(TaskRepository* repo) {
    memset(repo, 0, sizeof(TaskRepository));
    pthread_mutex_init(&repo->mutex, NULL);
    repository_load_sample_data(repo);
}

void repository_destroy(TaskRepository* repo) {
    pthread_mutex_destroy(&repo->mutex);
}

void repository_load_sample_data(TaskRepository* repo) {
    pthread_mutex_lock(&repo->mutex);
    
    // Sample task 1
    Task task1;
    task_init(&task1, "Implement C REST API");
    strcpy(task1.description, "Build a REST API server using minimal dependencies");
    task1.status = TASK_STATUS_IN_PROGRESS;
    task1.priority = TASK_PRIORITY_HIGH;
    strcpy(task1.tags[0], "c");
    strcpy(task1.tags[1], "rest");
    strcpy(task1.tags[2], "api");
    task1.tag_count = 3;
    strcpy(task1.assigned_to, "systems-team");
    repo->tasks[repo->count++] = task1;
    
    // Sample task 2
    Task task2;
    task_init(&task2, "Add gRPC support");
    strcpy(task2.description, "Implement gRPC server and client for C");
    task2.status = TASK_STATUS_PENDING;
    task2.priority = TASK_PRIORITY_MEDIUM;
    strcpy(task2.tags[0], "c");
    strcpy(task2.tags[1], "grpc");
    strcpy(task2.tags[2], "protobuf");
    task2.tag_count = 3;
    strcpy(task2.assigned_to, "backend-team");
    repo->tasks[repo->count++] = task2;
    
    // Sample task 3
    Task task3;
    task_init(&task3, "Write unit tests");
    strcpy(task3.description, "Add comprehensive test coverage using CUnit");
    task3.status = TASK_STATUS_PENDING;
    task3.priority = TASK_PRIORITY_HIGH;
    strcpy(task3.tags[0], "testing");
    strcpy(task3.tags[1], "quality");
    task3.tag_count = 2;
    strcpy(task3.assigned_to, "qa-team");
    repo->tasks[repo->count++] = task3;
    
    pthread_mutex_unlock(&repo->mutex);
}

bool repository_create_task(TaskRepository* repo, Task* task) {
    bool success = false;
    
    pthread_mutex_lock(&repo->mutex);
    if (repo->count < MAX_TASKS) {
        task_generate_id(task->id);
        task->created_at = time(NULL);
        task->updated_at = task->created_at;
        repo->tasks[repo->count++] = *task;
        success = true;
    }
    pthread_mutex_unlock(&repo->mutex);
    
    return success;
}

Task* repository_get_task(TaskRepository* repo, const char* id) {
    Task* result = NULL;
    
    pthread_mutex_lock(&repo->mutex);
    for (int i = 0; i < repo->count; i++) {
        if (strcmp(repo->tasks[i].id, id) == 0) {
            result = &repo->tasks[i];
            break;
        }
    }
    pthread_mutex_unlock(&repo->mutex);
    
    return result;
}

bool repository_update_task(TaskRepository* repo, const char* id, const Task* task) {
    bool success = false;
    
    pthread_mutex_lock(&repo->mutex);
    for (int i = 0; i < repo->count; i++) {
        if (strcmp(repo->tasks[i].id, id) == 0) {
            Task* existing = &repo->tasks[i];
            
            // Update fields
            if (strlen(task->title) > 0) {
                strcpy(existing->title, task->title);
            }
            if (strlen(task->description) > 0) {
                strcpy(existing->description, task->description);
            }
            existing->status = task->status;
            existing->priority = task->priority;
            
            if (task->tag_count > 0) {
                existing->tag_count = task->tag_count;
                for (int j = 0; j < task->tag_count; j++) {
                    strcpy(existing->tags[j], task->tags[j]);
                }
            }
            
            if (strlen(task->assigned_to) > 0) {
                strcpy(existing->assigned_to, task->assigned_to);
            }
            
            existing->updated_at = time(NULL);
            success = true;
            break;
        }
    }
    pthread_mutex_unlock(&repo->mutex);
    
    return success;
}

bool repository_delete_task(TaskRepository* repo, const char* id) {
    bool success = false;
    
    pthread_mutex_lock(&repo->mutex);
    for (int i = 0; i < repo->count; i++) {
        if (strcmp(repo->tasks[i].id, id) == 0) {
            // Shift remaining tasks
            for (int j = i; j < repo->count - 1; j++) {
                repo->tasks[j] = repo->tasks[j + 1];
            }
            repo->count--;
            success = true;
            break;
        }
    }
    pthread_mutex_unlock(&repo->mutex);
    
    return success;
}

int repository_list_tasks(TaskRepository* repo, Task* result, int max_results,
                         const char* status, const char* assigned_to, 
                         const char** tags, int tag_count) {
    int count = 0;
    
    pthread_mutex_lock(&repo->mutex);
    for (int i = 0; i < repo->count && count < max_results; i++) {
        Task* task = &repo->tasks[i];
        bool include = true;
        
        // Filter by status
        if (status && strlen(status) > 0) {
            if (task->status != task_string_to_status(status)) {
                include = false;
            }
        }
        
        // Filter by assigned_to
        if (include && assigned_to && strlen(assigned_to) > 0) {
            if (strcmp(task->assigned_to, assigned_to) != 0) {
                include = false;
            }
        }
        
        // Filter by tags
        if (include && tags && tag_count > 0) {
            for (int j = 0; j < tag_count; j++) {
                bool has_tag = false;
                for (int k = 0; k < task->tag_count; k++) {
                    if (strcmp(task->tags[k], tags[j]) == 0) {
                        has_tag = true;
                        break;
                    }
                }
                if (!has_tag) {
                    include = false;
                    break;
                }
            }
        }
        
        if (include) {
            task_copy(&result[count], task);
            count++;
        }
    }
    pthread_mutex_unlock(&repo->mutex);
    
    return count;
}

int repository_get_count(TaskRepository* repo) {
    int count;
    pthread_mutex_lock(&repo->mutex);
    count = repo->count;
    pthread_mutex_unlock(&repo->mutex);
    return count;
}