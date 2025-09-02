#ifndef TASK_REPOSITORY_H
#define TASK_REPOSITORY_H

#include "task.h"
#include <pthread.h>

#define MAX_TASKS 1000

typedef struct {
    Task tasks[MAX_TASKS];
    int count;
    pthread_mutex_t mutex;
} TaskRepository;

// Repository functions
void repository_init(TaskRepository* repo);
void repository_destroy(TaskRepository* repo);
void repository_load_sample_data(TaskRepository* repo);

// CRUD operations
bool repository_create_task(TaskRepository* repo, Task* task);
Task* repository_get_task(TaskRepository* repo, const char* id);
bool repository_update_task(TaskRepository* repo, const char* id, const Task* task);
bool repository_delete_task(TaskRepository* repo, const char* id);

// Query operations
int repository_list_tasks(TaskRepository* repo, Task* result, int max_results,
                         const char* status, const char* assigned_to, 
                         const char** tags, int tag_count);

// Utility
int repository_get_count(TaskRepository* repo);

#endif // TASK_REPOSITORY_H