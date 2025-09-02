#ifndef TASK_H
#define TASK_H

#include <time.h>
#include <stdbool.h>

#define MAX_TITLE_LEN 256
#define MAX_DESC_LEN 1024
#define MAX_ID_LEN 37  // UUID string length + null terminator
#define MAX_ASSIGNED_LEN 128
#define MAX_TAGS 10
#define MAX_TAG_LEN 64

typedef enum {
    TASK_STATUS_PENDING,
    TASK_STATUS_IN_PROGRESS,
    TASK_STATUS_COMPLETED,
    TASK_STATUS_CANCELLED
} TaskStatus;

typedef enum {
    TASK_PRIORITY_LOW,
    TASK_PRIORITY_MEDIUM,
    TASK_PRIORITY_HIGH,
    TASK_PRIORITY_URGENT
} TaskPriority;

typedef struct {
    char id[MAX_ID_LEN];
    char title[MAX_TITLE_LEN];
    char description[MAX_DESC_LEN];
    TaskStatus status;
    TaskPriority priority;
    char tags[MAX_TAGS][MAX_TAG_LEN];
    int tag_count;
    char assigned_to[MAX_ASSIGNED_LEN];
    time_t created_at;
    time_t updated_at;
} Task;

// Task functions
void task_init(Task* task, const char* title);
void task_generate_id(char* id);
const char* task_status_to_string(TaskStatus status);
TaskStatus task_string_to_status(const char* str);
const char* task_priority_to_string(TaskPriority priority);
TaskPriority task_string_to_priority(const char* str);
char* task_to_json(const Task* task);
bool task_from_json(Task* task, const char* json);
void task_copy(Task* dest, const Task* src);

#endif // TASK_H