#include "task.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uuid/uuid.h>
#include <ctype.h>

void task_init(Task* task, const char* title) {
    memset(task, 0, sizeof(Task));
    task_generate_id(task->id);
    strncpy(task->title, title, MAX_TITLE_LEN - 1);
    task->status = TASK_STATUS_PENDING;
    task->priority = TASK_PRIORITY_MEDIUM;
    task->created_at = time(NULL);
    task->updated_at = task->created_at;
    task->tag_count = 0;
}

void task_generate_id(char* id) {
    uuid_t uuid;
    uuid_generate(uuid);
    uuid_unparse_lower(uuid, id);
}

const char* task_status_to_string(TaskStatus status) {
    switch (status) {
        case TASK_STATUS_PENDING: return "pending";
        case TASK_STATUS_IN_PROGRESS: return "in_progress";
        case TASK_STATUS_COMPLETED: return "completed";
        case TASK_STATUS_CANCELLED: return "cancelled";
        default: return "pending";
    }
}

TaskStatus task_string_to_status(const char* str) {
    if (!str) return TASK_STATUS_PENDING;
    
    char lower[32];
    int i;
    for (i = 0; str[i] && i < 31; i++) {
        lower[i] = tolower(str[i]);
    }
    lower[i] = '\0';
    
    if (strcmp(lower, "in_progress") == 0) return TASK_STATUS_IN_PROGRESS;
    if (strcmp(lower, "completed") == 0) return TASK_STATUS_COMPLETED;
    if (strcmp(lower, "cancelled") == 0) return TASK_STATUS_CANCELLED;
    return TASK_STATUS_PENDING;
}

const char* task_priority_to_string(TaskPriority priority) {
    switch (priority) {
        case TASK_PRIORITY_LOW: return "low";
        case TASK_PRIORITY_MEDIUM: return "medium";
        case TASK_PRIORITY_HIGH: return "high";
        case TASK_PRIORITY_URGENT: return "urgent";
        default: return "medium";
    }
}

TaskPriority task_string_to_priority(const char* str) {
    if (!str) return TASK_PRIORITY_MEDIUM;
    
    char lower[32];
    int i;
    for (i = 0; str[i] && i < 31; i++) {
        lower[i] = tolower(str[i]);
    }
    lower[i] = '\0';
    
    if (strcmp(lower, "low") == 0) return TASK_PRIORITY_LOW;
    if (strcmp(lower, "high") == 0) return TASK_PRIORITY_HIGH;
    if (strcmp(lower, "urgent") == 0) return TASK_PRIORITY_URGENT;
    return TASK_PRIORITY_MEDIUM;
}

char* task_to_json(const Task* task) {
    char* json = malloc(4096);
    if (!json) return NULL;
    
    char created_at_str[32], updated_at_str[32];
    strftime(created_at_str, sizeof(created_at_str), "%Y-%m-%dT%H:%M:%SZ", gmtime(&task->created_at));
    strftime(updated_at_str, sizeof(updated_at_str), "%Y-%m-%dT%H:%M:%SZ", gmtime(&task->updated_at));
    
    // Build tags array
    char tags_json[1024] = "[";
    for (int i = 0; i < task->tag_count; i++) {
        if (i > 0) strcat(tags_json, ",");
        strcat(tags_json, "\"");
        strcat(tags_json, task->tags[i]);
        strcat(tags_json, "\"");
    }
    strcat(tags_json, "]");
    
    snprintf(json, 4096,
        "{"
        "\"id\":\"%s\","
        "\"title\":\"%s\","
        "\"description\":\"%s\","
        "\"status\":\"%s\","
        "\"priority\":\"%s\","
        "\"tags\":%s,"
        "\"assigned_to\":\"%s\","
        "\"created_at\":\"%s\","
        "\"updated_at\":\"%s\""
        "}",
        task->id,
        task->title,
        task->description,
        task_status_to_string(task->status),
        task_priority_to_string(task->priority),
        tags_json,
        task->assigned_to,
        created_at_str,
        updated_at_str
    );
    
    return json;
}

// Simple JSON parser for task (basic implementation)
bool task_from_json(Task* task, const char* json) {
    if (!task || !json) return false;
    
    memset(task, 0, sizeof(Task));
    
    // Extract title (required)
    const char* title_start = strstr(json, "\"title\":\"");
    if (!title_start) return false;
    title_start += 9;
    const char* title_end = strchr(title_start, '"');
    if (!title_end) return false;
    int title_len = title_end - title_start;
    if (title_len >= MAX_TITLE_LEN) title_len = MAX_TITLE_LEN - 1;
    strncpy(task->title, title_start, title_len);
    
    // Extract description (optional)
    const char* desc_start = strstr(json, "\"description\":\"");
    if (desc_start) {
        desc_start += 15;
        const char* desc_end = strchr(desc_start, '"');
        if (desc_end) {
            int desc_len = desc_end - desc_start;
            if (desc_len >= MAX_DESC_LEN) desc_len = MAX_DESC_LEN - 1;
            strncpy(task->description, desc_start, desc_len);
        }
    }
    
    // Extract status (optional)
    const char* status_start = strstr(json, "\"status\":\"");
    if (status_start) {
        status_start += 10;
        const char* status_end = strchr(status_start, '"');
        if (status_end) {
            char status_str[32];
            int status_len = status_end - status_start;
            if (status_len >= 32) status_len = 31;
            strncpy(status_str, status_start, status_len);
            status_str[status_len] = '\0';
            task->status = task_string_to_status(status_str);
        }
    } else {
        task->status = TASK_STATUS_PENDING;
    }
    
    // Extract priority (optional)
    const char* priority_start = strstr(json, "\"priority\":\"");
    if (priority_start) {
        priority_start += 12;
        const char* priority_end = strchr(priority_start, '"');
        if (priority_end) {
            char priority_str[32];
            int priority_len = priority_end - priority_start;
            if (priority_len >= 32) priority_len = 31;
            strncpy(priority_str, priority_start, priority_len);
            priority_str[priority_len] = '\0';
            task->priority = task_string_to_priority(priority_str);
        }
    } else {
        task->priority = TASK_PRIORITY_MEDIUM;
    }
    
    // Extract assigned_to (optional)
    const char* assigned_start = strstr(json, "\"assigned_to\":\"");
    if (assigned_start) {
        assigned_start += 15;
        const char* assigned_end = strchr(assigned_start, '"');
        if (assigned_end) {
            int assigned_len = assigned_end - assigned_start;
            if (assigned_len >= MAX_ASSIGNED_LEN) assigned_len = MAX_ASSIGNED_LEN - 1;
            strncpy(task->assigned_to, assigned_start, assigned_len);
        }
    }
    
    // Extract tags (optional) - simplified parsing
    const char* tags_start = strstr(json, "\"tags\":[");
    if (tags_start) {
        tags_start += 8;
        const char* tags_end = strchr(tags_start, ']');
        if (tags_end) {
            task->tag_count = 0;
            const char* tag_ptr = tags_start;
            while (tag_ptr < tags_end && task->tag_count < MAX_TAGS) {
                tag_ptr = strchr(tag_ptr, '"');
                if (!tag_ptr || tag_ptr >= tags_end) break;
                tag_ptr++;
                const char* tag_end = strchr(tag_ptr, '"');
                if (!tag_end || tag_end >= tags_end) break;
                
                int tag_len = tag_end - tag_ptr;
                if (tag_len >= MAX_TAG_LEN) tag_len = MAX_TAG_LEN - 1;
                strncpy(task->tags[task->tag_count], tag_ptr, tag_len);
                task->tags[task->tag_count][tag_len] = '\0';
                task->tag_count++;
                
                tag_ptr = tag_end + 1;
            }
        }
    }
    
    // Generate ID if not present
    const char* id_start = strstr(json, "\"id\":\"");
    if (id_start) {
        id_start += 6;
        const char* id_end = strchr(id_start, '"');
        if (id_end) {
            int id_len = id_end - id_start;
            if (id_len >= MAX_ID_LEN) id_len = MAX_ID_LEN - 1;
            strncpy(task->id, id_start, id_len);
        }
    } else {
        task_generate_id(task->id);
    }
    
    task->created_at = time(NULL);
    task->updated_at = task->created_at;
    
    return true;
}

void task_copy(Task* dest, const Task* src) {
    if (dest && src) {
        memcpy(dest, src, sizeof(Task));
    }
}