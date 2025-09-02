#import <Foundation/Foundation.h>
#import "Task.h"

@interface TaskRepository : NSObject

// Singleton instance
+ (instancetype)sharedRepository;

// CRUD operations
- (NSArray<Task *> *)getAllTasks;
- (NSArray<Task *> *)getTasksWithStatus:(TaskStatus)status;
- (NSArray<Task *> *)getTasksAssignedTo:(NSString *)assignedTo;
- (NSArray<Task *> *)getTasksWithTags:(NSArray<NSString *> *)tags;
- (Task *)getTaskById:(NSString *)taskId;
- (Task *)createTask:(Task *)task;
- (Task *)updateTask:(NSString *)taskId withRequest:(UpdateTaskRequest *)request;
- (Task *)updateTaskStatus:(NSString *)taskId status:(TaskStatus)status;
- (BOOL)deleteTask:(NSString *)taskId;

// Filtering and pagination
- (NSDictionary *)listTasksWithStatus:(NSString *)status
                           assignedTo:(NSString *)assignedTo
                                 tags:(NSArray<NSString *> *)tags
                             pageSize:(NSInteger)pageSize
                            pageToken:(NSString *)pageToken
                               sortBy:(NSString *)sortBy
                            sortOrder:(NSString *)sortOrder;

// Utility methods
- (NSInteger)taskCount;
- (void)loadSampleData;
- (void)clearAllTasks;

@end