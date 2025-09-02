#import <Foundation/Foundation.h>
#import "../../server/include/Task.h"

typedef void (^TaskCompletionHandler)(Task *task, NSError *error);
typedef void (^TaskListCompletionHandler)(NSArray<Task *> *tasks, NSInteger totalCount, NSString *nextPageToken, NSError *error);
typedef void (^BooleanCompletionHandler)(BOOL success, NSError *error);

@interface TaskAPIClient : NSObject

@property (nonatomic, strong, readonly) NSURL *baseURL;
@property (nonatomic, strong, readonly) NSURLSession *session;

// Initializer
- (instancetype)initWithBaseURL:(NSURL *)baseURL;

// API Methods
- (void)listTasksWithStatus:(NSString *)status
                 assignedTo:(NSString *)assignedTo
                       tags:(NSArray<NSString *> *)tags
                   pageSize:(NSInteger)pageSize
                  pageToken:(NSString *)pageToken
                     sortBy:(NSString *)sortBy
                  sortOrder:(NSString *)sortOrder
                 completion:(TaskListCompletionHandler)completion;

- (void)getTaskWithId:(NSString *)taskId 
           completion:(TaskCompletionHandler)completion;

- (void)createTaskWithRequest:(CreateTaskRequest *)request 
                   completion:(TaskCompletionHandler)completion;

- (void)updateTask:(NSString *)taskId 
       withRequest:(UpdateTaskRequest *)request 
        completion:(TaskCompletionHandler)completion;

- (void)updateTaskStatus:(NSString *)taskId 
                  status:(TaskStatus)status 
              completion:(TaskCompletionHandler)completion;

- (void)deleteTask:(NSString *)taskId 
        completion:(BooleanCompletionHandler)completion;

// Demo method
- (void)runDemo;

@end