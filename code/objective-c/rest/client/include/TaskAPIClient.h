#import <Foundation/Foundation.h>

// Simple completion handlers using NSDictionary for easier JSON handling
typedef void (^DictionaryCompletionHandler)(NSDictionary *response, NSError *error);
typedef void (^BooleanCompletionHandler)(BOOL success, NSError *error);

@interface TaskAPIClient : NSObject

@property (nonatomic, strong, readonly) NSString *baseURL;
@property (nonatomic, strong, readonly) NSURLSession *session;

// Initializer
- (instancetype)initWithBaseURL:(NSString *)baseURL;

// API Methods
- (void)listTasksWithStatus:(NSString *)status
                 assignedTo:(NSString *)assignedTo
                       tags:(NSArray<NSString *> *)tags
                   pageSize:(NSInteger)pageSize
                  pageToken:(NSString *)pageToken
                     sortBy:(NSString *)sortBy
                  sortOrder:(NSString *)sortOrder
                 completion:(DictionaryCompletionHandler)completion;

- (void)getTask:(NSString *)taskId 
     completion:(DictionaryCompletionHandler)completion;

- (void)createTask:(NSDictionary *)task 
        completion:(DictionaryCompletionHandler)completion;

- (void)updateTask:(NSString *)taskId 
           updates:(NSDictionary *)updates
        completion:(DictionaryCompletionHandler)completion;

- (void)updateTaskStatus:(NSString *)taskId 
                  status:(NSString *)status 
              completion:(DictionaryCompletionHandler)completion;

- (void)deleteTask:(NSString *)taskId 
        completion:(BooleanCompletionHandler)completion;

@end