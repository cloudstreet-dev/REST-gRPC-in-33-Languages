#import <Foundation/Foundation.h>

// Task Status Enumeration
typedef NS_ENUM(NSInteger, TaskStatus) {
    TaskStatusPending,
    TaskStatusInProgress,
    TaskStatusCompleted,
    TaskStatusCancelled
};

// Task Priority Enumeration
typedef NS_ENUM(NSInteger, TaskPriority) {
    TaskPriorityLow,
    TaskPriorityMedium,
    TaskPriorityHigh,
    TaskPriorityUrgent
};

// Task Model
@interface Task : NSObject <NSCopying>

@property (nonatomic, strong) NSString *taskId;
@property (nonatomic, strong) NSString *title;
@property (nonatomic, strong, nullable) NSString *taskDescription;
@property (nonatomic, assign) TaskStatus status;
@property (nonatomic, assign) TaskPriority priority;
@property (nonatomic, strong) NSArray<NSString *> *tags;
@property (nonatomic, strong, nullable) NSString *assignedTo;
@property (nonatomic, strong) NSDate *createdAt;
@property (nonatomic, strong) NSDate *updatedAt;

// Initializers
- (instancetype)initWithTitle:(NSString *)title;
- (instancetype)initWithDictionary:(NSDictionary *)dictionary;

// Serialization
- (NSDictionary *)toDictionary;
- (NSString *)toJSONString;

// Class methods
+ (NSString *)statusToString:(TaskStatus)status;
+ (TaskStatus)stringToStatus:(NSString *)statusString;
+ (NSString *)priorityToString:(TaskPriority)priority;
+ (TaskPriority)stringToPriority:(NSString *)priorityString;
+ (Task *)taskFromJSONString:(NSString *)jsonString;

@end

// Request Models
@interface CreateTaskRequest : NSObject

@property (nonatomic, strong) NSString *title;
@property (nonatomic, strong, nullable) NSString *taskDescription;
@property (nonatomic, assign) TaskPriority priority;
@property (nonatomic, strong, nullable) NSArray<NSString *> *tags;
@property (nonatomic, strong, nullable) NSString *assignedTo;

- (instancetype)initWithDictionary:(NSDictionary *)dictionary;
- (Task *)toTask;

@end

@interface UpdateTaskRequest : NSObject

@property (nonatomic, strong, nullable) NSString *title;
@property (nonatomic, strong, nullable) NSString *taskDescription;
@property (nonatomic, strong, nullable) NSNumber *status;
@property (nonatomic, strong, nullable) NSNumber *priority;
@property (nonatomic, strong, nullable) NSArray<NSString *> *tags;
@property (nonatomic, strong, nullable) NSString *assignedTo;

- (instancetype)initWithDictionary:(NSDictionary *)dictionary;

@end