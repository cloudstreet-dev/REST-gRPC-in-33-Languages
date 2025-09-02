#import "TaskRepository.h"

@interface TaskRepository ()
@property (nonatomic, strong) NSMutableDictionary<NSString *, Task *> *tasks;
@property (nonatomic, strong) dispatch_queue_t queue;
@end

@implementation TaskRepository

+ (instancetype)sharedRepository {
    static TaskRepository *sharedInstance = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        sharedInstance = [[self alloc] init];
    });
    return sharedInstance;
}

- (instancetype)init {
    self = [super init];
    if (self) {
        _tasks = [NSMutableDictionary dictionary];
        _queue = dispatch_queue_create("com.taskapi.repository", DISPATCH_QUEUE_CONCURRENT);
        [self loadSampleData];
    }
    return self;
}

- (void)loadSampleData {
    // Sample task 1
    Task *task1 = [[Task alloc] initWithTitle:@"Implement Objective-C REST API"];
    task1.taskDescription = @"Build a REST API server using Foundation framework";
    task1.status = TaskStatusInProgress;
    task1.priority = TaskPriorityHigh;
    task1.tags = @[@"objective-c", @"rest", @"api"];
    task1.assignedTo = @"ios-team";
    
    // Sample task 2
    Task *task2 = [[Task alloc] initWithTitle:@"Add gRPC support"];
    task2.taskDescription = @"Implement gRPC server and client for Objective-C";
    task2.status = TaskStatusPending;
    task2.priority = TaskPriorityMedium;
    task2.tags = @[@"objective-c", @"grpc", @"protobuf"];
    task2.assignedTo = @"backend-team";
    
    // Sample task 3
    Task *task3 = [[Task alloc] initWithTitle:@"Write unit tests"];
    task3.taskDescription = @"Add comprehensive test coverage using XCTest";
    task3.status = TaskStatusPending;
    task3.priority = TaskPriorityHigh;
    task3.tags = @[@"testing", @"quality"];
    task3.assignedTo = @"qa-team";
    
    dispatch_barrier_async(self.queue, ^{
        self.tasks[task1.taskId] = task1;
        self.tasks[task2.taskId] = task2;
        self.tasks[task3.taskId] = task3;
    });
}

- (NSArray<Task *> *)getAllTasks {
    __block NSArray<Task *> *allTasks;
    dispatch_sync(self.queue, ^{
        allTasks = [self.tasks allValues];
    });
    return allTasks;
}

- (NSArray<Task *> *)getTasksWithStatus:(TaskStatus)status {
    __block NSMutableArray<Task *> *filteredTasks = [NSMutableArray array];
    dispatch_sync(self.queue, ^{
        for (Task *task in [self.tasks allValues]) {
            if (task.status == status) {
                [filteredTasks addObject:task];
            }
        }
    });
    return [filteredTasks copy];
}

- (NSArray<Task *> *)getTasksAssignedTo:(NSString *)assignedTo {
    __block NSMutableArray<Task *> *filteredTasks = [NSMutableArray array];
    dispatch_sync(self.queue, ^{
        for (Task *task in [self.tasks allValues]) {
            if ([task.assignedTo isEqualToString:assignedTo]) {
                [filteredTasks addObject:task];
            }
        }
    });
    return [filteredTasks copy];
}

- (NSArray<Task *> *)getTasksWithTags:(NSArray<NSString *> *)tags {
    __block NSMutableArray<Task *> *filteredTasks = [NSMutableArray array];
    dispatch_sync(self.queue, ^{
        for (Task *task in [self.tasks allValues]) {
            BOOL hasAllTags = YES;
            for (NSString *tag in tags) {
                if (![task.tags containsObject:tag]) {
                    hasAllTags = NO;
                    break;
                }
            }
            if (hasAllTags) {
                [filteredTasks addObject:task];
            }
        }
    });
    return [filteredTasks copy];
}

- (Task *)getTaskById:(NSString *)taskId {
    __block Task *task;
    dispatch_sync(self.queue, ^{
        task = self.tasks[taskId];
    });
    return task;
}

- (Task *)createTask:(Task *)task {
    __block Task *createdTask;
    dispatch_barrier_async(self.queue, ^{
        self.tasks[task.taskId] = task;
        createdTask = task;
    });
    return createdTask;
}

- (Task *)updateTask:(NSString *)taskId withRequest:(UpdateTaskRequest *)request {
    __block Task *updatedTask;
    dispatch_barrier_sync(self.queue, ^{
        Task *existing = self.tasks[taskId];
        if (existing) {
            Task *updated = [existing copy];
            
            if (request.title) {
                updated.title = request.title;
            }
            if (request.taskDescription) {
                updated.taskDescription = request.taskDescription;
            }
            if (request.status) {
                updated.status = [request.status integerValue];
            }
            if (request.priority) {
                updated.priority = [request.priority integerValue];
            }
            if (request.tags) {
                updated.tags = request.tags;
            }
            if (request.assignedTo) {
                updated.assignedTo = request.assignedTo;
            }
            
            updated.updatedAt = [NSDate date];
            self.tasks[taskId] = updated;
            updatedTask = updated;
        }
    });
    return updatedTask;
}

- (Task *)updateTaskStatus:(NSString *)taskId status:(TaskStatus)status {
    UpdateTaskRequest *request = [[UpdateTaskRequest alloc] init];
    request.status = @(status);
    return [self updateTask:taskId withRequest:request];
}

- (BOOL)deleteTask:(NSString *)taskId {
    __block BOOL deleted = NO;
    dispatch_barrier_sync(self.queue, ^{
        if (self.tasks[taskId]) {
            [self.tasks removeObjectForKey:taskId];
            deleted = YES;
        }
    });
    return deleted;
}

- (NSDictionary *)listTasksWithStatus:(NSString *)status
                           assignedTo:(NSString *)assignedTo
                                 tags:(NSArray<NSString *> *)tags
                             pageSize:(NSInteger)pageSize
                            pageToken:(NSString *)pageToken
                               sortBy:(NSString *)sortBy
                            sortOrder:(NSString *)sortOrder {
    
    __block NSMutableArray<Task *> *filteredTasks = [NSMutableArray array];
    
    dispatch_sync(self.queue, ^{
        for (Task *task in [self.tasks allValues]) {
            BOOL includeTask = YES;
            
            // Filter by status
            if (status && status.length > 0) {
                TaskStatus taskStatus = [Task stringToStatus:status];
                if (task.status != taskStatus) {
                    includeTask = NO;
                }
            }
            
            // Filter by assigned to
            if (includeTask && assignedTo && assignedTo.length > 0) {
                if (![task.assignedTo isEqualToString:assignedTo]) {
                    includeTask = NO;
                }
            }
            
            // Filter by tags
            if (includeTask && tags && tags.count > 0) {
                for (NSString *tag in tags) {
                    if (![task.tags containsObject:tag]) {
                        includeTask = NO;
                        break;
                    }
                }
            }
            
            if (includeTask) {
                [filteredTasks addObject:task];
            }
        }
    });
    
    // Sort tasks
    NSSortDescriptor *sortDescriptor;
    if ([sortBy isEqualToString:@"title"]) {
        sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:@"title" 
                                                       ascending:[sortOrder isEqualToString:@"asc"]];
    } else if ([sortBy isEqualToString:@"updated_at"]) {
        sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:@"updatedAt" 
                                                       ascending:[sortOrder isEqualToString:@"asc"]];
    } else {
        // Default sort by created_at
        sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:@"createdAt" 
                                                       ascending:[sortOrder isEqualToString:@"asc"]];
    }
    
    NSArray<Task *> *sortedTasks = [filteredTasks sortedArrayUsingDescriptors:@[sortDescriptor]];
    
    // Apply pagination
    NSInteger offset = pageToken ? [pageToken integerValue] : 0;
    NSInteger actualPageSize = (pageSize > 0 && pageSize <= 100) ? pageSize : 20;
    
    NSRange range = NSMakeRange(offset, MIN(actualPageSize, sortedTasks.count - offset));
    NSArray<Task *> *paginatedTasks = [sortedTasks subarrayWithRange:range];
    
    // Prepare response
    NSMutableArray *taskDicts = [NSMutableArray array];
    for (Task *task in paginatedTasks) {
        [taskDicts addObject:[task toDictionary]];
    }
    
    NSString *nextPageToken = nil;
    if (offset + actualPageSize < sortedTasks.count) {
        nextPageToken = [@(offset + actualPageSize) stringValue];
    }
    
    return @{
        @"tasks": taskDicts,
        @"total_count": @(sortedTasks.count),
        @"page_size": @(actualPageSize),
        @"next_page_token": nextPageToken ?: [NSNull null]
    };
}

- (NSInteger)taskCount {
    __block NSInteger count;
    dispatch_sync(self.queue, ^{
        count = self.tasks.count;
    });
    return count;
}

- (void)clearAllTasks {
    dispatch_barrier_async(self.queue, ^{
        [self.tasks removeAllObjects];
    });
}

@end