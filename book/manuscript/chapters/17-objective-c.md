# Chapter 17: Objective-C - The Foundation of Apple Platforms

Objective-C, created in the early 1980s by Brad Cox and Tom Love, has been the primary language for Apple platform development for decades. As a superset of C with object-oriented capabilities inspired by Smalltalk, Objective-C combines the performance of C with dynamic runtime features and message passing. Despite Swift's emergence as Apple's modern language, Objective-C remains crucial for maintaining legacy codebases and understanding the foundations of iOS and macOS development.

This chapter explores how to build REST APIs and gRPC services using Objective-C, demonstrating the language's unique features including its dynamic runtime, message passing syntax, and powerful Foundation framework.

## Table of Contents

- [Objective-C Language Overview](#objective-c-language-overview)
- [Setting Up the Development Environment](#setting-up-the-development-environment)
- [REST API Implementation](#rest-api-implementation)
- [gRPC Implementation](#grpc-implementation)
- [Building and Running the Services](#building-and-running-the-services)
- [Testing the Implementations](#testing-the-implementations)
- [Objective-C Specific Features](#objective-c-specific-features)
- [Memory Management and ARC](#memory-management-and-arc)
- [Deployment Strategies](#deployment-strategies)
- [Conclusion](#conclusion)

## Objective-C Language Overview

Objective-C extends the C programming language with object-oriented features and a unique runtime system that enables dynamic message dispatch and introspection.

### Key Language Features

**Dynamic Runtime**
- Message passing instead of method calls
- Runtime method resolution and forwarding
- Dynamic typing with `id` type
- Introspection and reflection capabilities

**Memory Management**
- Automatic Reference Counting (ARC)
- Manual retain/release (legacy)
- Strong, weak, and unsafe references
- Autorelease pools for memory optimization

**Foundation Framework**
- Rich collection classes (NSArray, NSDictionary, NSSet)
- String handling with NSString
- Date and time with NSDate
- URL loading system for networking
- Grand Central Dispatch for concurrency

**Language Syntax**
- Square bracket message syntax: `[object method]`
- Properties with `@property` and `@synthesize`
- Categories for extending existing classes
- Protocols for defining interfaces
- Blocks for closures and callbacks

### Objective-C in Modern Development

While Swift has become Apple's primary language, Objective-C remains relevant for:
- **Legacy Codebases**: Millions of lines of Objective-C code still in production
- **C Integration**: Direct C compatibility for system programming
- **Runtime Features**: Dynamic capabilities not available in Swift
- **Cross-Platform**: Can be used outside Apple platforms with GNUstep
- **Performance**: Predictable performance characteristics

## Setting Up the Development Environment

### Prerequisites

**macOS Development**
- Xcode 14.0 or later (includes Objective-C compiler)
- Command Line Tools for Xcode
- macOS 12.0 (Monterey) or later

**Cross-Platform Development**
- GCC or Clang with Objective-C support
- GNUstep for non-Apple platforms
- Foundation framework alternative

### Installation

**macOS Setup**
```bash
# Install Xcode from App Store or:
xcode-select --install

# Verify installation
clang --version
# Apple clang version 14.0.0

# Install package manager (optional)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

**Linux Setup (Ubuntu/Debian)**
```bash
# Install GNUstep and Objective-C compiler
sudo apt-get update
sudo apt-get install -y build-essential
sudo apt-get install -y gobjc gnustep gnustep-devel
sudo apt-get install -y libgnustep-base-dev

# Set up GNUstep environment
source /usr/share/GNUstep/Makefiles/GNUstep.sh
```

### Project Structure

```
objective-c/
├── rest/
│   ├── server/
│   │   ├── include/
│   │   │   ├── Task.h
│   │   │   ├── TaskRepository.h
│   │   │   └── TaskServer.h
│   │   ├── src/
│   │   │   ├── main.m
│   │   │   ├── Task.m
│   │   │   ├── TaskRepository.m
│   │   │   └── TaskServer.m
│   │   └── Makefile
│   └── client/
│       ├── include/
│       │   └── TaskAPIClient.h
│       ├── src/
│       │   ├── main.m
│       │   └── TaskAPIClient.m
│       └── Makefile
├── grpc/
│   ├── Podfile
│   └── README.md
└── README.md
```

## REST API Implementation

### Data Models with Objective-C

Objective-C uses header files (.h) for interfaces and implementation files (.m):

```objc
// Task.h
#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, TaskStatus) {
    TaskStatusPending,
    TaskStatusInProgress,
    TaskStatusCompleted,
    TaskStatusCancelled
};

typedef NS_ENUM(NSInteger, TaskPriority) {
    TaskPriorityLow,
    TaskPriorityMedium,
    TaskPriorityHigh,
    TaskPriorityUrgent
};

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

- (instancetype)initWithTitle:(NSString *)title;
- (instancetype)initWithDictionary:(NSDictionary *)dictionary;
- (NSDictionary *)toDictionary;
- (NSString *)toJSONString;

+ (NSString *)statusToString:(TaskStatus)status;
+ (TaskStatus)stringToStatus:(NSString *)statusString;

@end
```

Key Objective-C features shown:
- **NS_ENUM**: Type-safe enumerations
- **Properties**: Automatic getter/setter generation
- **Nullability**: Explicit null annotations for Swift interop
- **Protocols**: NSCopying for object copying
- **Class methods**: Prefixed with `+`
- **Instance methods**: Prefixed with `-`

### Repository Pattern with Thread Safety

```objc
// TaskRepository.m
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
        _queue = dispatch_queue_create("com.taskapi.repository", 
                                      DISPATCH_QUEUE_CONCURRENT);
    }
    return self;
}

- (NSArray<Task *> *)getAllTasks {
    __block NSArray<Task *> *allTasks;
    dispatch_sync(self.queue, ^{
        allTasks = [self.tasks allValues];
    });
    return allTasks;
}

- (Task *)createTask:(Task *)task {
    dispatch_barrier_async(self.queue, ^{
        self.tasks[task.taskId] = task;
    });
    return task;
}

- (Task *)updateTask:(NSString *)taskId 
         withRequest:(UpdateTaskRequest *)request {
    __block Task *updatedTask;
    dispatch_barrier_sync(self.queue, ^{
        Task *existing = self.tasks[taskId];
        if (existing) {
            Task *updated = [existing copy];
            // Update properties...
            updated.updatedAt = [NSDate date];
            self.tasks[taskId] = updated;
            updatedTask = updated;
        }
    });
    return updatedTask;
}
@end
```

Demonstrates:
- **Singleton pattern**: Using dispatch_once for thread-safe initialization
- **Grand Central Dispatch**: Concurrent queue with barriers for thread safety
- **Blocks**: Objective-C closures for async operations
- **Property synthesis**: Automatic backing variable generation

### HTTP Server with GCDWebServer

GCDWebServer provides a lightweight HTTP server for macOS and iOS:

```objc
// TaskServer.m
@implementation TaskServer

- (void)setupRoutes {
    __weak typeof(self) weakSelf = self;
    
    // GET /api/tasks - List all tasks
    [self.webServer addHandlerForMethod:@"GET"
                                   path:@"/api/tasks"
                           requestClass:[GCDWebServerRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        return [weakSelf handleListTasks:request];
    }];
    
    // GET /api/tasks/:id - Get task by ID
    [self.webServer addHandlerForMethod:@"GET"
                            pathRegex:@"^/api/tasks/([^/]+)$"
                           requestClass:[GCDWebServerRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        NSArray *captures = [request attributeForKey:GCDWebServerRequestAttribute_RegexCaptures];
        NSString *taskId = captures[1];
        return [weakSelf handleGetTask:taskId];
    }];
    
    // POST /api/tasks - Create new task
    [self.webServer addHandlerForMethod:@"POST"
                                   path:@"/api/tasks"
                           requestClass:[GCDWebServerDataRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        return [weakSelf handleCreateTask:(GCDWebServerDataRequest *)request];
    }];
}

- (GCDWebServerResponse *)handleListTasks:(GCDWebServerRequest *)request {
    // Parse query parameters
    NSString *status = request.query[@"status"];
    NSInteger pageSize = request.query[@"page_size"] ? 
                        [request.query[@"page_size"] integerValue] : 20;
    
    NSDictionary *result = [self.repository listTasksWithStatus:status
                                                      pageSize:pageSize];
    
    return [self jsonResponse:result statusCode:200];
}

- (GCDWebServerDataResponse *)jsonResponse:(NSDictionary *)data 
                                statusCode:(NSInteger)statusCode {
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:data
                                                       options:NSJSONWritingPrettyPrinted
                                                         error:&error];
    
    GCDWebServerDataResponse *response = 
        [GCDWebServerDataResponse responseWithData:jsonData
                                       contentType:@"application/json"];
    response.statusCode = statusCode;
    
    // Add CORS headers
    [response setValue:@"*" forAdditionalHeader:@"Access-Control-Allow-Origin"];
    
    return response;
}
@end
```

### REST Client Implementation

```objc
// TaskAPIClient.m
@implementation TaskAPIClient

- (void)listTasksWithStatus:(NSString *)status
                 completion:(TaskListCompletionHandler)completion {
    
    NSURLComponents *components = [NSURLComponents componentsWithURL:self.baseURL 
                                             resolvingAgainstBaseURL:YES];
    components.path = @"/api/tasks";
    
    NSMutableArray<NSURLQueryItem *> *queryItems = [NSMutableArray array];
    if (status) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"status" 
                                                           value:status]];
    }
    components.queryItems = queryItems;
    
    NSURLRequest *request = [NSURLRequest requestWithURL:components.URL];
    
    [[self.session dataTaskWithRequest:request 
                      completionHandler:^(NSData *data, 
                                        NSURLResponse *response, 
                                        NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, 0, nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 200) {
            NSError *jsonError;
            NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data 
                                                                        options:0 
                                                                          error:&jsonError];
            
            NSMutableArray<Task *> *tasks = [NSMutableArray array];
            for (NSDictionary *taskDict in responseDict[@"tasks"]) {
                Task *task = [[Task alloc] initWithDictionary:taskDict];
                [tasks addObject:task];
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(tasks, [responseDict[@"total_count"] integerValue], 
                          responseDict[@"next_page_token"], nil);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: 
                                                   [NSString stringWithFormat:@"HTTP Error %ld", 
                                                            (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, 0, nil, httpError);
            });
        }
    }] resume];
}

- (void)createTaskWithRequest:(CreateTaskRequest *)request 
                   completion:(TaskCompletionHandler)completion {
    NSURL *url = [self.baseURL URLByAppendingPathComponent:@"/api/tasks"];
    
    NSMutableDictionary *body = [NSMutableDictionary dictionary];
    body[@"title"] = request.title;
    if (request.taskDescription) {
        body[@"description"] = request.taskDescription;
    }
    body[@"priority"] = [Task priorityToString:request.priority];
    
    NSError *jsonError;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:body 
                                                       options:0 
                                                         error:&jsonError];
    
    NSMutableURLRequest *urlRequest = [NSMutableURLRequest requestWithURL:url];
    urlRequest.HTTPMethod = @"POST";
    urlRequest.HTTPBody = jsonData;
    [urlRequest setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    
    [[self.session dataTaskWithRequest:urlRequest 
                      completionHandler:^(NSData *data, 
                                        NSURLResponse *response, 
                                        NSError *error) {
        // Handle response...
    }] resume];
}
@end
```

## gRPC Implementation

### Protocol Buffer Integration

Objective-C gRPC support requires Protocol Buffer compiler with Objective-C plugin:

```bash
# Generate Objective-C files from proto
protoc --objc_out=. --grpc_out=. \
  --plugin=protoc-gen-grpc=`which grpc_objective_c_plugin` \
  tasks.proto
```

### gRPC Service Implementation

```objc
// TaskServiceImpl.h
#import "Tasks.pbrpc.h"

@interface TaskServiceImpl : NSObject <TATaskService>
@end

// TaskServiceImpl.m
@implementation TaskServiceImpl

- (void)listTasksWithRequest:(TAListTasksRequest *)request 
                      handler:(void(^)(TAListTasksResponse *response, 
                                      NSError *error))handler {
    TAListTasksResponse *response = [[TAListTasksResponse alloc] init];
    
    // Apply filters
    NSPredicate *predicate = nil;
    if (request.hasStatus) {
        predicate = [NSPredicate predicateWithFormat:@"status == %d", 
                                                     request.status];
    }
    
    NSArray *filteredTasks = [[TaskRepository sharedRepository] getAllTasks];
    if (predicate) {
        filteredTasks = [filteredTasks filteredArrayUsingPredicate:predicate];
    }
    
    // Convert to protobuf messages
    for (Task *task in filteredTasks) {
        TATask *pbTask = [self taskToProtobuf:task];
        [response.tasksArray addObject:pbTask];
    }
    
    response.totalCount = filteredTasks.count;
    handler(response, nil);
}

- (void)createTaskWithRequest:(TACreateTaskRequest *)request 
                       handler:(void(^)(TATask *response, 
                                       NSError *error))handler {
    if (request.title.length == 0) {
        NSError *error = [NSError errorWithDomain:@"TaskService" 
                                             code:400 
                                         userInfo:@{NSLocalizedDescriptionKey: 
                                                   @"Title is required"}];
        handler(nil, error);
        return;
    }
    
    Task *task = [[Task alloc] initWithTitle:request.title];
    task.taskDescription = request.description_p;
    task.priority = request.priority;
    task.tags = [request.tagsArray copy];
    
    Task *created = [[TaskRepository sharedRepository] createTask:task];
    TATask *response = [self taskToProtobuf:created];
    
    handler(response, nil);
}

- (TATask *)taskToProtobuf:(Task *)task {
    TATask *pbTask = [[TATask alloc] init];
    pbTask.id_p = task.taskId;
    pbTask.title = task.title;
    pbTask.description_p = task.taskDescription ?: @"";
    pbTask.status = (TATaskStatus)task.status;
    pbTask.priority = (TATaskPriority)task.priority;
    [pbTask.tagsArray addObjectsFromArray:task.tags];
    pbTask.assignedTo = task.assignedTo ?: @"";
    
    return pbTask;
}
@end
```

### gRPC Client

```objc
// TaskGRPCClient.m
@interface TaskGRPCClient ()
@property (nonatomic, strong) TATaskService *service;
@property (nonatomic, strong) GRPCProtoCall *currentCall;
@end

@implementation TaskGRPCClient

- (instancetype)initWithHost:(NSString *)host port:(NSInteger)port {
    self = [super init];
    if (self) {
        NSString *address = [NSString stringWithFormat:@"%@:%ld", host, (long)port];
        _service = [[TATaskService alloc] initWithHost:address];
    }
    return self;
}

- (void)listTasksWithCompletion:(void(^)(NSArray<Task *> *tasks, 
                                        NSError *error))completion {
    TAListTasksRequest *request = [[TAListTasksRequest alloc] init];
    
    self.currentCall = [self.service RPCToListTasksWithRequest:request 
                                                       handler:^(TAListTasksResponse *response, 
                                                               NSError *error) {
        if (response) {
            NSMutableArray<Task *> *tasks = [NSMutableArray array];
            for (TATask *pbTask in response.tasksArray) {
                Task *task = [self protobufToTask:pbTask];
                [tasks addObject:task];
            }
            completion(tasks, nil);
        } else {
            completion(nil, error);
        }
    }];
    
    [self.currentCall start];
}

- (void)createTask:(CreateTaskRequest *)request 
        completion:(void(^)(Task *task, NSError *error))completion {
    TACreateTaskRequest *pbRequest = [[TACreateTaskRequest alloc] init];
    pbRequest.title = request.title;
    pbRequest.description_p = request.taskDescription ?: @"";
    pbRequest.priority = (TATaskPriority)request.priority;
    [pbRequest.tagsArray addObjectsFromArray:request.tags ?: @[]];
    
    self.currentCall = [self.service RPCToCreateTaskWithRequest:pbRequest 
                                                        handler:^(TATask *response, 
                                                                NSError *error) {
        if (response) {
            Task *task = [self protobufToTask:response];
            completion(task, nil);
        } else {
            completion(nil, error);
        }
    }];
    
    [self.currentCall start];
}

- (Task *)protobufToTask:(TATask *)pbTask {
    Task *task = [[Task alloc] init];
    task.taskId = pbTask.id_p;
    task.title = pbTask.title;
    task.taskDescription = pbTask.description_p.length > 0 ? pbTask.description_p : nil;
    task.status = (TaskStatus)pbTask.status;
    task.priority = (TaskPriority)pbTask.priority;
    task.tags = [pbTask.tagsArray copy];
    task.assignedTo = pbTask.assignedTo.length > 0 ? pbTask.assignedTo : nil;
    
    return task;
}
@end
```

## Building and Running the Services

### Makefile for REST Server

```makefile
CC = clang
OBJC = clang
CFLAGS = -Wall -Wextra -O2 -fobjc-arc
FRAMEWORKS = -framework Foundation -framework GCDWebServer
INCLUDES = -I./include -I/usr/local/include
LDFLAGS = -L/usr/local/lib

SOURCES = src/main.m src/Task.m src/TaskRepository.m src/TaskServer.m
OBJECTS = $(SOURCES:.m=.o)
EXECUTABLE = task-rest-server

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(OBJC) $(CFLAGS) $(FRAMEWORKS) $(LDFLAGS) -o $@ $^

%.o: %.m
	$(OBJC) $(CFLAGS) $(INCLUDES) -c $< -o $@

run: $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)
```

### Running the Services

**REST Server**
```bash
cd code/objective-c/rest/server
make
./task-rest-server
# Server starts on http://localhost:8080
```

**REST Client**
```bash
cd code/objective-c/rest/client
make
./task-rest-client demo
```

**gRPC with CocoaPods**
```bash
cd code/objective-c/grpc
pod install
xcodebuild -workspace TaskGRPC.xcworkspace -scheme TaskGRPCServer build
./build/Release/TaskGRPCServer
```

## Testing the Implementations

### Unit Testing with XCTest

```objc
// TaskTests.m
#import <XCTest/XCTest.h>
#import "Task.h"
#import "TaskRepository.h"

@interface TaskTests : XCTestCase
@property (nonatomic, strong) TaskRepository *repository;
@end

@implementation TaskTests

- (void)setUp {
    [super setUp];
    self.repository = [[TaskRepository alloc] init];
    [self.repository clearAllTasks];
}

- (void)testTaskCreation {
    Task *task = [[Task alloc] initWithTitle:@"Test Task"];
    
    XCTAssertNotNil(task.taskId);
    XCTAssertEqualObjects(task.title, @"Test Task");
    XCTAssertEqual(task.status, TaskStatusPending);
    XCTAssertEqual(task.priority, TaskPriorityMedium);
}

- (void)testRepositoryCRUD {
    // Create
    Task *task = [[Task alloc] initWithTitle:@"Test Task"];
    Task *created = [self.repository createTask:task];
    XCTAssertNotNil(created);
    
    // Read
    Task *retrieved = [self.repository getTaskById:created.taskId];
    XCTAssertEqualObjects(retrieved.title, @"Test Task");
    
    // Update
    UpdateTaskRequest *request = [[UpdateTaskRequest alloc] init];
    request.status = @(TaskStatusInProgress);
    Task *updated = [self.repository updateTask:created.taskId 
                                    withRequest:request];
    XCTAssertEqual(updated.status, TaskStatusInProgress);
    
    // Delete
    BOOL deleted = [self.repository deleteTask:created.taskId];
    XCTAssertTrue(deleted);
    
    Task *notFound = [self.repository getTaskById:created.taskId];
    XCTAssertNil(notFound);
}

- (void)testConcurrentAccess {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Concurrent operations"];
    
    dispatch_group_t group = dispatch_group_create();
    dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    
    // Create 100 tasks concurrently
    for (int i = 0; i < 100; i++) {
        dispatch_group_enter(group);
        dispatch_async(queue, ^{
            Task *task = [[Task alloc] initWithTitle:[NSString stringWithFormat:@"Task %d", i]];
            [self.repository createTask:task];
            dispatch_group_leave(group);
        });
    }
    
    dispatch_group_notify(group, dispatch_get_main_queue(), ^{
        NSInteger count = [self.repository taskCount];
        XCTAssertEqual(count, 100);
        [expectation fulfill];
    });
    
    [self waitForExpectationsWithTimeout:5.0 handler:nil];
}
@end
```

### Integration Testing

```objc
// IntegrationTests.m
@interface IntegrationTests : XCTestCase
@property (nonatomic, strong) TaskAPIClient *client;
@property (nonatomic, strong) TaskServer *server;
@end

@implementation IntegrationTests

- (void)setUp {
    [super setUp];
    
    // Start server
    self.server = [[TaskServer alloc] init];
    [self.server startServerOnPort:8081];
    
    // Create client
    NSURL *baseURL = [NSURL URLWithString:@"http://localhost:8081"];
    self.client = [[TaskAPIClient alloc] initWithBaseURL:baseURL];
}

- (void)tearDown {
    [self.server stopServer];
    [super tearDown];
}

- (void)testCompleteWorkflow {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Complete CRUD workflow"];
    
    // Create task
    CreateTaskRequest *request = [[CreateTaskRequest alloc] init];
    request.title = @"Integration Test Task";
    request.priority = TaskPriorityHigh;
    
    [self.client createTaskWithRequest:request completion:^(Task *task, NSError *error) {
        XCTAssertNil(error);
        XCTAssertNotNil(task);
        XCTAssertEqualObjects(task.title, @"Integration Test Task");
        
        // Update status
        [self.client updateTaskStatus:task.taskId 
                              status:TaskStatusCompleted 
                          completion:^(Task *updated, NSError *error) {
            XCTAssertNil(error);
            XCTAssertEqual(updated.status, TaskStatusCompleted);
            
            // Delete task
            [self.client deleteTask:task.taskId completion:^(BOOL success, NSError *error) {
                XCTAssertNil(error);
                XCTAssertTrue(success);
                [expectation fulfill];
            }];
        }];
    }];
    
    [self waitForExpectationsWithTimeout:10.0 handler:nil];
}
@end
```

## Objective-C Specific Features

### Categories for Extension

```objc
// NSString+TaskExtensions.h
@interface NSString (TaskExtensions)
- (BOOL)isValidTaskStatus;
- (TaskStatus)toTaskStatus;
@end

// NSString+TaskExtensions.m
@implementation NSString (TaskExtensions)

- (BOOL)isValidTaskStatus {
    NSArray *validStatuses = @[@"pending", @"in_progress", @"completed", @"cancelled"];
    return [validStatuses containsObject:self.lowercaseString];
}

- (TaskStatus)toTaskStatus {
    return [Task stringToStatus:self];
}
@end
```

### Key-Value Coding (KVC) and Observing (KVO)

```objc
// Using KVC for dynamic property access
- (void)updateTaskWithDictionary:(NSDictionary *)updates {
    for (NSString *key in updates) {
        if ([self.task respondsToSelector:NSSelectorFromString(key)]) {
            [self.task setValue:updates[key] forKey:key];
        }
    }
}

// KVO for change notifications
- (void)observeTask:(Task *)task {
    [task addObserver:self 
           forKeyPath:@"status" 
              options:NSKeyValueObservingOptionNew | NSKeyValueObservingOptionOld 
              context:nil];
}

- (void)observeValueForKeyPath:(NSString *)keyPath 
                       ofObject:(id)object 
                         change:(NSDictionary *)change 
                        context:(void *)context {
    if ([keyPath isEqualToString:@"status"]) {
        TaskStatus oldStatus = [change[NSKeyValueChangeOldKey] integerValue];
        TaskStatus newStatus = [change[NSKeyValueChangeNewKey] integerValue];
        NSLog(@"Task status changed from %@ to %@", 
              [Task statusToString:oldStatus], 
              [Task statusToString:newStatus]);
    }
}
```

### Method Swizzling for Runtime Modification

```objc
// Adding logging to all API calls
@implementation TaskAPIClient (Logging)

+ (void)load {
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        [self swizzleMethod:@selector(createTaskWithRequest:completion:) 
                withMethod:@selector(logged_createTaskWithRequest:completion:)];
    });
}

+ (void)swizzleMethod:(SEL)originalSelector withMethod:(SEL)swizzledSelector {
    Method originalMethod = class_getInstanceMethod(self, originalSelector);
    Method swizzledMethod = class_getInstanceMethod(self, swizzledSelector);
    
    method_exchangeImplementations(originalMethod, swizzledMethod);
}

- (void)logged_createTaskWithRequest:(CreateTaskRequest *)request 
                          completion:(TaskCompletionHandler)completion {
    NSLog(@"Creating task: %@", request.title);
    
    // Call original implementation (now swapped)
    [self logged_createTaskWithRequest:request completion:^(Task *task, NSError *error) {
        if (task) {
            NSLog(@"Task created successfully: %@", task.taskId);
        } else {
            NSLog(@"Task creation failed: %@", error);
        }
        completion(task, error);
    }];
}
@end
```

## Memory Management and ARC

### Automatic Reference Counting

```objc
// Strong references (default)
@property (nonatomic, strong) Task *currentTask;

// Weak references to avoid retain cycles
@property (nonatomic, weak) id<TaskDelegate> delegate;

// Unsafe unretained (use with caution)
@property (nonatomic, unsafe_unretained) TaskServer *server;

// Block retain cycles
- (void)performAsyncOperation {
    __weak typeof(self) weakSelf = self;
    
    [self.apiClient fetchTasksWithCompletion:^(NSArray *tasks) {
        __strong typeof(weakSelf) strongSelf = weakSelf;
        if (strongSelf) {
            strongSelf.tasks = tasks;
            [strongSelf.tableView reloadData];
        }
    }];
}

// Autorelease pools for memory optimization
- (void)processLargeDataSet {
    for (int i = 0; i < 10000; i++) {
        @autoreleasepool {
            Task *task = [[Task alloc] initWithTitle:[NSString stringWithFormat:@"Task %d", i]];
            [self processTask:task];
            // task is released at end of autoreleasepool block
        }
    }
}
```

## Deployment Strategies

### macOS Application Bundle

```bash
# Create app bundle structure
mkdir -p TaskAPI.app/Contents/MacOS
mkdir -p TaskAPI.app/Contents/Resources

# Copy executable
cp task-rest-server TaskAPI.app/Contents/MacOS/

# Create Info.plist
cat > TaskAPI.app/Contents/Info.plist << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" 
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>task-rest-server</string>
    <key>CFBundleIdentifier</key>
    <string>com.example.taskapi</string>
    <key>CFBundleName</key>
    <string>Task API Server</string>
    <key>CFBundleVersion</key>
    <string>1.0.0</string>
</dict>
</plist>
EOF

# Sign the application (requires Developer ID)
codesign --deep --force --verify --verbose --sign "Developer ID" TaskAPI.app
```

### Docker Container

```dockerfile
# Dockerfile for Objective-C REST server
FROM ubuntu:22.04

# Install GNUstep and dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    gobjc \
    gnustep \
    gnustep-devel \
    libgnustep-base-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set up GNUstep environment
ENV GNUSTEP_MAKEFILES=/usr/share/GNUstep/Makefiles
RUN echo ". ${GNUSTEP_MAKEFILES}/GNUstep.sh" >> /etc/profile

# Copy source code
WORKDIR /app
COPY . .

# Build the application
RUN . ${GNUSTEP_MAKEFILES}/GNUstep.sh && make

# Expose port
EXPOSE 8080

# Run the server
CMD ["/app/task-rest-server"]
```

## Conclusion

This chapter demonstrated Objective-C's capabilities for building REST APIs and gRPC services. Key takeaways include:

### Language Strengths

**Dynamic Runtime**
- Message passing enables flexible method dispatch
- Runtime introspection for powerful metaprogramming
- Categories extend existing classes without subclassing
- Method swizzling for runtime behavior modification

**Memory Management**
- ARC eliminates most manual memory management
- Clear ownership semantics with strong/weak references
- Autorelease pools for memory optimization
- Predictable object lifecycle

**Foundation Framework**
- Rich collection classes with mutable variants
- Powerful string manipulation and formatting
- Built-in networking with NSURLSession
- Grand Central Dispatch for concurrency

### Practical Applications

**Apple Platform Development**
- Essential for iOS/macOS legacy codebases
- Interoperability with Swift code
- Access to platform-specific APIs
- Native performance on Apple hardware

**Systems Programming**
- Direct C compatibility for low-level operations
- Predictable performance characteristics
- Manual memory control when needed
- Integration with C libraries

### Development Experience

**Pros**
- Mature ecosystem with extensive documentation
- Excellent debugging tools in Xcode
- Dynamic features enable powerful patterns
- C compatibility for system programming
- Strong Apple platform integration

**Cons**
- Verbose syntax compared to modern languages
- Manual memory management in non-ARC code
- Limited cross-platform support
- Declining community as Swift gains adoption
- Steeper learning curve for modern developers

**Best Use Cases**
- Maintaining legacy iOS/macOS applications
- System-level Apple platform development
- Bridging C libraries to higher-level code
- Performance-critical Apple platform applications
- Educational purposes for understanding iOS/macOS foundations

Objective-C remains a powerful language for Apple platform development, offering unique dynamic features and deep system integration. While Swift has become Apple's primary language, understanding Objective-C is crucial for maintaining existing codebases and leveraging the full power of Apple's platforms. Its combination of C compatibility, dynamic runtime, and rich frameworks makes it a valuable tool in the modern developer's toolkit, particularly for those working with Apple technologies.