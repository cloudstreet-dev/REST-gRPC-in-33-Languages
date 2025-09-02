#import "TaskServer.h"
#import "TaskRepository.h"
#import "Task.h"
#import <GCDWebServer/GCDWebServer.h>
#import <GCDWebServer/GCDWebServerDataRequest.h>
#import <GCDWebServer/GCDWebServerDataResponse.h>

@interface TaskServer ()
@property (nonatomic, strong) GCDWebServer *webServer;
@property (nonatomic, strong) TaskRepository *repository;
@end

@implementation TaskServer

- (instancetype)init {
    self = [super init];
    if (self) {
        _webServer = [[GCDWebServer alloc] init];
        _repository = [TaskRepository sharedRepository];
        [self setupRoutes];
    }
    return self;
}

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
    
    // PUT /api/tasks/:id - Update task
    [self.webServer addHandlerForMethod:@"PUT"
                            pathRegex:@"^/api/tasks/([^/]+)$"
                           requestClass:[GCDWebServerDataRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        NSArray *captures = [request attributeForKey:GCDWebServerRequestAttribute_RegexCaptures];
        NSString *taskId = captures[1];
        return [weakSelf handleUpdateTask:taskId request:(GCDWebServerDataRequest *)request];
    }];
    
    // PATCH /api/tasks/:id/status - Update task status
    [self.webServer addHandlerForMethod:@"PATCH"
                            pathRegex:@"^/api/tasks/([^/]+)/status$"
                           requestClass:[GCDWebServerRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        NSArray *captures = [request attributeForKey:GCDWebServerRequestAttribute_RegexCaptures];
        NSString *taskId = captures[1];
        NSString *status = request.query[@"status"];
        return [weakSelf handleUpdateTaskStatus:taskId status:status];
    }];
    
    // DELETE /api/tasks/:id - Delete task
    [self.webServer addHandlerForMethod:@"DELETE"
                            pathRegex:@"^/api/tasks/([^/]+)$"
                           requestClass:[GCDWebServerRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        NSArray *captures = [request attributeForKey:GCDWebServerRequestAttribute_RegexCaptures];
        NSString *taskId = captures[1];
        return [weakSelf handleDeleteTask:taskId];
    }];
    
    // GET /health - Health check
    [self.webServer addHandlerForMethod:@"GET"
                                   path:@"/health"
                           requestClass:[GCDWebServerRequest class]
                           processBlock:^GCDWebServerResponse *(GCDWebServerRequest *request) {
        return [weakSelf handleHealthCheck];
    }];
}

#pragma mark - Route Handlers

- (GCDWebServerResponse *)handleListTasks:(GCDWebServerRequest *)request {
    NSString *status = request.query[@"status"];
    NSString *assignedTo = request.query[@"assigned_to"];
    NSString *tagsString = request.query[@"tags"];
    NSArray<NSString *> *tags = tagsString ? [tagsString componentsSeparatedByString:@","] : nil;
    
    NSInteger pageSize = request.query[@"page_size"] ? [request.query[@"page_size"] integerValue] : 20;
    NSString *pageToken = request.query[@"page_token"];
    NSString *sortBy = request.query[@"sort_by"] ?: @"created_at";
    NSString *sortOrder = request.query[@"sort_order"] ?: @"desc";
    
    NSDictionary *result = [self.repository listTasksWithStatus:status
                                                    assignedTo:assignedTo
                                                          tags:tags
                                                      pageSize:pageSize
                                                     pageToken:pageToken
                                                        sortBy:sortBy
                                                     sortOrder:sortOrder];
    
    return [self jsonResponse:result statusCode:200];
}

- (GCDWebServerResponse *)handleGetTask:(NSString *)taskId {
    Task *task = [self.repository getTaskById:taskId];
    
    if (task) {
        return [self jsonResponse:[task toDictionary] statusCode:200];
    } else {
        return [self errorResponse:@"Task not found" statusCode:404];
    }
}

- (GCDWebServerResponse *)handleCreateTask:(GCDWebServerDataRequest *)request {
    NSError *error;
    NSDictionary *body = [NSJSONSerialization JSONObjectWithData:request.data
                                                        options:0
                                                          error:&error];
    
    if (error) {
        return [self errorResponse:@"Invalid JSON" statusCode:400];
    }
    
    if (!body[@"title"] || [body[@"title"] length] == 0) {
        return [self errorResponse:@"Title is required" statusCode:400];
    }
    
    CreateTaskRequest *createRequest = [[CreateTaskRequest alloc] initWithDictionary:body];
    Task *task = [createRequest toTask];
    Task *createdTask = [self.repository createTask:task];
    
    return [self jsonResponse:[createdTask toDictionary] statusCode:201];
}

- (GCDWebServerResponse *)handleUpdateTask:(NSString *)taskId request:(GCDWebServerDataRequest *)request {
    NSError *error;
    NSDictionary *body = [NSJSONSerialization JSONObjectWithData:request.data
                                                        options:0
                                                          error:&error];
    
    if (error) {
        return [self errorResponse:@"Invalid JSON" statusCode:400];
    }
    
    UpdateTaskRequest *updateRequest = [[UpdateTaskRequest alloc] initWithDictionary:body];
    Task *updatedTask = [self.repository updateTask:taskId withRequest:updateRequest];
    
    if (updatedTask) {
        return [self jsonResponse:[updatedTask toDictionary] statusCode:200];
    } else {
        return [self errorResponse:@"Task not found" statusCode:404];
    }
}

- (GCDWebServerResponse *)handleUpdateTaskStatus:(NSString *)taskId status:(NSString *)statusString {
    if (!statusString || statusString.length == 0) {
        return [self errorResponse:@"Status is required" statusCode:400];
    }
    
    TaskStatus status = [Task stringToStatus:statusString];
    Task *updatedTask = [self.repository updateTaskStatus:taskId status:status];
    
    if (updatedTask) {
        return [self jsonResponse:[updatedTask toDictionary] statusCode:200];
    } else {
        return [self errorResponse:@"Task not found" statusCode:404];
    }
}

- (GCDWebServerResponse *)handleDeleteTask:(NSString *)taskId {
    BOOL deleted = [self.repository deleteTask:taskId];
    
    if (deleted) {
        return [GCDWebServerResponse responseWithStatusCode:204];
    } else {
        return [self errorResponse:@"Task not found" statusCode:404];
    }
}

- (GCDWebServerResponse *)handleHealthCheck {
    NSDictionary *health = @{
        @"status": @"healthy",
        @"service": @"objective-c-task-api",
        @"task_count": @([self.repository taskCount])
    };
    
    return [self jsonResponse:health statusCode:200];
}

#pragma mark - Helper Methods

- (GCDWebServerDataResponse *)jsonResponse:(NSDictionary *)data statusCode:(NSInteger)statusCode {
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:data
                                                       options:NSJSONWritingPrettyPrinted
                                                         error:&error];
    
    if (error) {
        return [self errorResponse:@"Failed to serialize response" statusCode:500];
    }
    
    GCDWebServerDataResponse *response = [GCDWebServerDataResponse responseWithData:jsonData
                                                                        contentType:@"application/json"];
    response.statusCode = statusCode;
    
    // Add CORS headers
    [response setValue:@"*" forAdditionalHeader:@"Access-Control-Allow-Origin"];
    [response setValue:@"GET, POST, PUT, PATCH, DELETE, OPTIONS" forAdditionalHeader:@"Access-Control-Allow-Methods"];
    [response setValue:@"Content-Type" forAdditionalHeader:@"Access-Control-Allow-Headers"];
    
    return response;
}

- (GCDWebServerDataResponse *)errorResponse:(NSString *)message statusCode:(NSInteger)statusCode {
    NSDictionary *error = @{
        @"error": message,
        @"status_code": @(statusCode)
    };
    
    return [self jsonResponse:error statusCode:statusCode];
}

#pragma mark - Server Lifecycle

- (BOOL)startServerOnPort:(NSUInteger)port {
    NSMutableDictionary *options = [NSMutableDictionary dictionary];
    options[GCDWebServerOption_Port] = @(port);
    options[GCDWebServerOption_BindToLocalhost] = @NO;
    options[GCDWebServerOption_AutomaticallySuspendInBackground] = @NO;
    
    NSError *error;
    BOOL started = [self.webServer startWithOptions:options error:&error];
    
    if (started) {
        _port = port;
        NSLog(@"Objective-C Task REST Server started on port %lu", (unsigned long)port);
        NSLog(@"Visit http://localhost:%lu/api/tasks", (unsigned long)port);
    } else {
        NSLog(@"Failed to start server: %@", error);
    }
    
    return started;
}

- (void)stopServer {
    [self.webServer stop];
    NSLog(@"Server stopped");
}

- (BOOL)isRunning {
    return self.webServer.isRunning;
}

- (void)runUntilTerminated {
    NSLog(@"Server is running. Press Ctrl+C to stop.");
    
    // Set up signal handler for graceful shutdown
    dispatch_source_t signalSource = dispatch_source_create(DISPATCH_SOURCE_TYPE_SIGNAL, SIGINT, 0, dispatch_get_main_queue());
    dispatch_source_set_event_handler(signalSource, ^{
        NSLog(@"\nShutting down server...");
        [self stopServer];
        exit(0);
    });
    dispatch_resume(signalSource);
    signal(SIGINT, SIG_IGN);
    
    // Keep the run loop alive
    [[NSRunLoop currentRunLoop] run];
}

@end