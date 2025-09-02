#import "TaskAPIClient.h"

@interface TaskAPIClient ()
@property (nonatomic, strong, readwrite) NSURL *baseURL;
@property (nonatomic, strong, readwrite) NSURLSession *session;
@end

@implementation TaskAPIClient

- (instancetype)initWithBaseURL:(NSURL *)baseURL {
    self = [super init];
    if (self) {
        _baseURL = baseURL;
        
        NSURLSessionConfiguration *config = [NSURLSessionConfiguration defaultSessionConfiguration];
        config.timeoutIntervalForRequest = 30.0;
        config.timeoutIntervalForResource = 60.0;
        _session = [NSURLSession sessionWithConfiguration:config];
    }
    return self;
}

#pragma mark - API Methods

- (void)listTasksWithStatus:(NSString *)status
                 assignedTo:(NSString *)assignedTo
                       tags:(NSArray<NSString *> *)tags
                   pageSize:(NSInteger)pageSize
                  pageToken:(NSString *)pageToken
                     sortBy:(NSString *)sortBy
                  sortOrder:(NSString *)sortOrder
                 completion:(TaskListCompletionHandler)completion {
    
    NSURLComponents *components = [NSURLComponents componentsWithURL:self.baseURL resolvingAgainstBaseURL:YES];
    components.path = @"/api/tasks";
    
    NSMutableArray<NSURLQueryItem *> *queryItems = [NSMutableArray array];
    
    if (status && status.length > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"status" value:status]];
    }
    if (assignedTo && assignedTo.length > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"assigned_to" value:assignedTo]];
    }
    if (tags && tags.count > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"tags" value:[tags componentsJoinedByString:@","]]];
    }
    if (pageSize > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"page_size" value:[@(pageSize) stringValue]]];
    }
    if (pageToken && pageToken.length > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"page_token" value:pageToken]];
    }
    if (sortBy && sortBy.length > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"sort_by" value:sortBy]];
    }
    if (sortOrder && sortOrder.length > 0) {
        [queryItems addObject:[NSURLQueryItem queryItemWithName:@"sort_order" value:sortOrder]];
    }
    
    if (queryItems.count > 0) {
        components.queryItems = queryItems;
    }
    
    NSURLRequest *request = [NSURLRequest requestWithURL:components.URL];
    
    [[self.session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, 0, nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 200) {
            NSError *jsonError;
            NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data options:0 error:&jsonError];
            
            if (jsonError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, 0, nil, jsonError);
                });
                return;
            }
            
            NSMutableArray<Task *> *tasks = [NSMutableArray array];
            for (NSDictionary *taskDict in responseDict[@"tasks"]) {
                Task *task = [[Task alloc] initWithDictionary:taskDict];
                [tasks addObject:task];
            }
            
            NSInteger totalCount = [responseDict[@"total_count"] integerValue];
            NSString *nextPageToken = responseDict[@"next_page_token"];
            if ([nextPageToken isKindOfClass:[NSNull class]]) {
                nextPageToken = nil;
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(tasks, totalCount, nextPageToken, nil);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, 0, nil, httpError);
            });
        }
    }] resume];
}

- (void)getTaskWithId:(NSString *)taskId completion:(TaskCompletionHandler)completion {
    NSURL *url = [self.baseURL URLByAppendingPathComponent:[NSString stringWithFormat:@"/api/tasks/%@", taskId]];
    NSURLRequest *request = [NSURLRequest requestWithURL:url];
    
    [[self.session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 200) {
            NSError *jsonError;
            NSDictionary *taskDict = [NSJSONSerialization JSONObjectWithData:data options:0 error:&jsonError];
            
            if (jsonError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, jsonError);
                });
                return;
            }
            
            Task *task = [[Task alloc] initWithDictionary:taskDict];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(task, nil);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIError" 
                                                         code:404 
                                                     userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, notFoundError);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)createTaskWithRequest:(CreateTaskRequest *)request completion:(TaskCompletionHandler)completion {
    NSURL *url = [self.baseURL URLByAppendingPathComponent:@"/api/tasks"];
    
    NSMutableDictionary *body = [NSMutableDictionary dictionary];
    body[@"title"] = request.title;
    
    if (request.taskDescription) {
        body[@"description"] = request.taskDescription;
    }
    body[@"priority"] = [Task priorityToString:request.priority];
    if (request.tags) {
        body[@"tags"] = request.tags;
    }
    if (request.assignedTo) {
        body[@"assigned_to"] = request.assignedTo;
    }
    
    NSError *jsonError;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:body options:0 error:&jsonError];
    
    if (jsonError) {
        completion(nil, jsonError);
        return;
    }
    
    NSMutableURLRequest *urlRequest = [NSMutableURLRequest requestWithURL:url];
    urlRequest.HTTPMethod = @"POST";
    urlRequest.HTTPBody = jsonData;
    [urlRequest setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    
    [[self.session dataTaskWithRequest:urlRequest completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 201) {
            NSError *parseError;
            NSDictionary *taskDict = [NSJSONSerialization JSONObjectWithData:data options:0 error:&parseError];
            
            if (parseError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, parseError);
                });
                return;
            }
            
            Task *task = [[Task alloc] initWithDictionary:taskDict];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(task, nil);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)updateTask:(NSString *)taskId withRequest:(UpdateTaskRequest *)request completion:(TaskCompletionHandler)completion {
    NSURL *url = [self.baseURL URLByAppendingPathComponent:[NSString stringWithFormat:@"/api/tasks/%@", taskId]];
    
    NSMutableDictionary *body = [NSMutableDictionary dictionary];
    
    if (request.title) {
        body[@"title"] = request.title;
    }
    if (request.taskDescription) {
        body[@"description"] = request.taskDescription;
    }
    if (request.status) {
        body[@"status"] = [Task statusToString:[request.status integerValue]];
    }
    if (request.priority) {
        body[@"priority"] = [Task priorityToString:[request.priority integerValue]];
    }
    if (request.tags) {
        body[@"tags"] = request.tags;
    }
    if (request.assignedTo) {
        body[@"assigned_to"] = request.assignedTo;
    }
    
    NSError *jsonError;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:body options:0 error:&jsonError];
    
    if (jsonError) {
        completion(nil, jsonError);
        return;
    }
    
    NSMutableURLRequest *urlRequest = [NSMutableURLRequest requestWithURL:url];
    urlRequest.HTTPMethod = @"PUT";
    urlRequest.HTTPBody = jsonData;
    [urlRequest setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    
    [[self.session dataTaskWithRequest:urlRequest completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 200) {
            NSError *parseError;
            NSDictionary *taskDict = [NSJSONSerialization JSONObjectWithData:data options:0 error:&parseError];
            
            if (parseError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, parseError);
                });
                return;
            }
            
            Task *task = [[Task alloc] initWithDictionary:taskDict];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(task, nil);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIError" 
                                                         code:404 
                                                     userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, notFoundError);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)updateTaskStatus:(NSString *)taskId status:(TaskStatus)status completion:(TaskCompletionHandler)completion {
    NSString *statusString = [Task statusToString:status];
    NSURL *url = [self.baseURL URLByAppendingPathComponent:[NSString stringWithFormat:@"/api/tasks/%@/status?status=%@", taskId, statusString]];
    
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];
    request.HTTPMethod = @"PATCH";
    
    [[self.session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 200) {
            NSError *parseError;
            NSDictionary *taskDict = [NSJSONSerialization JSONObjectWithData:data options:0 error:&parseError];
            
            if (parseError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, parseError);
                });
                return;
            }
            
            Task *task = [[Task alloc] initWithDictionary:taskDict];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(task, nil);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIError" 
                                                         code:404 
                                                     userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, notFoundError);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)deleteTask:(NSString *)taskId completion:(BooleanCompletionHandler)completion {
    NSURL *url = [self.baseURL URLByAppendingPathComponent:[NSString stringWithFormat:@"/api/tasks/%@", taskId]];
    
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];
    request.HTTPMethod = @"DELETE";
    
    [[self.session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(NO, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 204) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(YES, nil);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIError" 
                                                         code:404 
                                                     userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(NO, notFoundError);
            });
        } else {
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIError" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode]}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(NO, httpError);
            });
        }
    }] resume];
}

#pragma mark - Demo

- (void)runDemo {
    NSLog(@"\n==================================================");
    NSLog(@"Objective-C REST Client Demo");
    NSLog(@"==================================================\n");
    
    dispatch_group_t group = dispatch_group_create();
    __block Task *demoTask = nil;
    
    // 1. List existing tasks
    NSLog(@"1. Listing existing tasks...");
    dispatch_group_enter(group);
    [self listTasksWithStatus:nil assignedTo:nil tags:nil pageSize:20 pageToken:nil sortBy:nil sortOrder:nil completion:^(NSArray<Task *> *tasks, NSInteger totalCount, NSString *nextPageToken, NSError *error) {
        if (error) {
            NSLog(@"Error listing tasks: %@", error.localizedDescription);
        } else {
            NSLog(@"Found %ld tasks:", (long)totalCount);
            for (Task *task in tasks) {
                NSLog(@"  - [%@] %@ (ID: %@)", [Task statusToString:task.status], task.title, task.taskId);
            }
        }
        dispatch_group_leave(group);
    }];
    
    dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
    
    // 2. Create a new task
    NSLog(@"\n2. Creating a new task...");
    dispatch_group_enter(group);
    
    CreateTaskRequest *createRequest = [[CreateTaskRequest alloc] init];
    createRequest.title = @"Objective-C REST Client Demo Task";
    createRequest.taskDescription = @"Testing the Objective-C REST client";
    createRequest.priority = TaskPriorityHigh;
    createRequest.tags = @[@"demo", @"objective-c", @"rest"];
    createRequest.assignedTo = @"ios-team";
    
    [self createTaskWithRequest:createRequest completion:^(Task *task, NSError *error) {
        if (error) {
            NSLog(@"Error creating task: %@", error.localizedDescription);
        } else {
            NSLog(@"Created task successfully!");
            NSLog(@"  ID: %@", task.taskId);
            NSLog(@"  Title: %@", task.title);
            NSLog(@"  Status: %@", [Task statusToString:task.status]);
            demoTask = task;
        }
        dispatch_group_leave(group);
    }];
    
    dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
    
    if (!demoTask) {
        NSLog(@"Demo stopped due to task creation failure");
        return;
    }
    
    // 3. Get the created task
    NSLog(@"\n3. Retrieving task %@...", demoTask.taskId);
    dispatch_group_enter(group);
    [self getTaskWithId:demoTask.taskId completion:^(Task *task, NSError *error) {
        if (error) {
            NSLog(@"Error getting task: %@", error.localizedDescription);
        } else {
            NSLog(@"Retrieved task:");
            NSLog(@"  Title: %@", task.title);
            NSLog(@"  Description: %@", task.taskDescription ?: @"N/A");
            NSLog(@"  Priority: %@", [Task priorityToString:task.priority]);
        }
        dispatch_group_leave(group);
    }];
    
    dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
    
    // 4. Update task status
    NSLog(@"\n4. Updating task status to in_progress...");
    dispatch_group_enter(group);
    [self updateTaskStatus:demoTask.taskId status:TaskStatusInProgress completion:^(Task *task, NSError *error) {
        if (error) {
            NSLog(@"Error updating status: %@", error.localizedDescription);
        } else {
            NSLog(@"Status updated successfully!");
            NSLog(@"  New status: %@", [Task statusToString:task.status]);
        }
        dispatch_group_leave(group);
    }];
    
    dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
    
    // 5. Update task details
    NSLog(@"\n5. Updating task details...");
    dispatch_group_enter(group);
    
    UpdateTaskRequest *updateRequest = [[UpdateTaskRequest alloc] init];
    updateRequest.taskDescription = @"Updated description from Objective-C client";
    updateRequest.priority = @(TaskPriorityMedium);
    
    [self updateTask:demoTask.taskId withRequest:updateRequest completion:^(Task *task, NSError *error) {
        if (error) {
            NSLog(@"Error updating task: %@", error.localizedDescription);
        } else {
            NSLog(@"Task updated successfully!");
            NSLog(@"  Description: %@", task.taskDescription);
            NSLog(@"  Priority: %@", [Task priorityToString:task.priority]);
        }
        dispatch_group_leave(group);
    }];
    
    dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
    
    // 6. Delete the task
    NSLog(@"\n6. Deleting task %@...", demoTask.taskId);
    dispatch_group_enter(group);
    [self deleteTask:demoTask.taskId completion:^(BOOL success, NSError *error) {
        if (error) {
            NSLog(@"Error deleting task: %@", error.localizedDescription);
        } else if (success) {
            NSLog(@"Task deleted successfully!");
        }
        dispatch_group_leave(group);
    }];
    
    dispatch_group_wait(group, DISPATCH_TIME_FOREVER);
    
    NSLog(@"\n==================================================");
    NSLog(@"Demo completed!");
    NSLog(@"==================================================\n");
}

@end