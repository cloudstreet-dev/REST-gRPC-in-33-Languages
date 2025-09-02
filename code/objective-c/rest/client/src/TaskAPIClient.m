#import "TaskAPIClient.h"

@interface TaskAPIClient ()
@property (nonatomic, strong, readwrite) NSString *baseURL;
@property (nonatomic, strong, readwrite) NSURLSession *session;
@end

@implementation TaskAPIClient

- (instancetype)initWithBaseURL:(NSString *)baseURL {
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
                 completion:(DictionaryCompletionHandler)completion {
    
    NSURLComponents *components = [NSURLComponents componentsWithString:[NSString stringWithFormat:@"%@/api/tasks", self.baseURL]];
    
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
                completion(nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 200) {
            NSError *jsonError;
            NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:data options:0 error:&jsonError];
            
            if (jsonError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, jsonError);
                });
                return;
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(responseDict, nil);
            });
        } else {
            NSString *errorMessage = [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode];
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: errorMessage}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)getTask:(NSString *)taskId completion:(DictionaryCompletionHandler)completion {
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@/api/tasks/%@", self.baseURL, taskId]];
    
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
            NSDictionary *task = [NSJSONSerialization JSONObjectWithData:data options:0 error:&jsonError];
            
            if (jsonError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, jsonError);
                });
                return;
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(task, nil);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                          code:404 
                                                      userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, notFoundError);
            });
        } else {
            NSString *errorMessage = [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode];
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: errorMessage}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)createTask:(NSDictionary *)task completion:(DictionaryCompletionHandler)completion {
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@/api/tasks", self.baseURL]];
    
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];
    request.HTTPMethod = @"POST";
    [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    
    NSError *jsonError;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:task options:0 error:&jsonError];
    
    if (jsonError) {
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(nil, jsonError);
        });
        return;
    }
    
    request.HTTPBody = jsonData;
    
    [[self.session dataTaskWithRequest:request completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
        if (error) {
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, error);
            });
            return;
        }
        
        NSHTTPURLResponse *httpResponse = (NSHTTPURLResponse *)response;
        
        if (httpResponse.statusCode == 201) {
            NSError *parseError;
            NSDictionary *createdTask = [NSJSONSerialization JSONObjectWithData:data options:0 error:&parseError];
            
            if (parseError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, parseError);
                });
                return;
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(createdTask, nil);
            });
        } else if (httpResponse.statusCode == 400) {
            NSError *badRequestError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                           code:400 
                                                       userInfo:@{NSLocalizedDescriptionKey: @"Bad request - title is required"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, badRequestError);
            });
        } else {
            NSString *errorMessage = [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode];
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: errorMessage}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)updateTask:(NSString *)taskId updates:(NSDictionary *)updates completion:(DictionaryCompletionHandler)completion {
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@/api/tasks/%@", self.baseURL, taskId]];
    
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];
    request.HTTPMethod = @"PUT";
    [request setValue:@"application/json" forHTTPHeaderField:@"Content-Type"];
    
    NSError *jsonError;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:updates options:0 error:&jsonError];
    
    if (jsonError) {
        dispatch_async(dispatch_get_main_queue(), ^{
            completion(nil, jsonError);
        });
        return;
    }
    
    request.HTTPBody = jsonData;
    
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
            NSDictionary *updatedTask = [NSJSONSerialization JSONObjectWithData:data options:0 error:&parseError];
            
            if (parseError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, parseError);
                });
                return;
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(updatedTask, nil);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                          code:404 
                                                      userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, notFoundError);
            });
        } else {
            NSString *errorMessage = [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode];
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: errorMessage}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)updateTaskStatus:(NSString *)taskId status:(NSString *)status completion:(DictionaryCompletionHandler)completion {
    NSURLComponents *components = [NSURLComponents componentsWithString:[NSString stringWithFormat:@"%@/api/tasks/%@/status", self.baseURL, taskId]];
    
    NSURLQueryItem *statusQuery = [NSURLQueryItem queryItemWithName:@"status" value:status];
    components.queryItems = @[statusQuery];
    
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:components.URL];
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
            NSDictionary *updatedTask = [NSJSONSerialization JSONObjectWithData:data options:0 error:&parseError];
            
            if (parseError) {
                dispatch_async(dispatch_get_main_queue(), ^{
                    completion(nil, parseError);
                });
                return;
            }
            
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(updatedTask, nil);
            });
        } else if (httpResponse.statusCode == 400) {
            NSError *badRequestError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                           code:400 
                                                       userInfo:@{NSLocalizedDescriptionKey: @"Invalid status value"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, badRequestError);
            });
        } else if (httpResponse.statusCode == 404) {
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                          code:404 
                                                      userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, notFoundError);
            });
        } else {
            NSString *errorMessage = [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode];
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: errorMessage}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(nil, httpError);
            });
        }
    }] resume];
}

- (void)deleteTask:(NSString *)taskId completion:(BooleanCompletionHandler)completion {
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@/api/tasks/%@", self.baseURL, taskId]];
    
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
            NSError *notFoundError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                          code:404 
                                                      userInfo:@{NSLocalizedDescriptionKey: @"Task not found"}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(NO, notFoundError);
            });
        } else {
            NSString *errorMessage = [NSString stringWithFormat:@"HTTP Error %ld", (long)httpResponse.statusCode];
            NSError *httpError = [NSError errorWithDomain:@"TaskAPIClient" 
                                                     code:httpResponse.statusCode 
                                                 userInfo:@{NSLocalizedDescriptionKey: errorMessage}];
            dispatch_async(dispatch_get_main_queue(), ^{
                completion(NO, httpError);
            });
        }
    }] resume];
}

@end