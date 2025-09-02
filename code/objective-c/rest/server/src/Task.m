#import "Task.h"

@implementation Task

- (instancetype)initWithTitle:(NSString *)title {
    self = [super init];
    if (self) {
        _taskId = [[NSUUID UUID] UUIDString];
        _title = title;
        _status = TaskStatusPending;
        _priority = TaskPriorityMedium;
        _tags = @[];
        _createdAt = [NSDate date];
        _updatedAt = [NSDate date];
    }
    return self;
}

- (instancetype)initWithDictionary:(NSDictionary *)dictionary {
    self = [super init];
    if (self) {
        _taskId = dictionary[@"id"] ?: [[NSUUID UUID] UUIDString];
        _title = dictionary[@"title"];
        _taskDescription = dictionary[@"description"];
        
        if (dictionary[@"status"]) {
            _status = [Task stringToStatus:dictionary[@"status"]];
        } else {
            _status = TaskStatusPending;
        }
        
        if (dictionary[@"priority"]) {
            _priority = [Task stringToPriority:dictionary[@"priority"]];
        } else {
            _priority = TaskPriorityMedium;
        }
        
        _tags = dictionary[@"tags"] ?: @[];
        _assignedTo = dictionary[@"assigned_to"];
        
        // Handle dates
        NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
        [formatter setDateFormat:@"yyyy-MM-dd'T'HH:mm:ss'Z'"];
        [formatter setTimeZone:[NSTimeZone timeZoneWithName:@"UTC"]];
        
        if (dictionary[@"created_at"]) {
            _createdAt = [formatter dateFromString:dictionary[@"created_at"]];
        } else {
            _createdAt = [NSDate date];
        }
        
        if (dictionary[@"updated_at"]) {
            _updatedAt = [formatter dateFromString:dictionary[@"updated_at"]];
        } else {
            _updatedAt = [NSDate date];
        }
    }
    return self;
}

- (id)copyWithZone:(NSZone *)zone {
    Task *copy = [[Task allocWithZone:zone] init];
    copy.taskId = [self.taskId copy];
    copy.title = [self.title copy];
    copy.taskDescription = [self.taskDescription copy];
    copy.status = self.status;
    copy.priority = self.priority;
    copy.tags = [self.tags copy];
    copy.assignedTo = [self.assignedTo copy];
    copy.createdAt = [self.createdAt copy];
    copy.updatedAt = [self.updatedAt copy];
    return copy;
}

- (NSDictionary *)toDictionary {
    NSMutableDictionary *dict = [NSMutableDictionary dictionary];
    
    dict[@"id"] = self.taskId;
    dict[@"title"] = self.title;
    
    if (self.taskDescription) {
        dict[@"description"] = self.taskDescription;
    }
    
    dict[@"status"] = [Task statusToString:self.status];
    dict[@"priority"] = [Task priorityToString:self.priority];
    dict[@"tags"] = self.tags;
    
    if (self.assignedTo) {
        dict[@"assigned_to"] = self.assignedTo;
    }
    
    NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
    [formatter setDateFormat:@"yyyy-MM-dd'T'HH:mm:ss'Z'"];
    [formatter setTimeZone:[NSTimeZone timeZoneWithName:@"UTC"]];
    
    dict[@"created_at"] = [formatter stringFromDate:self.createdAt];
    dict[@"updated_at"] = [formatter stringFromDate:self.updatedAt];
    
    return [dict copy];
}

- (NSString *)toJSONString {
    NSError *error;
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:[self toDictionary]
                                                       options:NSJSONWritingPrettyPrinted
                                                         error:&error];
    if (error) {
        NSLog(@"Error converting task to JSON: %@", error);
        return nil;
    }
    
    return [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
}

+ (NSString *)statusToString:(TaskStatus)status {
    switch (status) {
        case TaskStatusPending:
            return @"pending";
        case TaskStatusInProgress:
            return @"in_progress";
        case TaskStatusCompleted:
            return @"completed";
        case TaskStatusCancelled:
            return @"cancelled";
        default:
            return @"pending";
    }
}

+ (TaskStatus)stringToStatus:(NSString *)statusString {
    NSString *lowercaseStatus = [statusString lowercaseString];
    
    if ([lowercaseStatus isEqualToString:@"pending"]) {
        return TaskStatusPending;
    } else if ([lowercaseStatus isEqualToString:@"in_progress"] || 
               [lowercaseStatus isEqualToString:@"inprogress"]) {
        return TaskStatusInProgress;
    } else if ([lowercaseStatus isEqualToString:@"completed"]) {
        return TaskStatusCompleted;
    } else if ([lowercaseStatus isEqualToString:@"cancelled"]) {
        return TaskStatusCancelled;
    }
    
    return TaskStatusPending;
}

+ (NSString *)priorityToString:(TaskPriority)priority {
    switch (priority) {
        case TaskPriorityLow:
            return @"low";
        case TaskPriorityMedium:
            return @"medium";
        case TaskPriorityHigh:
            return @"high";
        case TaskPriorityUrgent:
            return @"urgent";
        default:
            return @"medium";
    }
}

+ (TaskPriority)stringToPriority:(NSString *)priorityString {
    NSString *lowercasePriority = [priorityString lowercaseString];
    
    if ([lowercasePriority isEqualToString:@"low"]) {
        return TaskPriorityLow;
    } else if ([lowercasePriority isEqualToString:@"medium"]) {
        return TaskPriorityMedium;
    } else if ([lowercasePriority isEqualToString:@"high"]) {
        return TaskPriorityHigh;
    } else if ([lowercasePriority isEqualToString:@"urgent"]) {
        return TaskPriorityUrgent;
    }
    
    return TaskPriorityMedium;
}

+ (Task *)taskFromJSONString:(NSString *)jsonString {
    NSError *error;
    NSData *jsonData = [jsonString dataUsingEncoding:NSUTF8StringEncoding];
    NSDictionary *dict = [NSJSONSerialization JSONObjectWithData:jsonData
                                                        options:0
                                                          error:&error];
    if (error) {
        NSLog(@"Error parsing JSON: %@", error);
        return nil;
    }
    
    return [[Task alloc] initWithDictionary:dict];
}

@end

@implementation CreateTaskRequest

- (instancetype)initWithDictionary:(NSDictionary *)dictionary {
    self = [super init];
    if (self) {
        _title = dictionary[@"title"];
        _taskDescription = dictionary[@"description"];
        
        if (dictionary[@"priority"]) {
            _priority = [Task stringToPriority:dictionary[@"priority"]];
        } else {
            _priority = TaskPriorityMedium;
        }
        
        _tags = dictionary[@"tags"];
        _assignedTo = dictionary[@"assigned_to"];
    }
    return self;
}

- (Task *)toTask {
    Task *task = [[Task alloc] initWithTitle:self.title];
    task.taskDescription = self.taskDescription;
    task.priority = self.priority;
    task.tags = self.tags ?: @[];
    task.assignedTo = self.assignedTo;
    return task;
}

@end

@implementation UpdateTaskRequest

- (instancetype)initWithDictionary:(NSDictionary *)dictionary {
    self = [super init];
    if (self) {
        _title = dictionary[@"title"];
        _taskDescription = dictionary[@"description"];
        
        if (dictionary[@"status"]) {
            _status = @([Task stringToStatus:dictionary[@"status"]]);
        }
        
        if (dictionary[@"priority"]) {
            _priority = @([Task stringToPriority:dictionary[@"priority"]]);
        }
        
        _tags = dictionary[@"tags"];
        _assignedTo = dictionary[@"assigned_to"];
    }
    return self;
}

@end