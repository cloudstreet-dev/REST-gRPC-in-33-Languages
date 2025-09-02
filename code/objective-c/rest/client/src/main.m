#import <Foundation/Foundation.h>
#import "TaskAPIClient.h"

void printTask(NSDictionary *task) {
    NSLog(@"╔═══════════════════════════════════════════════════════╗");
    NSLog(@"║ Task ID: %-44s ║", [task[@"id"] UTF8String]);
    NSLog(@"║ Title: %-46s ║", [task[@"title"] UTF8String]);
    
    if (task[@"description"]) {
        NSString *desc = task[@"description"];
        if (desc.length > 44) {
            desc = [desc substringToIndex:44];
        }
        NSLog(@"║ Description: %-40s ║", [desc UTF8String]);
    }
    
    NSLog(@"║ Status: %-45s ║", [task[@"status"] UTF8String]);
    NSLog(@"║ Priority: %-43s ║", [task[@"priority"] UTF8String]);
    
    if (task[@"tags"] && [task[@"tags"] count] > 0) {
        NSString *tags = [task[@"tags"] componentsJoinedByString:@", "];
        if (tags.length > 42) {
            tags = [tags substringToIndex:42];
        }
        NSLog(@"║ Tags: %-47s ║", [tags UTF8String]);
    }
    
    if (task[@"assigned_to"]) {
        NSLog(@"║ Assigned To: %-40s ║", [task[@"assigned_to"] UTF8String]);
    }
    
    NSLog(@"║ Created: %-44s ║", [task[@"created_at"] UTF8String]);
    NSLog(@"║ Updated: %-44s ║", [task[@"updated_at"] UTF8String]);
    NSLog(@"╚═══════════════════════════════════════════════════════╝");
}

void printHelp() {
    NSLog(@"╔════════════════════════════════════════════════╗");
    NSLog(@"║     Objective-C Task Management CLI            ║");
    NSLog(@"╚════════════════════════════════════════════════╝");
    NSLog(@"");
    NSLog(@"Usage: client <command> [options]");
    NSLog(@"");
    NSLog(@"Commands:");
    NSLog(@"  list [--status <status>] [--assigned-to <user>] [--tags <tag1,tag2>]");
    NSLog(@"       List all tasks with optional filters");
    NSLog(@"");
    NSLog(@"  get <task-id>");
    NSLog(@"       Get a specific task by ID");
    NSLog(@"");
    NSLog(@"  create --title <title> [--description <desc>] [--priority <priority>]");
    NSLog(@"         [--tags <tag1,tag2>] [--assigned-to <user>]");
    NSLog(@"       Create a new task");
    NSLog(@"");
    NSLog(@"  update <task-id> [--title <title>] [--description <desc>]");
    NSLog(@"         [--status <status>] [--priority <priority>]");
    NSLog(@"         [--tags <tag1,tag2>] [--assigned-to <user>]");
    NSLog(@"       Update an existing task");
    NSLog(@"");
    NSLog(@"  update-status <task-id> <status>");
    NSLog(@"       Update the status of a task");
    NSLog(@"");
    NSLog(@"  delete <task-id>");
    NSLog(@"       Delete a task");
    NSLog(@"");
    NSLog(@"  demo");
    NSLog(@"       Run a demonstration of all operations");
    NSLog(@"");
    NSLog(@"  help");
    NSLog(@"       Show this help message");
    NSLog(@"");
    NSLog(@"Options:");
    NSLog(@"  --status      Task status (pending, in_progress, completed, cancelled)");
    NSLog(@"  --priority    Task priority (low, medium, high, urgent)");
    NSLog(@"  --tags        Comma-separated list of tags");
    NSLog(@"  --assigned-to User assignment");
    NSLog(@"");
    NSLog(@"Environment:");
    NSLog(@"  TASK_API_URL  Base URL for the Task API (default: http://localhost:8080)");
}

void runDemo(TaskAPIClient *client) {
    NSLog(@"╔════════════════════════════════════════════════╗");
    NSLog(@"║     Task Management API Demo                   ║");
    NSLog(@"╚════════════════════════════════════════════════╝");
    NSLog(@"");
    
    dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);
    __block NSString *createdTaskId = nil;
    
    // Step 1: List existing tasks
    NSLog(@"1. Listing all tasks...");
    [client listTasksWithStatus:nil
                     assignedTo:nil
                           tags:nil
                       pageSize:10
                      pageToken:nil
                         sortBy:nil
                      sortOrder:nil
                     completion:^(NSDictionary *response, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else {
            NSArray *tasks = response[@"tasks"];
            NSLog(@"   Found %lu tasks", (unsigned long)tasks.count);
            for (NSDictionary *task in tasks) {
                NSLog(@"   - %@: %@", task[@"id"], task[@"title"]);
            }
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    // Step 2: Create a new task
    NSLog(@"");
    NSLog(@"2. Creating a new task...");
    NSDictionary *newTask = @{
        @"title": @"Demo Task from Objective-C Client",
        @"description": @"This task was created using the Objective-C REST client",
        @"priority": @"high",
        @"tags": @[@"demo", @"objective-c", @"api-test"],
        @"assigned_to": @"demo-user"
    };
    
    [client createTask:newTask completion:^(NSDictionary *task, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else {
            NSLog(@"   Created task: %@", task[@"id"]);
            printTask(task);
            createdTaskId = task[@"id"];
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    if (!createdTaskId) {
        NSLog(@"Failed to create task. Exiting demo.");
        return;
    }
    
    // Step 3: Get the created task
    NSLog(@"");
    NSLog(@"3. Retrieving the created task...");
    [client getTask:createdTaskId completion:^(NSDictionary *task, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else {
            NSLog(@"   Retrieved task:");
            printTask(task);
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    // Step 4: Update the task
    NSLog(@"");
    NSLog(@"4. Updating the task...");
    NSDictionary *updates = @{
        @"title": @"Updated Demo Task",
        @"description": @"This task has been updated via the API",
        @"status": @"in_progress",
        @"priority": @"urgent"
    };
    
    [client updateTask:createdTaskId updates:updates completion:^(NSDictionary *task, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else {
            NSLog(@"   Updated task:");
            printTask(task);
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    // Step 5: Update task status
    NSLog(@"");
    NSLog(@"5. Updating task status to completed...");
    [client updateTaskStatus:createdTaskId status:@"completed" completion:^(NSDictionary *task, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else {
            NSLog(@"   Task status updated:");
            printTask(task);
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    // Step 6: List tasks with filters
    NSLog(@"");
    NSLog(@"6. Listing completed tasks...");
    [client listTasksWithStatus:@"completed"
                     assignedTo:nil
                           tags:nil
                       pageSize:10
                      pageToken:nil
                         sortBy:@"updated_at"
                      sortOrder:@"desc"
                     completion:^(NSDictionary *response, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else {
            NSArray *tasks = response[@"tasks"];
            NSLog(@"   Found %lu completed tasks", (unsigned long)tasks.count);
            for (NSDictionary *task in tasks) {
                NSLog(@"   - %@: %@ (Status: %@)", task[@"id"], task[@"title"], task[@"status"]);
            }
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    // Step 7: Delete the task
    NSLog(@"");
    NSLog(@"7. Deleting the demo task...");
    [client deleteTask:createdTaskId completion:^(BOOL success, NSError *error) {
        if (error) {
            NSLog(@"   Error: %@", error.localizedDescription);
        } else if (success) {
            NSLog(@"   Task deleted successfully");
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    // Step 8: Verify deletion
    NSLog(@"");
    NSLog(@"8. Verifying task deletion...");
    [client getTask:createdTaskId completion:^(NSDictionary *task, NSError *error) {
        if (error) {
            NSLog(@"   Task not found (as expected): %@", error.localizedDescription);
        } else {
            NSLog(@"   Warning: Task still exists!");
        }
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    
    NSLog(@"");
    NSLog(@"╔════════════════════════════════════════════════╗");
    NSLog(@"║     Demo Complete!                             ║");
    NSLog(@"╚════════════════════════════════════════════════╝");
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Get API URL from environment or use default
        NSString *apiUrl = [[NSProcessInfo processInfo] environment][@"TASK_API_URL"];
        if (!apiUrl) {
            apiUrl = @"http://localhost:8080";
        }
        
        TaskAPIClient *client = [[TaskAPIClient alloc] initWithBaseURL:apiUrl];
        
        if (argc < 2) {
            printHelp();
            return 1;
        }
        
        NSString *command = [NSString stringWithUTF8String:argv[1]];
        dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);
        
        if ([command isEqualToString:@"help"]) {
            printHelp();
            return 0;
        }
        
        if ([command isEqualToString:@"demo"]) {
            runDemo(client);
            return 0;
        }
        
        if ([command isEqualToString:@"list"]) {
            NSString *status = nil;
            NSString *assignedTo = nil;
            NSArray *tags = nil;
            
            // Parse command line arguments
            for (int i = 2; i < argc; i++) {
                NSString *arg = [NSString stringWithUTF8String:argv[i]];
                
                if ([arg isEqualToString:@"--status"] && i + 1 < argc) {
                    status = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--assigned-to"] && i + 1 < argc) {
                    assignedTo = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--tags"] && i + 1 < argc) {
                    NSString *tagsStr = [NSString stringWithUTF8String:argv[++i]];
                    tags = [tagsStr componentsSeparatedByString:@","];
                }
            }
            
            [client listTasksWithStatus:status
                             assignedTo:assignedTo
                                   tags:tags
                               pageSize:20
                              pageToken:nil
                                 sortBy:@"created_at"
                              sortOrder:@"desc"
                             completion:^(NSDictionary *response, NSError *error) {
                if (error) {
                    NSLog(@"Error: %@", error.localizedDescription);
                } else {
                    NSArray *tasks = response[@"tasks"];
                    NSLog(@"Found %lu tasks", (unsigned long)tasks.count);
                    for (NSDictionary *task in tasks) {
                        printTask(task);
                        NSLog(@"");
                    }
                }
                dispatch_semaphore_signal(semaphore);
            }];
            
            dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
            return 0;
        }
        
        if ([command isEqualToString:@"get"] && argc >= 3) {
            NSString *taskId = [NSString stringWithUTF8String:argv[2]];
            
            [client getTask:taskId completion:^(NSDictionary *task, NSError *error) {
                if (error) {
                    NSLog(@"Error: %@", error.localizedDescription);
                } else {
                    printTask(task);
                }
                dispatch_semaphore_signal(semaphore);
            }];
            
            dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
            return 0;
        }
        
        if ([command isEqualToString:@"create"]) {
            NSMutableDictionary *task = [NSMutableDictionary dictionary];
            
            // Parse command line arguments
            for (int i = 2; i < argc; i++) {
                NSString *arg = [NSString stringWithUTF8String:argv[i]];
                
                if ([arg isEqualToString:@"--title"] && i + 1 < argc) {
                    task[@"title"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--description"] && i + 1 < argc) {
                    task[@"description"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--priority"] && i + 1 < argc) {
                    task[@"priority"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--tags"] && i + 1 < argc) {
                    NSString *tagsStr = [NSString stringWithUTF8String:argv[++i]];
                    task[@"tags"] = [tagsStr componentsSeparatedByString:@","];
                } else if ([arg isEqualToString:@"--assigned-to"] && i + 1 < argc) {
                    task[@"assigned_to"] = [NSString stringWithUTF8String:argv[++i]];
                }
            }
            
            if (!task[@"title"]) {
                NSLog(@"Error: --title is required for creating a task");
                return 1;
            }
            
            [client createTask:task completion:^(NSDictionary *createdTask, NSError *error) {
                if (error) {
                    NSLog(@"Error: %@", error.localizedDescription);
                } else {
                    NSLog(@"Task created successfully:");
                    printTask(createdTask);
                }
                dispatch_semaphore_signal(semaphore);
            }];
            
            dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
            return 0;
        }
        
        if ([command isEqualToString:@"update"] && argc >= 3) {
            NSString *taskId = [NSString stringWithUTF8String:argv[2]];
            NSMutableDictionary *updates = [NSMutableDictionary dictionary];
            
            // Parse command line arguments
            for (int i = 3; i < argc; i++) {
                NSString *arg = [NSString stringWithUTF8String:argv[i]];
                
                if ([arg isEqualToString:@"--title"] && i + 1 < argc) {
                    updates[@"title"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--description"] && i + 1 < argc) {
                    updates[@"description"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--status"] && i + 1 < argc) {
                    updates[@"status"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--priority"] && i + 1 < argc) {
                    updates[@"priority"] = [NSString stringWithUTF8String:argv[++i]];
                } else if ([arg isEqualToString:@"--tags"] && i + 1 < argc) {
                    NSString *tagsStr = [NSString stringWithUTF8String:argv[++i]];
                    updates[@"tags"] = [tagsStr componentsSeparatedByString:@","];
                } else if ([arg isEqualToString:@"--assigned-to"] && i + 1 < argc) {
                    updates[@"assigned_to"] = [NSString stringWithUTF8String:argv[++i]];
                }
            }
            
            [client updateTask:taskId updates:updates completion:^(NSDictionary *task, NSError *error) {
                if (error) {
                    NSLog(@"Error: %@", error.localizedDescription);
                } else {
                    NSLog(@"Task updated successfully:");
                    printTask(task);
                }
                dispatch_semaphore_signal(semaphore);
            }];
            
            dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
            return 0;
        }
        
        if ([command isEqualToString:@"update-status"] && argc >= 4) {
            NSString *taskId = [NSString stringWithUTF8String:argv[2]];
            NSString *status = [NSString stringWithUTF8String:argv[3]];
            
            [client updateTaskStatus:taskId status:status completion:^(NSDictionary *task, NSError *error) {
                if (error) {
                    NSLog(@"Error: %@", error.localizedDescription);
                } else {
                    NSLog(@"Task status updated successfully:");
                    printTask(task);
                }
                dispatch_semaphore_signal(semaphore);
            }];
            
            dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
            return 0;
        }
        
        if ([command isEqualToString:@"delete"] && argc >= 3) {
            NSString *taskId = [NSString stringWithUTF8String:argv[2]];
            
            [client deleteTask:taskId completion:^(BOOL success, NSError *error) {
                if (error) {
                    NSLog(@"Error: %@", error.localizedDescription);
                } else if (success) {
                    NSLog(@"Task %@ deleted successfully", taskId);
                } else {
                    NSLog(@"Failed to delete task %@", taskId);
                }
                dispatch_semaphore_signal(semaphore);
            }];
            
            dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
            return 0;
        }
        
        NSLog(@"Unknown command: %@", command);
        printHelp();
        return 1;
    }
    return 0;
}